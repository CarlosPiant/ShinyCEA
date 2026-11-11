# R code export for the app ("R code for this tab").
#
# cea_analysis_code() turns the state of the app (model, parameter table,
# settings and the inputs of each tab) into a commented R script that
# reproduces the analysis outside the app. It is a pure function of a plain
# list, with no Shiny dependency, so it can be unit-tested: the tests run the
# generated code and compare it with direct calls.
#
# A script has three parts: (1) setup - model, parameters, settings; (2) the
# runs the analysis depends on - deterministic and/or PSA; (3) the analysis of
# the tab. Each tab has a section builder returning list(needs, lines), where
# `needs` says which runs must come first ("det", "psa").

# Tabs of the app in display order, as reported by input$main_tabs.
.code_tabs <- c("Model Info", "Study", "Parameters", "Health", "Deterministic", "Threshold",
                "PSA", "PSA Parameters", "CEAC", "VOI", "Survival", "Traces",
                "Scenarios", "Weighted", "BIA", "Subgroups", "Equity", "Validation",
                "settings")

# ---- Writing R values as code ------------------------------------------------

# A number as it should appear in a script: plain notation (100000, not 1e+05),
# falling back to 17 significant digits when 15 do not round-trip exactly.
.code_num <- function(x) {
  if (is.na(x)) return("NA")
  if (!is.finite(x)) return(if (x > 0) "Inf" else "-Inf")
  s <- format(x, digits = 15, scientific = FALSE, trim = TRUE)
  if (!isTRUE(as.numeric(s) == x)) s <- sprintf("%.17g", x)
  s
}

# A scalar or vector as code. Strings are quoted with deparse() so that quotes
# and backslashes inside them stay valid R.
.code_val <- function(x) {
  if (is.null(x)) return("NULL")
  one <- function(v) {
    if (is.character(v)) return(if (is.na(v)) "NA_character_" else deparse(v))
    if (is.logical(v)) return(if (is.na(v)) "NA" else if (v) "TRUE" else "FALSE")
    .code_num(as.numeric(v))
  }
  vals <- vapply(seq_along(x), function(i) one(x[[i]]), character(1))
  if (!is.null(names(x))) vals <- paste0(.code_name(names(x)), " = ", vals)
  if (length(x) == 1 && is.null(names(x))) vals else paste0("c(", paste(vals, collapse = ", "), ")")
}

# An argument or list name: bare when syntactic, quoted otherwise ("High risk").
.code_name <- function(nm) {
  ifelse(make.names(nm) == nm, nm, vapply(nm, deparse, character(1)))
}

# A named list of override lists or weight vectors (scenarios, subgroups,
# composites) as a multi-line list(...) literal assigned to `var`.
.code_named_list <- function(var, x, inner = c("list", "c")) {
  inner <- match.arg(inner)
  items <- vapply(names(x), function(nm) {
    el <- x[[nm]]
    body <- if (length(el) == 0) {
      paste0(inner, "()")
    } else {
      vals <- vapply(seq_along(el), function(i) .code_val(unname(el[[i]])), character(1))
      paste0(inner, "(", paste0(.code_name(names(el)), " = ", vals, collapse = ", "), ")")
    }
    paste0("  ", .code_name(nm), " = ", body)
  }, character(1))
  c(paste0(var, " <- list("), paste0(items, c(rep(",", length(items) - 1L), "")), ")")
}

# ---- Setup -------------------------------------------------------------------

# Lines that rebuild the parameter table of the app from the model defaults.
# Edited cells become one assignment each. When the table has other rows than
# the defaults (a CSV upload), cell-by-cell edits cannot describe it, so the
# script reads the CSV the app can download.
.code_params <- function(default_df, current_df) {
  base_line <- "params <- sanitize_params(model$default_params())$df"
  if (is.null(default_df) || is.null(current_df)) return(base_line)
  if (!identical(as.character(default_df$name), as.character(current_df$name))) {
    return(c(
      "# The parameter table in the app has different rows than the model defaults",
      "# (e.g. after a CSV upload). Save it with 'Download current parameters CSV'",
      "# on the Parameters tab and read that file here.",
      "params <- sanitize_params(read.csv(\"parameters_current.csv\", stringsAsFactors = FALSE))$df"
    ))
  }
  edits <- character(0)
  for (col in intersect(names(default_df), names(current_df))) {
    if (col == "name") next
    old <- default_df[[col]]; new <- current_df[[col]]
    # A cell changed when exactly one side is NA or both are set and differ
    changed <- which(xor(is.na(old), is.na(new)) |
                     (!is.na(old) & !is.na(new) & as.character(old) != as.character(new)))
    for (i in changed) {
      edits <- c(edits, paste0("params$", col, "[params$name == ",
                               deparse(as.character(current_df$name[i])), "] <- ",
                               .code_val(new[i])))
    }
  }
  if (!length(edits)) return(base_line)
  c(base_line, "# Values edited in the app (they differ from the model defaults)", edits)
}

# The study details as a cea_study(...) call, one filled field per line.
.code_study <- function(study) {
  study <- .as_study(study)
  fields <- intersect(c(.study_fields$field, "comparators"), names(study))
  if (!length(fields)) return("study <- cea_study()   # no study details entered yet")
  args <- vapply(fields, function(f) {
    v <- study[[f]]
    # Vectors (assumptions, comparators) get one element per line
    val <- if (length(v) > 1 || !is.null(names(v))) {
      el <- vapply(seq_along(v), function(i) deparse(unname(v[[i]])), character(1))
      if (!is.null(names(v))) el <- paste0(.code_name(names(v)), " = ", el)
      paste0("c(\n    ", paste(el, collapse = ",\n    "), ")")
    } else {
      deparse(v)
    }
    paste0("  ", f, " = ", val)
  }, character(1))
  # Each argument may span several lines: split so every element is one line
  unlist(strsplit(c("study <- cea_study(",
                    paste0(args, c(rep(",", length(args) - 1L), "")), ")"), "\n", fixed = TRUE))
}

# The palette of the Settings tab as code.
.code_palette <- function(state) {
  choice <- if (is.null(state$palette_choice)) "cream" else state$palette_choice
  if (identical(choice, "custom") && !is.null(state$palette_custom)) {
    pc <- state$palette_custom
    return(c(
      "pal <- cea_palette_custom(",
      paste0("  strategies = ", .code_val(pc$strategies), ","),
      paste0("  frontier = ", .code_val(pc$frontier), ", dominated = ", .code_val(pc$dominated),
             ", ext_dominated = ", .code_val(pc$ext_dominated)),
      ")"
    ))
  }
  paste0("pal <- cea_palette(", deparse(choice), ")")
}

.code_setup <- function(state) {
  model_lines <- if (is.null(state$model_code) || is.na(state$model_code) ||
                     !nzchar(state$model_code)) {
    c("# TODO: the app could not tell how this model was created. Replace `my_model`",
      "# with your cea_model object (built with new_cea_model()).",
      "model <- my_model")
  } else {
    paste0("model <- ", state$model_code)
  }
  pinned <- identical(state$ref_mode, "pinned") && !is.null(state$ref_pinned)
  c("library(ShinyCEA)",
    "",
    "# ---- Setup: model, parameters and settings ----",
    model_lines,
    "",
    "# Parameter table: `base` feeds the deterministic run, the distribution",
    "# columns feed the PSA, lower/upper feed the sensitivity analyses",
    .code_params(state$default_params, state$params),
    "",
    paste0("wtp <- ", .code_num(state$wtp), "   # willingness to pay per QALY"),
    if (pinned) {
      paste0("ref_strategy <- ", deparse(state$ref_pinned), "   # reference pinned in the app")
    } else {
      "ref_strategy <- NULL   # NULL = reference chosen automatically (highest NMB)"
    },
    .code_palette(state))
}

.code_det_run <- function() {
  c("# ---- Deterministic run (base-case values) ----",
    "det <- run_deterministic(model, params, wtp = wtp, ref_strategy = ref_strategy)")
}

.code_psa_run <- function(state) {
  # The app keeps the number of iterations within 10 to 10,000
  n <- suppressWarnings(as.integer(state$psa_n))
  if (length(n) != 1 || is.na(n) || n < 10) n <- 100L
  n <- max(10L, min(n, 10000L))
  seed <- if (is.null(state$psa_seed) || !is.finite(state$psa_seed)) 20250605 else state$psa_seed
  c("# ---- Probabilistic sensitivity analysis ----",
    "# Same seed as the app, so the draws (and every PSA-based result) match it",
    paste0("set.seed(", .code_num(seed), ")"),
    paste0("psa <- run_psa(model, params, n_sim = ", n, ")"))
}

# ---- Tab sections --------------------------------------------------------------

# Target strategy line: the app's choice when known, otherwise the first
# non-reference strategy (what the app preselects).
.code_target <- function(var, value) {
  if (is.null(value) || !nzchar(value)) {
    paste0(var, " <- setdiff(det$strategies, det$ref)[1]   # first non-reference strategy")
  } else {
    paste0(var, " <- ", deparse(value), "   # compared with the reference, det$ref")
  }
}

.code_sec_health <- function(state) {
  list(needs = character(0), lines = c(
    "# ---- Health: checks of the parameter table and model outputs ----",
    "health <- validate_params_and_model(model, params, wtp = wtp)",
    "table(health$status)   # number of PASS / WARN / FAIL checks",
    "health"))
}

.code_sec_deterministic <- function(state) {
  metric <- if (is.null(state$tornado_metric)) "INB" else state$tornado_metric
  lines <- c(
    "# ---- Deterministic results ----",
    "det$ref     # reference strategy",
    "det$table   # costs, QALYs, NMB and increments vs the reference",
    "",
    "# Dominance and extended dominance, with the ICERs along the frontier",
    "merge(det$frontier, det$icers, by = \"Strategy\", all.x = TRUE)",
    "plot_frontier(det$frontier_status, palette = pal)",
    "",
    paste0("# Tornado diagram: one-way sensitivity of the ", metric, " to each parameter,"),
    "# moved between its lower and upper bound",
    .code_target("target", state$tornado_target),
    "tor_params <- params[params$include_tornado %in% TRUE &",
    "                     (is.finite(params$lower) | is.finite(params$upper)), ]",
    "tor <- build_tornado_data(tor_params, det$pars, model, det$ref, target,",
    paste0("                          metric = ", deparse(metric), ", wtp = wtp)"),
    "plot_tornado(tor, base_val = unique(tor$base)[1],",
    paste0("             metric_label = paste0(\"", metric, " (\", target, \" vs \", det$ref, \")\"),"),
    "             title = paste0(\"Tornado: \", target, \" vs \", det$ref), palette = pal)")
  tw <- state$tw
  if (!is.null(tw$param1) && !is.null(tw$param2) && !identical(tw$param1, tw$param2)) {
    n_grid <- if (is.null(tw$n) || !is.finite(tw$n)) 20L else as.integer(tw$n)
    lines <- c(lines, "",
      "# Two-way sensitivity analysis: both parameters over their lower-upper range",
      paste0("p1 <- ", deparse(tw$param1)),
      paste0("p2 <- ", deparse(tw$param2)),
      "tw <- twoway_dsa(",
      "  model, det$pars, param1 = p1, param2 = p2,",
      "  range1 = c(params$lower[params$name == p1], params$upper[params$name == p1]),",
      "  range2 = c(params$lower[params$name == p2], params$upper[params$name == p2]),",
      paste0("  n_grid = ", n_grid, "L, ref = det$ref, target = ",
             if (is.null(tw$target)) "target" else deparse(tw$target), ","),
      paste0("  wtp = wtp, metric = ", deparse(if (is.null(tw$metric)) "INB" else tw$metric)),
      ")",
      "plot_twoway_dsa(tw, palette = pal)")
  }
  list(needs = "det", lines = lines)
}

.code_sec_threshold <- function(state) {
  thr <- state$thr
  if (is.null(thr$param) || is.null(thr$min) || is.null(thr$max)) {
    return(list(needs = character(0), lines = c(
      "# ---- Threshold analysis ----",
      "# No threshold search is set up in the app yet: run the deterministic analysis,",
      "# choose a parameter on the Threshold tab, then export the code again.")))
  }
  mode <- if (is.null(thr$mode)) "icer" else thr$mode
  n <- if (is.null(thr$grid_n) || !is.finite(thr$grid_n)) 30 else thr$grid_n
  list(needs = "det", lines = c(
    "# ---- Threshold analysis ----",
    paste0("# Value of one parameter at which ",
           if (identical(mode, "icer")) "the ICER equals the WTP" else "the incremental net benefit is zero"),
    .code_target("thr_target", thr$strategy),
    paste0("thr_param <- ", deparse(thr$param)),
    "thr <- threshold_analysis(",
    "  model, det$pars, thr_param, det$ref, thr_target,",
    paste0("  range_min = ", .code_num(thr$min), ", range_max = ", .code_num(thr$max),
           ", n_points = ", .code_num(n), ","),
    paste0("  wtp = wtp, mode = ", deparse(mode)),
    ")",
    "thr$threshold   # NA when the search range contains no crossing",
    paste0("plot_threshold(thr$grid, thr$threshold, thr_param, wtp, mode = ", deparse(mode),
           ", palette = pal)")))
}

.code_sec_psa <- function(state) {
  list(needs = c("det", "psa"), lines = c(
    "# ---- PSA results ----",
    "psa$dropped   # iterations dropped because the model returned non-finite values",
    "",
    "# Cost-effectiveness plane: increments of each strategy vs the reference",
    "inc <- do.call(rbind, lapply(setdiff(det$strategies, det$ref), function(s) {",
    "  data.frame(",
    "    strategy = s,",
    "    inc_qaly = psa$results[[paste0(s, \".q\")]] - psa$results[[paste0(det$ref, \".q\")]],",
    "    inc_cost = psa$results[[paste0(s, \".cost\")]] - psa$results[[paste0(det$ref, \".cost\")]]",
    "  )",
    "}))",
    "plot_ce_plane(inc, wtp, palette = pal)",
    "",
    "# Mean and 95% interval of costs, QALYs and NMB per strategy",
    "build_psa_summary(psa$results, det$strategies, wtp = wtp)"))
}

.code_sec_psa_params <- function(state) {
  sel <- state$psa_param_select
  bins <- if (is.null(state$psa_param_bins)) 40 else state$psa_param_bins
  list(needs = "psa", lines = c(
    "# ---- PSA parameter draws ----",
    "# Distributions actually sampled, to check them against the intended ones",
    if (is.null(sel) || !length(sel)) {
      "sel <- names(psa$draws)[1]   # parameters to show"
    } else {
      paste0("sel <- ", .code_val(sel), "   # parameters to show")
    },
    "draws_long <- do.call(rbind, lapply(sel, function(nm) {",
    "  v <- psa$draws[[nm]]",
    "  data.frame(parameter = nm, value = v[is.finite(v)])",
    "}))",
    "ggplot2::ggplot(draws_long, ggplot2::aes(x = value)) +",
    paste0("  ggplot2::geom_histogram(bins = ", .code_num(bins), ", alpha = 0.8) +"),
    "  ggplot2::facet_wrap(~ parameter, scales = \"free\", ncol = min(length(sel), 3)) +",
    "  ggplot2::labs(x = \"Draw value\", y = \"Frequency\", title = \"PSA parameter draws\") +",
    "  theme_cea()",
    "",
    "# Mean, SD and 95% interval of each selected parameter",
    "t(sapply(psa$draws[sel], function(v) {",
    "  c(Mean = mean(v, na.rm = TRUE), SD = sd(v, na.rm = TRUE),",
    "    quantile(v, c(0.025, 0.975), na.rm = TRUE))",
    "}))"))
}

.code_sec_ceac <- function(state) {
  show_ceaf <- isTRUE(state$ceac_show_ceaf)
  list(needs = c("det", "psa"), lines = c(
    "# ---- CEAC, CEAF and expected loss curves ----",
    "wtp_max <- max(wtp, 200000)   # the curves cover at least 0 to 200,000 per QALY",
    "",
    "# CEAC: probability that each strategy has the highest net benefit",
    "ceac <- compute_ceac(psa$results, det$strategies, wtp_max = wtp_max)",
    "# CEAF: the strategy with the highest EXPECTED net benefit at each WTP",
    "ceaf <- compute_ceaf(psa$results, det$strategies, wtp_max = wtp_max)",
    if (show_ceaf) {
      "plot_ceac(ceac, palette = pal, ceaf = ceaf)   # frontier superimposed, as in the app"
    } else {
      "plot_ceac(ceac, palette = pal)   # add `ceaf = ceaf` to superimpose the frontier"
    },
    "",
    "# Expected loss curves: average net benefit forgone by choosing each strategy.",
    "# The lowest curve is the optimal strategy and its height is the EVPI.",
    "el <- compute_expected_loss(psa$results, det$strategies, wtp_max = wtp_max, wtp_step = 5000)",
    "plot_expected_loss(el, palette = pal, show_optimal = TRUE)"))
}

.code_sec_voi <- function(state) {
  list(needs = c("det", "psa"), lines = c(
    "# ---- Value of information ----",
    "wtp_max <- max(wtp, 200000)",
    "",
    "# EVPI: value of removing all parameter uncertainty, by WTP",
    "evpi <- compute_evpi(psa$results, det$strategies, wtp_max = wtp_max)",
    "plot_evpi(evpi, palette = pal)",
    "",
    "# Expected loss of each strategy; the minimum across strategies is the EVPI",
    "el <- compute_expected_loss(psa$results, det$strategies, wtp_max = wtp_max)",
    "plot_expected_loss(el, palette = pal)",
    "",
    "# EVPPI: value of removing the uncertainty of one parameter at a time, at `wtp`",
    "evppi <- compute_evppi(psa$results, psa$draws, det$strategies, wtp = wtp)",
    "evppi",
    "plot_evppi(evppi, palette = pal)"))
}

.code_sec_survival <- function(state) {
  list(needs = "det", lines = c(
    "# ---- Survival curves (base case) ----",
    "surv <- cea_survival_data(det, params)   # NULL if the model returns no survival or trace",
    if (is.null(state$surv_curve)) {
      "curve <- sort(unique(surv$curve))[1]"
    } else {
      paste0("curve <- ", deparse(state$surv_curve))
    },
    paste0("plot_survival(surv, curve_name = curve, facet = ",
           .code_val(isTRUE(state$surv_facets)), ", palette = pal)")))
}

.code_sec_traces <- function(state) {
  one_state <- identical(state$trace_scope, "One state") && !is.null(state$trace_state)
  # The app facets by strategy unless the box was unticked
  facet <- is.null(state$trace_facet) || isTRUE(state$trace_facet)
  list(needs = "det", lines = c(
    "# ---- Cohort traces (state occupancy over time, base case) ----",
    "trace <- cea_trace_data(det, params)   # NULL if the model returns no trace or survival",
    paste0("plot_trace(trace, state_filter = ",
           if (one_state) deparse(state$trace_state) else "NULL",
           ", facet = ", .code_val(facet), ", palette = pal)")))
}

.code_sec_scenarios <- function(state) {
  sc <- state$scenarios
  if (!length(sc)) {
    return(list(needs = character(0), lines = c(
      "# ---- Scenario analysis ----",
      "# No scenarios are defined in the app. A scenario is a named list of parameter",
      "# overrides, e.g.:",
      "# scenarios <- list(\"Base case\" = list(), \"No discounting\" = list(d_cost = 0, d_qaly = 0))",
      "# sc <- run_scenarios(model, params, scenarios, wtp = wtp, ref_strategy = ref_strategy)")))
  }
  metric <- if (is.null(state$forest_metric)) "ICER" else state$forest_metric
  list(needs = character(0), lines = c(
    "# ---- Scenario analysis ----",
    "# Each scenario overrides the base value of the listed parameters",
    .code_named_list("scenarios", sc, "list"),
    "sc <- run_scenarios(model, params, scenarios, wtp = wtp, ref_strategy = ref_strategy)",
    "sc",
    "summarize_scenarios(sc)   # optimal strategy in each scenario",
    "",
    paste0("# Forest plot of one strategy's ", metric, " across the scenarios"),
    if (is.null(state$forest_target)) {
      "forest_target <- unique(sc$Strategy)[1]"
    } else {
      paste0("forest_target <- ", deparse(state$forest_target))
    },
    paste0("plot_scenario_forest(sc, target = forest_target, metric = ", deparse(metric),
           ", wtp = wtp, palette = pal)")))
}

.code_sec_weighted <- function(state) {
  wc <- state$composites
  if (!length(wc)) {
    return(list(needs = character(0), lines = c(
      "# ---- Weighted comparator analysis ----",
      "# No composites are defined in the app. A composite is a named vector of",
      "# strategy weights that sum to 1, e.g.:",
      "# composites <- list(\"Current care\" = c(std = 0.6, t1 = 0.4), \"New drug\" = c(t2 = 1))",
      "# wc <- run_weighted_analysis(model, params, composites, wtp = wtp)")))
  }
  metric <- if (is.null(state$wc_forest_metric)) "INB" else state$wc_forest_metric
  list(needs = character(0), lines = c(
    "# ---- Weighted comparator analysis ----",
    "# Each composite mixes strategies with the given weights (e.g. market shares)",
    .code_named_list("composites", wc, "c"),
    "wc <- run_weighted_analysis(model, params, composites, wtp = wtp)",
    "wc$composites   # cost, QALYs and NMB of each composite",
    "wc$pairwise     # composites compared with each other",
    "plot_weighted_comparison(wc$details, metric = \"Cost\", palette = pal)",
    "plot_weighted_comparison(wc$details, metric = \"QALY\", palette = pal)",
    paste0("if (nrow(wc$pairwise) > 0) plot_weighted_pairwise(wc$pairwise, metric = ",
           deparse(metric), ", wtp = wtp, palette = pal)")))
}

.code_sec_bia <- function(state) {
  bia <- state$bia
  strats <- state$strategies
  n <- length(strats)
  # Shares the BIA tab starts with: equal current shares, and everyone on the
  # last strategy in the final year
  cur <- bia$cur_mix
  if (is.null(cur)) cur <- stats::setNames(rep(round(1 / n, 2), n), strats)
  new <- bia$new_mix
  if (is.null(new)) new <- stats::setNames(as.numeric(seq_len(n) == n), strats)
  years <- if (is.null(bia$years) || !is.finite(bia$years)) 5 else bia$years
  pop <- if (is.null(bia$pop) || !is.finite(bia$pop)) 100000 else bia$pop
  growth <- if (is.null(bia$growth) || !is.finite(bia$growth)) 0 else bia$growth
  linear <- is.null(bia$linear) || isTRUE(bia$linear)
  list(needs = character(0), lines = c(
    "# ---- Budget impact analysis ----",
    "# Market shares by strategy: today, and in the final projection year",
    paste0("current_mix <- ", .code_val(cur)),
    paste0("new_final   <- ", .code_val(new)),
    paste0("years <- ", .code_num(years)),
    if (linear) {
      c("# Shares move linearly from the current mix to the final mix",
        "new_mix <- linear_uptake(current_mix, new_final, years = years)")
    } else {
      "new_mix <- new_final   # the new mix applies from the first year"
    },
    paste0("bia <- run_bia(model, params, population = ", .code_num(pop), ","),
    "               current_mix = current_mix, new_mix = new_mix, years = years,",
    paste0("               annual_growth = ", .code_num(growth / 100),
           ")   # population growth as a proportion"),
    "bia$impact         # cost under each mix and budget impact, by year",
    "bia$total_impact   # total over the projection",
    "plot_bia(bia$impact, palette = pal)",
    "plot_bia_waterfall(bia$impact, palette = pal)"))
}

.code_sec_subgroups <- function(state) {
  sg <- state$subgroups
  if (!length(sg)) {
    return(list(needs = character(0), lines = c(
      "# ---- Subgroup analysis ----",
      "# No subgroups are defined in the app. A subgroup is a named list of parameter",
      "# overrides, e.g.:",
      "# subgroups <- list(\"Overall\" = list(), \"High risk\" = list(p_HS_std = 0.08))",
      "# sg <- run_subgroups(model, params, subgroups, target = \"t1\", ref = \"std\", wtp = wtp)")))
  }
  strats <- state$strategies
  ref <- if (is.null(state$sg_ref)) strats[1] else state$sg_ref
  target <- if (is.null(state$sg_target)) strats[min(2, length(strats))] else state$sg_target
  metric <- if (is.null(state$sg_forest_metric)) "ICER" else state$sg_forest_metric
  list(needs = character(0), lines = c(
    "# ---- Subgroup analysis ----",
    "# Each subgroup overrides the base value of the listed parameters",
    .code_named_list("subgroups", sg, "list"),
    paste0("sg <- run_subgroups(model, params, subgroups, target = ", deparse(target),
           ", ref = ", deparse(ref), ", wtp = wtp)"),
    "sg",
    paste0("plot_subgroup_forest(sg, metric = ", deparse(metric), ", wtp = wtp, palette = pal)")))
}

.code_sec_equity <- function(state) {
  if (length(state$model_subgroups) < 2) {
    return(list(needs = character(0), lines = c(
      "# ---- Distributional cost-effectiveness analysis ----",
      "# The equity analysis needs a model with at least two predefined subgroups:",
      "# new_cea_model(..., subgroups = list(\"Low risk\" = list(...), \"High risk\" = list(...)))")))
  }
  eps <- if (is.null(state$equity_epsilon) || !is.finite(state$equity_epsilon)) 1 else state$equity_epsilon
  list(needs = character(0), lines = c(
    "# ---- Distributional cost-effectiveness analysis ----",
    "# The model's predefined subgroups, with the assumptions the app makes: equal",
    "# population shares, and baseline health of 0.80 QALYs for the first subgroup,",
    "# 0.10 lower for each following one. Edit `share` and `baseline_qaly` as needed.",
    "sg <- model$subgroups",
    "dcea_subgroups <- lapply(seq_along(sg), function(i) {",
    "  list(overrides = if (is.list(sg[[i]])) sg[[i]] else list(),",
    "       share = 1 / length(sg),",
    "       baseline_qaly = 0.80 - (i - 1) * 0.10)",
    "})",
    "names(dcea_subgroups) <- names(sg)",
    "dcea <- run_distributional_cea(model, params, dcea_subgroups, wtp = wtp)",
    "",
    "plot_nmb_by_subgroup(dcea, \"NMB\", palette = pal)",
    if (is.null(state$equity_lorenz_strat)) {
      "lorenz_strategy <- dcea$strategies[1]"
    } else {
      paste0("lorenz_strategy <- ", deparse(state$equity_lorenz_strat))
    },
    "plot_health_distribution(dcea, strategy = lorenz_strategy, palette = pal)",
    "# Concentration index of each strategy (how unequally health is distributed)",
    "lapply(dcea$strategies, function(s) calculate_concentration_index(dcea, strategy = s))",
    "# Net benefit weighted for inequality aversion (Atkinson)",
    paste0("calculate_equity_weighted_nmb(dcea, method = \"atkinson\", epsilon = ", .code_num(eps), ")"),
    "equity_frontier(dcea, palette = pal)"))
}

.code_sec_validation <- function(state) {
  list(needs = character(0), lines = c(
    "# ---- Model validation report ----",
    "val <- generate_validation_report(model, params, wtp = wtp)",
    "val$score     # 0-100",
    "val$summary   # number of PASS / WARN / FAIL checks",
    "val$details",
    "# To save the report: generate_validation_report(model, params, wtp = wtp,",
    "#                                                output_file = \"validation_report.md\")"))
}

.code_sec_study <- function(state) {
  list(needs = character(0), lines = c(
    "# ---- Study details and CHEERS 2022 checklist ----",
    "# The context of the analysis: who, where, whose perspective, which costs and",
    "# outcomes, which assumptions. Pass it to new_cea_model(study = ) to keep it",
    "# with the model.",
    .code_study(state$study),
    "",
    "# Which checklist items are available. `analyses` lists what was run, any of",
    "# \"deterministic\", \"psa\", \"scenarios\", \"subgroups\", \"equity\".",
    "checklist <- cheers_checklist(model, study = study, params = params,",
    paste0("                              analyses = ",
           if (length(state$analyses)) .code_val(state$analyses) else "character(0)", ")"),
    "table(checklist$Status)",
    "checklist[checklist$Status %in% c(\"Partly\", \"Not reported\"), c(\"Item\", \"Topic\", \"Where\")]"))
}

# Tabs without an analysis of their own: show what the tab displays.
.code_sec_setup_only <- function(tab) {
  lines <- switch(tab,
    "Model Info" = c("# ---- Model information ----",
                     "model$info()   # key, name and strategies",
                     "params[, c(\"name\", \"distribution\", \"mean\", \"lower\", \"upper\", \"base\")]"),
    "Parameters" = c("# ---- Parameter table ----", "params"),
    c("# ---- Figure palette ----",
      "# Pass `palette = pal` to any plot_*() function of ShinyCEA", "pal$strategies"))
  list(needs = character(0), lines = lines)
}

.code_section <- function(tab, state) {
  switch(tab,
    "Study" = .code_sec_study(state),
    "Health" = .code_sec_health(state),
    "Deterministic" = .code_sec_deterministic(state),
    "Threshold" = .code_sec_threshold(state),
    "PSA" = .code_sec_psa(state),
    "PSA Parameters" = .code_sec_psa_params(state),
    "CEAC" = .code_sec_ceac(state),
    "VOI" = .code_sec_voi(state),
    "Survival" = .code_sec_survival(state),
    "Traces" = .code_sec_traces(state),
    "Scenarios" = .code_sec_scenarios(state),
    "Weighted" = .code_sec_weighted(state),
    "BIA" = .code_sec_bia(state),
    "Subgroups" = .code_sec_subgroups(state),
    "Equity" = .code_sec_equity(state),
    "Validation" = .code_sec_validation(state),
    .code_sec_setup_only(tab))
}

# ---- Entry point ---------------------------------------------------------------

#' R Code Reproducing an Analysis of the App
#'
#' Builds a commented R script that reproduces, outside the app, the analysis
#' of one tab (or of all tabs) with the model, parameter values and settings
#' currently selected in the app.
#'
#' @param tab Name of the tab as reported by `input$main_tabs` (e.g. `"CEAC"`),
#'   or `"full"` for one script covering every tab.
#' @param state A list describing the app state. Used fields: `model_code`
#'   (expression that creates the model, as a string), `model_name`,
#'   `strategies`, `default_params`, `params`, `wtp`, `ref_mode`, `ref_pinned`,
#'   `psa_n`, `psa_seed`, `palette_choice`, `palette_custom`, and the inputs
#'   and stores of the tabs. Missing fields fall back to the app defaults.
#' @return A character vector, one element per line of code.
#' @keywords internal
cea_analysis_code <- function(tab, state) {
  full <- identical(tab, "full")
  tabs <- if (full) setdiff(.code_tabs, c("Model Info", "Parameters", "settings")) else tab
  sections <- lapply(tabs, .code_section, state = state)
  needs <- unique(unlist(lapply(sections, `[[`, "needs")))

  title <- if (full) "all analyses" else if (identical(tab, "settings")) "Settings" else tab
  header <- c(
    paste0("# ShinyCEA analysis script: ", title),
    if (!is.null(state$model_name)) paste0("# Model: ", state$model_name),
    paste0("# Exported from the ShinyCEA app on ", format(Sys.Date()), "."),
    "# Reproduces the app's analysis with the parameter values and settings",
    "# selected there. Inputs: the model object and the values below. Outputs:",
    "# tables and plots printed to the console / plot window.",
    if ("psa" %in% needs) "# The PSA uses the seed set in the app, so results match it.",
    "")

  body <- .code_setup(state)
  if ("det" %in% needs) body <- c(body, "", .code_det_run())
  if ("psa" %in% needs) body <- c(body, "", .code_psa_run(state))
  for (s in sections) body <- c(body, "", s$lines)
  c(header, body)
}
