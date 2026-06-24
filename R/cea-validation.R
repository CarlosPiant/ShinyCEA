#' Check Transition Matrix Properties
#'
#' Validates that a Markov trace or transition matrix satisfies standard
#' properties: rows sum to 1, no negative entries, absorbing states stay
#' absorbing, and death is monotone non-decreasing.
#'
#' @param trace A matrix (rows = cycles, columns = states) from a model's
#'   simulate() output, OR a square transition matrix.
#' @param tol Tolerance for row-sum checks. Default 1e-4.
#' @return A data.frame with columns: category, check, status, details.
#' @export
check_transition_matrix <- function(trace, tol = 1e-4) {
  results <- list()
  add <- function(check, status, details = "") {
    results[[length(results) + 1L]] <<- data.frame(
      category = "Transition matrix", check = check,
      status = status, details = details, stringsAsFactors = FALSE
    )
  }

  if (is.null(trace) || !is.matrix(trace)) {
    add("Trace available", "WARN", "No trace matrix provided (partitioned survival?).")
    return(do.call(rbind, results))
  }
  add("Trace available", "PASS", paste0(nrow(trace), " x ", ncol(trace)))

  # Non-negative entries
  neg <- sum(trace < -tol, na.rm = TRUE)
  if (neg > 0) {
    add("Non-negative entries", "FAIL", paste0(neg, " negative entries found."))
  } else {
    add("Non-negative entries", "PASS", "")
  }

  # Row sums ~ 1
  rs <- rowSums(trace, na.rm = TRUE)
  max_dev <- max(abs(rs - 1), na.rm = TRUE)
  bad_rows <- sum(abs(rs - 1) > tol, na.rm = TRUE)
  if (bad_rows > 0) {
    add("Rows sum to 1", "WARN",
        paste0(bad_rows, " row(s) deviate from 1; max deviation: ",
               round(max_dev, 6)))
  } else {
    add("Rows sum to 1", "PASS",
        paste0("Max deviation: ", round(max_dev, 8)))
  }

  # Death/absorbing state monotonicity
  cn <- colnames(trace)
  if (!is.null(cn)) {
    death_cols <- grep("^d$|death|dead", tolower(cn))
    if (length(death_cols) > 0) {
      for (dc in death_cols) {
        dv <- trace[, dc]
        non_mono <- sum(diff(dv) < -tol, na.rm = TRUE)
        col_name <- cn[dc]
        if (non_mono > 0) {
          add(paste0("'", col_name, "' monotone non-decreasing"), "WARN",
              paste0(non_mono, " decreasing step(s)."))
        } else {
          add(paste0("'", col_name, "' monotone non-decreasing"), "PASS", "")
        }
      }
    }
  }

  # First row: cohort starts somewhere
  if (abs(sum(trace[1, ]) - 1) > tol) {
    add("Initial distribution sums to 1", "FAIL",
        paste0("Sum = ", round(sum(trace[1, ]), 6)))
  } else {
    add("Initial distribution sums to 1", "PASS", "")
  }

  do.call(rbind, results)
}

#' Check Probability Parameters
#'
#' Validates that probability-type parameters are in the range 0 to 1 and that groups
#' of mutually exclusive probabilities (e.g., transitions from one state)
#' sum to <= 1.
#'
#' @param param_df Parameter data.frame with 'name', 'base', and 'distribution' columns.
#' @param prob_groups Optional named list of character vectors. Each element
#'   names a group of parameters whose base values should sum to <= 1.
#'   Example: \code{list("from_healthy" = c("p_HS", "p_HD"))}.
#'   If NULL, auto-detects groups from common prefixes.
#' @return A data.frame with columns: category, check, status, details.
#' @export
check_probabilities_sum <- function(param_df, prob_groups = NULL) {
  results <- list()
  add <- function(check, status, details = "") {
    results[[length(results) + 1L]] <<- data.frame(
      category = "Probabilities", check = check,
      status = status, details = details, stringsAsFactors = FALSE
    )
  }

  # Identify probability params
  is_prob <- grepl("^p_|^pr_|_prob$|probability", tolower(param_df$name)) |
    tolower(param_df$distribution) == "beta"

  prob_params <- param_df[is_prob, ]
  if (nrow(prob_params) == 0) {
    add("Probability params found", "WARN", "No probability parameters detected.")
    return(do.call(rbind, results))
  }
  add("Probability params found", "PASS",
      paste0(nrow(prob_params), " probability-type parameters."))

  # Range check [0, 1]
  for (i in seq_len(nrow(prob_params))) {
    nm <- prob_params$name[i]
    val <- prob_params$base[i]
    if (!is.finite(val)) next
    if (val < 0 || val > 1) {
      add(paste0(nm, " in [0,1]"), "FAIL",
          paste0("Base = ", round(val, 6), " outside [0,1]."))
    }
  }
  n_out <- sum(prob_params$base < 0 | prob_params$base > 1, na.rm = TRUE)
  if (n_out == 0) add("All probabilities in [0,1]", "PASS", "")

  # Group sum checks
  if (is.null(prob_groups)) {
    # Auto-detect: group by prefix before last underscore, but only for
    # parameters whose name starts with p_ or pr_ (not utilities, costs, etc.)
    true_probs <- prob_params[grepl("^p_|^pr_", prob_params$name), ]
    if (nrow(true_probs) > 0) {
      prefixes <- sub("_[^_]+$", "", true_probs$name)
      tab <- table(prefixes)
      prob_groups <- list()
      for (pfx in names(tab[tab >= 2])) {
        members <- true_probs$name[sub("_[^_]+$", "", true_probs$name) == pfx]
        prob_groups[[pfx]] <- members
      }
    } else {
      prob_groups <- list()
    }
  }

  for (grp_name in names(prob_groups)) {
    members <- prob_groups[[grp_name]]
    vals <- param_df$base[param_df$name %in% members]
    vals <- vals[is.finite(vals)]
    if (length(vals) == 0) next
    s <- sum(vals)
    if (s > 1 + 1e-6) {
      add(paste0("Group '", grp_name, "' sum <= 1"), "FAIL",
          paste0("Sum = ", round(s, 4), " (members: ",
                 paste(members, collapse = ", "), ")."))
    } else {
      add(paste0("Group '", grp_name, "' sum <= 1"), "PASS",
          paste0("Sum = ", round(s, 4)))
    }
  }

  do.call(rbind, results)
}

#' Check Discounting Consistency
#'
#' Verifies that discounting is correctly implemented by comparing results
#' with and without discounting, and checking that discount rate parameters
#' are in a reasonable range.
#'
#' @param model A `cea_model` object.
#' @param params_df Parameter data.frame.
#' @param wtp WTP threshold.
#' @return A data.frame with columns: category, check, status, details.
#' @export
check_discounting <- function(model, params_df, wtp = 100000) {
  results <- list()
  add <- function(check, status, details = "") {
    results[[length(results) + 1L]] <<- data.frame(
      category = "Discounting", check = check,
      status = status, details = details, stringsAsFactors = FALSE
    )
  }

  # Check discount parameters exist
  dc_names <- c("d_cost", "d_qaly", "discount_rate")
  found <- intersect(dc_names, params_df$name)
  if (length(found) == 0) {
    add("Discount parameters present", "WARN",
        "No standard discount parameters (d_cost, d_qaly) found.")
    return(do.call(rbind, results))
  }
  add("Discount parameters present", "PASS", paste(found, collapse = ", "))

  # Check range
  for (nm in found) {
    val <- params_df$base[params_df$name == nm]
    if (is.finite(val) && (val < 0 || val > 0.10)) {
      add(paste0(nm, " in reasonable range"), "WARN",
          paste0("Value = ", val, "; typical range is 0-0.05."))
    } else {
      add(paste0(nm, " in reasonable range"), "PASS",
          paste0("Value = ", val))
    }
  }

  # Compare discounted vs undiscounted
  strat1 <- model$info()$strategies[1]
  pars_disc <- stats::setNames(as.list(params_df$base), params_df$name)
  pars_nodisc <- pars_disc
  for (nm in found) pars_nodisc[[nm]] <- 0

  res_disc <- safe_simulate(model, pars_disc, strat1)
  res_nodisc <- safe_simulate(model, pars_nodisc, strat1)

  if (is.null(res_disc) || is.null(res_nodisc)) {
    add("Discount comparison", "WARN", "Could not simulate for comparison.")
    return(do.call(rbind, results))
  }

  if (res_nodisc$cost >= res_disc$cost - 1e-6) {
    add("Undiscounted cost >= discounted", "PASS",
        paste0("$", round(res_nodisc$cost), " vs $", round(res_disc$cost)))
  } else {
    add("Undiscounted cost >= discounted", "FAIL",
        paste0("$", round(res_nodisc$cost), " < $", round(res_disc$cost)))
  }

  if (res_nodisc$qaly >= res_disc$qaly - 1e-6) {
    add("Undiscounted QALY >= discounted", "PASS",
        paste0(round(res_nodisc$qaly, 4), " vs ", round(res_disc$qaly, 4)))
  } else {
    add("Undiscounted QALY >= discounted", "FAIL",
        paste0(round(res_nodisc$qaly, 4), " < ", round(res_disc$qaly, 4)))
  }

  do.call(rbind, results)
}

#' Check PSA Input Validity
#'
#' Validates that PSA-eligible parameters have appropriate distributions,
#' domain-consistent bounds, and reasonable uncertainty ranges.
#'
#' @param param_df Parameter data.frame.
#' @return A data.frame with columns: category, check, status, details.
#' @export
check_psa_inputs <- function(param_df) {
  results <- list()
  add <- function(check, status, details = "") {
    results[[length(results) + 1L]] <<- data.frame(
      category = "PSA inputs", check = check,
      status = status, details = details, stringsAsFactors = FALSE
    )
  }

  psa_rows <- param_df[param_df$include_psa == TRUE, ]
  if (nrow(psa_rows) == 0) {
    add("PSA-eligible params", "WARN", "No parameters marked include_psa=TRUE.")
    return(do.call(rbind, results))
  }
  add("PSA-eligible params", "PASS", paste0(nrow(psa_rows), " parameters."))

  supported <- c("beta", "gamma", "lognormal", "normal", "uniform",
                 "triangular", "pert")
  n_fixed <- sum(tolower(psa_rows$distribution) == "fixed")
  if (n_fixed > 0) {
    add("No fixed distributions in PSA", "WARN",
        paste0(n_fixed, " param(s) have include_psa=TRUE but distribution='fixed'."))
  } else {
    add("No fixed distributions in PSA", "PASS", "")
  }

  n_unsupported <- sum(!tolower(psa_rows$distribution) %in% c(supported, "fixed"))
  if (n_unsupported > 0) {
    add("All distributions recognized", "FAIL",
        paste0(n_unsupported, " unrecognized distribution(s)."))
  } else {
    add("All distributions recognized", "PASS", "")
  }

  # Domain checks
  beta_rows <- psa_rows[tolower(psa_rows$distribution) == "beta", ]
  for (i in seq_len(nrow(beta_rows))) {
    nm <- beta_rows$name[i]
    m <- beta_rows$mean[i]
    if (is.finite(m) && (m <= 0 || m >= 1)) {
      add(paste0(nm, ": beta mean in (0,1)"), "FAIL", paste0("Mean = ", m))
    }
  }

  gamma_rows <- psa_rows[tolower(psa_rows$distribution) == "gamma", ]
  for (i in seq_len(nrow(gamma_rows))) {
    nm <- gamma_rows$name[i]
    m <- gamma_rows$mean[i]
    if (is.finite(m) && m <= 0) {
      add(paste0(nm, ": gamma mean > 0"), "FAIL", paste0("Mean = ", m))
    }
  }

  ln_rows <- psa_rows[tolower(psa_rows$distribution) == "lognormal", ]
  for (i in seq_len(nrow(ln_rows))) {
    nm <- ln_rows$name[i]
    m <- ln_rows$mean[i]
    if (is.finite(m) && m <= 0) {
      add(paste0(nm, ": lognormal mean > 0"), "FAIL", paste0("Mean = ", m))
    }
  }

  # Bounds width check
  has_bounds <- is.finite(psa_rows$lower) & is.finite(psa_rows$upper)
  no_bounds <- psa_rows[!has_bounds & tolower(psa_rows$distribution) != "fixed", ]
  if (nrow(no_bounds) > 0) {
    add("All PSA params have bounds", "WARN",
        paste0(nrow(no_bounds), " param(s) without finite bounds: ",
               paste(utils::head(no_bounds$name, 5), collapse = ", ")))
  } else {
    add("All PSA params have bounds", "PASS", "")
  }

  # Reasonable range (bounds not too wide relative to mean)
  bounded <- psa_rows[has_bounds, ]
  for (i in seq_len(nrow(bounded))) {
    nm <- bounded$name[i]
    m <- bounded$mean[i]
    lo <- bounded$lower[i]
    hi <- bounded$upper[i]
    if (!is.finite(m) || abs(m) < 1e-12) next
    width_ratio <- (hi - lo) / abs(m)
    if (width_ratio > 5) {
      add(paste0(nm, ": range not excessively wide"), "WARN",
          paste0("Range/mean = ", round(width_ratio, 1),
                 " (bounds: ", lo, " to ", hi, ", mean: ", m, ")."))
    }
  }

  n_domain_fail <- sum(grepl("FAIL", vapply(results, function(r) r$status, "")))
  if (n_domain_fail == 0) {
    add("Domain consistency", "PASS", "All distributions match parameter domains.")
  }

  do.call(rbind, results)
}

#' Check Dominance Logic
#'
#' Validates the frontier analysis: correctly identified dominated and
#' extendedly dominated strategies, monotone sequential ICERs, and
#' consistency between NMB ranking and ICER ordering.
#'
#' @param det_result Output from [run_deterministic()].
#' @param wtp WTP threshold used in the analysis.
#' @return A data.frame with columns: category, check, status, details.
#' @export
check_dominance_logic <- function(det_result, wtp = 100000) {
  results <- list()
  add <- function(check, status, details = "") {
    results[[length(results) + 1L]] <<- data.frame(
      category = "Dominance", check = check,
      status = status, details = details, stringsAsFactors = FALSE
    )
  }

  fs <- det_result$frontier_status
  if (is.null(fs)) {
    add("Frontier status available", "FAIL", "No frontier_status in results.")
    return(do.call(rbind, results))
  }
  add("Frontier status available", "PASS",
      paste0(nrow(fs), " strategies classified."))

  # Verify dominated strategies
  dom <- fs[fs$Status == "Dominated", ]
  front <- fs[fs$Status == "Frontier", ]

  for (i in seq_len(nrow(dom))) {
    d_cost <- dom$Cost[i]; d_qaly <- dom$QALY[i]
    truly_dom <- any(front$Cost <= d_cost & front$QALY >= d_qaly &
                       (front$Cost < d_cost | front$QALY > d_qaly))
    if (!truly_dom) {
      add(paste0("'", dom$Strategy[i], "' correctly dominated"), "FAIL",
          "No frontier strategy dominates it.")
    }
  }
  if (nrow(dom) > 0 && sum(grepl("FAIL", vapply(results, function(r) r$status, ""))) == 0) {
    add("All dominated correctly identified", "PASS",
        paste0(nrow(dom), " dominated strategies verified."))
  }

  # Frontier ordering
  front_ord <- front[order(front$QALY, front$Cost), ]
  if (nrow(front_ord) >= 2) {
    cost_increasing <- all(diff(front_ord$Cost) >= -1e-6)
    if (cost_increasing) {
      add("Frontier ordered by cost/QALY", "PASS", "")
    } else {
      add("Frontier ordered by cost/QALY", "WARN",
          "Frontier strategies not monotonically increasing in cost.")
    }
  }

  # Sequential ICER monotonicity
  icers <- det_result$icers
  if (!is.null(icers) && nrow(icers) >= 2) {
    icer_vals <- icers$ICER[is.finite(icers$ICER)]
    if (length(icer_vals) >= 2 && all(diff(icer_vals) >= -1e-6)) {
      add("Sequential ICERs monotone increasing", "PASS",
          paste0("Range: $", round(min(icer_vals)), " - $", round(max(icer_vals))))
    } else if (length(icer_vals) >= 2) {
      add("Sequential ICERs monotone increasing", "WARN",
          "ICERs are not monotonically increasing along the frontier.")
    }
  }

  # NMB consistency
  tab <- det_result$table
  if (!is.null(tab) && "NMB" %in% names(tab)) {
    ref <- det_result$ref
    ref_nmb <- tab$NMB[tab$Strategy == ref]
    max_nmb <- max(tab$NMB, na.rm = TRUE)
    if (abs(ref_nmb - max_nmb) < 1e-6) {
      add("Reference = max NMB", "PASS",
          paste0("Reference '", ref, "' has highest NMB ($", round(ref_nmb), ")."))
    } else {
      add("Reference = max NMB", "WARN",
          paste0("Reference '", ref, "' NMB ($", round(ref_nmb),
                 ") != max ($", round(max_nmb), ")."))
    }
  }

  do.call(rbind, results)
}

#' Validate Model Outputs
#'
#' Runs a model at base case and checks that costs, QALYs, trace, and
#' survival outputs are internally consistent and plausible.
#'
#' @param model A `cea_model` object.
#' @param params_df Parameter data.frame.
#' @param wtp WTP threshold.
#' @return A data.frame with columns: category, check, status, details.
#' @export
validate_model_outputs <- function(model, params_df, wtp = 100000) {
  results <- list()
  add <- function(check, status, details = "") {
    results[[length(results) + 1L]] <<- data.frame(
      category = "Model outputs", check = check,
      status = status, details = details, stringsAsFactors = FALSE
    )
  }

  strategies <- model$info()$strategies
  pars <- stats::setNames(as.list(params_df$base), params_df$name)

  # Find time horizon for QALY plausibility
  th_names <- c("time_horizon", "n_cycles", "horizon")
  th <- NA_real_
  for (nm in th_names) {
    if (nm %in% params_df$name) {
      th <- params_df$base[params_df$name == nm]
      break
    }
  }

  for (s in strategies) {
    res <- safe_simulate(model, pars, s)
    if (is.null(res)) {
      add(paste0(s, ": simulation"), "FAIL", "simulate() returned NULL.")
      next
    }

    # Cost checks
    if (res$cost < 0) {
      add(paste0(s, ": cost >= 0"), "FAIL",
          paste0("Cost = $", round(res$cost)))
    } else {
      add(paste0(s, ": cost >= 0"), "PASS",
          paste0("$", formatC(round(res$cost), big.mark = ",")))
    }

    if (res$cost > 5e6) {
      add(paste0(s, ": cost plausible"), "WARN",
          paste0("Cost = $", formatC(round(res$cost), big.mark = ","),
                 " (>$5M per patient)."))
    }

    # QALY checks
    if (res$qaly < 0) {
      add(paste0(s, ": QALY >= 0"), "FAIL",
          paste0("QALY = ", round(res$qaly, 4)))
    } else {
      add(paste0(s, ": QALY >= 0"), "PASS",
          paste0(round(res$qaly, 4)))
    }

    if (is.finite(th) && res$qaly > th * 1.1) {
      add(paste0(s, ": QALY <= horizon"), "WARN",
          paste0("QALY = ", round(res$qaly, 3),
                 " > time horizon (", th, " years)."))
    }

    # Trace checks
    if (!is.null(res$trace) && is.matrix(res$trace)) {
      trace_checks <- check_transition_matrix(res$trace)
      trace_checks$check <- paste0(s, ": ", trace_checks$check)
      results <- c(results, split(trace_checks, seq_len(nrow(trace_checks))))
      next  # trace checks already added
    }

    # Survival checks
    sv <- res$survival
    if (!is.null(sv) && is.data.frame(sv) && nrow(sv) > 0) {
      if (all(c("time", "surv", "curve") %in% names(sv))) {
        for (crv in unique(sv$curve)) {
          sub <- sv[sv$curve == crv, ]
          sub <- sub[order(sub$time), ]

          if (abs(sub$surv[1] - 1) > 0.01) {
            add(paste0(s, ": ", crv, " starts at 1"), "WARN",
                paste0("S(0) = ", round(sub$surv[1], 4)))
          }

          if (any(sub$surv < -0.01 | sub$surv > 1.01, na.rm = TRUE)) {
            add(paste0(s, ": ", crv, " in [0,1]"), "FAIL",
                paste0("Range: [", round(min(sub$surv, na.rm = TRUE), 4),
                       ", ", round(max(sub$surv, na.rm = TRUE), 4), "]"))
          } else {
            add(paste0(s, ": ", crv, " in [0,1]"), "PASS", "")
          }
        }

        # OS >= PFS check
        curves <- unique(sv$curve)
        if ("OS" %in% curves && "PFS" %in% curves) {
          os <- sv[sv$curve == "OS", ]
          pfs <- sv[sv$curve == "PFS", ]
          os <- os[order(os$time), ]
          pfs <- pfs[order(pfs$time), ]
          if (nrow(os) == nrow(pfs)) {
            violations <- sum(os$surv < pfs$surv - 0.01, na.rm = TRUE)
            if (violations > 0) {
              add(paste0(s, ": OS >= PFS"), "FAIL",
                  paste0(violations, " time points where OS < PFS."))
            } else {
              add(paste0(s, ": OS >= PFS"), "PASS", "")
            }
          }
        }
      }
    }
  }

  do.call(rbind, results)
}

#' Generate Comprehensive Validation Report
#'
#' Runs all validation checks on a model and compiles results into a
#' structured report with summary scores. Optionally exports to markdown.
#'
#' @param model A `cea_model` object.
#' @param params_df Parameter data.frame (defaults to model's default_params()).
#' @param wtp WTP threshold.
#' @param prob_groups Optional probability groups for [check_probabilities_sum()].
#' @param output_file Optional path to write a markdown report.
#' @return A list with `$summary` (counts), `$details` (full results table),
#'   `$score` (0-100), and `$output_file` if exported.
#' @export
generate_validation_report <- function(model, params_df = NULL, wtp = 100000,
                                       prob_groups = NULL, output_file = NULL) {
  if (is.null(params_df)) params_df <- model$default_params()

  all_checks <- list()

  # 1. Model outputs
  out_checks <- validate_model_outputs(model, params_df, wtp)
  all_checks <- c(all_checks, list(out_checks))

  # 2. Probability checks
  prob_checks <- check_probabilities_sum(params_df, prob_groups)
  all_checks <- c(all_checks, list(prob_checks))

  # 3. PSA inputs
  psa_checks <- check_psa_inputs(params_df)
  all_checks <- c(all_checks, list(psa_checks))

  # 4. Discounting
  disc_checks <- check_discounting(model, params_df, wtp)
  all_checks <- c(all_checks, list(disc_checks))

  # 5. Deterministic + dominance
  det <- tryCatch(
    run_deterministic(model, params_df, wtp = wtp),
    error = function(e) NULL
  )
  if (!is.null(det)) {
    dom_checks <- check_dominance_logic(det, wtp)
    all_checks <- c(all_checks, list(dom_checks))
  }

  # 6. Parameter validation
  if ("se" %in% names(params_df)) {
    param_checks <- validate_param_table(params_df)
    param_checks$category <- "Parameter validation"
    names(param_checks)[names(param_checks) == "parameter"] <- "check"
    names(param_checks)[names(param_checks) == "message"] <- "details"
    all_checks <- c(all_checks, list(param_checks))
  }

  # 7. Half-cycle correction
  hcc_checks <- tryCatch(check_half_cycle(model, params_df),
                          error = function(e) NULL)
  if (!is.null(hcc_checks)) all_checks <- c(all_checks, list(hcc_checks))

  # 8. PH assumption
  ph_checks <- tryCatch(check_ph_assumption(model, params_df),
                         error = function(e) NULL)
  if (!is.null(ph_checks)) all_checks <- c(all_checks, list(ph_checks))

  # 9. Scenario robustness (if model has predefined scenarios)
  if (!is.null(model$scenarios) && length(model$scenarios) >= 2) {
    sc_checks <- tryCatch(
      check_scenario_robustness(model, params_df, model$scenarios, wtp),
      error = function(e) NULL
    )
    if (!is.null(sc_checks)) all_checks <- c(all_checks, list(sc_checks))
  }

  # Combine
  details <- do.call(rbind, all_checks)
  rownames(details) <- NULL

  # Summary
  n_pass <- sum(details$status == "PASS", na.rm = TRUE)
  n_warn <- sum(details$status == "WARN", na.rm = TRUE)
  n_fail <- sum(details$status == "FAIL", na.rm = TRUE)
  total <- n_pass + n_warn + n_fail

  # Score: 100 - 5*FAIL - 1*WARN
  score <- max(0, 100 - 5 * n_fail - 1 * n_warn)

  summary_df <- data.frame(
    Status = c("PASS", "WARN", "FAIL", "TOTAL"),
    Count = c(n_pass, n_warn, n_fail, total),
    stringsAsFactors = FALSE
  )

  result <- list(
    summary = summary_df,
    details = details,
    score = score,
    model_name = model$info()$name
  )

  # Export if requested
  if (!is.null(output_file)) {
    lines <- c(
      paste0("# Validation Report: ", model$info()$name),
      "",
      paste0("**Score: ", score, "/100** | ",
             n_pass, " PASS | ", n_warn, " WARN | ", n_fail, " FAIL"),
      "",
      "## Details",
      ""
    )

    for (cat in unique(details$category)) {
      lines <- c(lines, paste0("### ", cat), "")
      sub <- details[details$category == cat, ]
      lines <- c(lines, "| Check | Status | Details |",
                  "|-------|--------|---------|")
      for (i in seq_len(nrow(sub))) {
        lines <- c(lines, paste0("| ", sub$check[i], " | **",
                                  sub$status[i], "** | ", sub$details[i], " |"))
      }
      lines <- c(lines, "")
    }

    writeLines(lines, output_file)
    result$output_file <- output_file
  }

  result
}
