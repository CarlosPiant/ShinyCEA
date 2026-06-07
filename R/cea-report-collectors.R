#' Collect All CEA Deterministic Results
#'
#' Runs deterministic analysis, frontier, tornado, and threshold, and
#' packages all outputs (tables, plots, metadata) into a single object
#' for report generation.
#'
#' @param model A `cea_model` object.
#' @param params_df Parameter data.frame.
#' @param wtp WTP threshold.
#' @param ref_strategy Optional reference strategy.
#' @param tornado_target Target strategy for tornado (auto-selected if NULL).
#' @param threshold_param Parameter for threshold analysis (first tornado param if NULL).
#' @return A list with `$det` (deterministic result), `$plots` (named ggplot list),
#'   `$tables` (named data.frame list).
#' @export
collect_cea_results <- function(model, params_df, wtp = 100000,
                                 ref_strategy = NULL, tornado_target = NULL,
                                 threshold_param = NULL) {
  det <- run_deterministic(model, params_df, wtp = wtp, ref_strategy = ref_strategy)

  # Plots
  plots <- list()
  plots$frontier <- plot_frontier(det$frontier_status,
                                  palette = cea_palette("classic"))

  # Tornado
  df_tor <- params_df[params_df$include_tornado &
                        (is.finite(params_df$lower) | is.finite(params_df$upper)), ]
  if (is.null(tornado_target)) {
    tornado_target <- setdiff(det$strategies, det$ref)[1]
  }

  if (nrow(df_tor) > 0 && !is.null(tornado_target)) {
    tor <- build_tornado_data(df_tor, det$pars, model,
                              det$ref, tornado_target,
                              metric = "INB", wtp = wtp)
    if (!is.null(tor) && nrow(tor) > 0) {
      plots$tornado <- plot_tornado(
        tor, unique(tor$base)[1],
        paste0("INB (", tornado_target, " vs ", det$ref, ")"),
        palette = cea_palette("classic")
      )
    }
  }

  # Survival
  cl_val <- 1
  if ("cycle_length" %in% params_df$name) {
    cl_val <- params_df$base[params_df$name == "cycle_length"]
  }
  surv_list <- lapply(det$strategies, function(s) {
    res <- det$res[[s]]
    if (!is.null(res$survival) && is.data.frame(res$survival)) {
      df <- res$survival
      df$strategy <- s
      df
    } else {
      NULL
    }
  })
  surv_data <- do.call(rbind, surv_list[!vapply(surv_list, is.null, logical(1))])
  if (!is.null(surv_data) && nrow(surv_data) > 0) {
    plots$survival <- plot_survival(surv_data, palette = cea_palette("classic"))
  }

  # Tables
  tables <- list(
    base_case = table_base_case(det),
    incremental = table_incremental(det),
    frontier = det$frontier_status[, c("Strategy", "Cost", "QALY", "Status")]
  )

  list(det = det, plots = plots, tables = tables)
}

#' Collect All PSA Results
#'
#' Runs PSA, CEAC, expected loss, and EVPI/EVPPI, and packages all outputs.
#'
#' @param model A `cea_model` object.
#' @param params_df Parameter data.frame.
#' @param det_result Output from [run_deterministic()] (needed for reference strategy).
#' @param wtp WTP threshold.
#' @param n_psa Number of PSA iterations.
#' @return A list with `$psa`, `$plots`, `$tables`.
#' @export
collect_psa_results <- function(model, params_df, det_result, wtp = 100000,
                                 n_psa = 1000L) {
  psa <- run_psa(model, params_df, n_sim = n_psa)
  strategies <- det_result$strategies
  ref <- det_result$ref

  plots <- list()
  tables <- list()

  # CE Plane
  inc_list <- lapply(setdiff(strategies, ref), function(s) {
    data.frame(
      strategy = s,
      inc_qaly = psa$results[[paste0(s, ".q")]] - psa$results[[paste0(ref, ".q")]],
      inc_cost = psa$results[[paste0(s, ".cost")]] - psa$results[[paste0(ref, ".cost")]],
      stringsAsFactors = FALSE
    )
  })
  inc_df <- do.call(rbind, inc_list)
  plots$ce_plane <- plot_ce_plane(inc_df, wtp, palette = cea_palette("classic"))

  # CEAC
  ceac <- compute_ceac(psa$results, strategies, wtp_max = max(wtp, 200000))
  plots$ceac <- plot_ceac(ceac, palette = cea_palette("classic"))

  # Expected Loss
  el <- compute_expected_loss(psa$results, strategies, wtp_max = max(wtp, 200000))
  plots$expected_loss <- plot_expected_loss(el, palette = cea_palette("classic"))

  # EVPI
  evpi <- compute_evpi(psa$results, strategies, wtp_max = max(wtp, 200000))
  plots$evpi <- plot_evpi(evpi, palette = cea_palette("classic"))

  # EVPPI (if draws available)
  if (!is.null(psa$draws) && ncol(psa$draws) > 0) {
    evppi <- compute_evppi(psa$results, psa$draws, strategies, wtp = wtp)
    plots$evppi <- plot_evppi(evppi, palette = cea_palette("classic"))
    tables$evppi <- evppi
  }

  # Summary table
  tables$psa_summary <- table_psa_summary(psa$results, strategies, wtp)

  list(psa = psa, plots = plots, tables = tables)
}

#' Collect BIA Results
#'
#' Runs BIA and packages outputs for reporting.
#'
#' @param model A `cea_model` object.
#' @param params_df Parameter data.frame.
#' @param population Either a `bia_population` object or a scalar population size.
#' @param current_mix Named numeric vector of current market shares.
#' @param new_mix Named numeric vector of new market shares.
#' @param years Number of projection years.
#' @return A list with `$bia`, `$plots`, `$tables`.
#' @export
collect_bia_results <- function(model, params_df, population,
                                 current_mix, new_mix, years = 5L) {
  if (inherits(population, "bia_population")) {
    shares <- define_market_shares(current_mix, new_mix)
    uptake <- define_uptake_curve(shares, years = population$years)
    bia <- run_bia_modern(population, shares, model = model, params_df = params_df,
                          uptake = uptake)
  } else {
    bia <- run_bia(model, params_df, population = population,
                   current_mix = current_mix,
                   new_mix = linear_uptake(current_mix, new_mix, years),
                   years = years)
  }

  plots <- list()
  plots$budget_impact <- plot_bia(bia$impact, palette = cea_palette("classic"))
  plots$waterfall <- plot_bia_waterfall(bia$impact, palette = cea_palette("classic"))

  tables <- list()
  if (inherits(population, "bia_population")) {
    tables$annual <- table_annual_budget(bia)
  } else {
    tables$annual <- bia$impact
  }

  list(bia = bia, plots = plots, tables = tables)
}
