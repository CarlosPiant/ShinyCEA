#' Run Multiple Scenarios
#'
#' Runs deterministic analysis for a set of named scenarios, each defined
#' by a parameter modification relative to the base case.
#'
#' @param model A `cea_model` object.
#' @param base_params Base-case parameter data.frame.
#' @param scenarios A named list of scenarios. Each element is a named list
#'   of parameter overrides (e.g., `list(d_cost = 0, d_qaly = 0)`).
#' @param wtp WTP threshold.
#' @param ref_strategy Optional reference strategy. When `NULL`, the
#'   reference is the max-NMB strategy of the base case (`base_params`) and
#'   is kept for every scenario, so `ICER_vsRef` always compares against the
#'   same strategy (review finding D4: a per-scenario reference left the
#'   strategy of interest without an ICER).
#' @return A data.frame with columns: Scenario, Strategy, Cost, QALY, NMB,
#'   ICER_vsRef, Reference.
#' @export
#' @examples
#' \dontrun{
#' m <- cea_sick_sicker()
#' scenarios <- list(
#'   "Base case" = list(),
#'   "No discounting" = list(d_cost = 0, d_qaly = 0),
#'   "High drug cost" = list(drug_t1 = 800, drug_t2 = 1200)
#' )
#' run_scenarios(m, m$default_params(), scenarios, wtp = 100000)
#' }
run_scenarios <- function(model, base_params, scenarios, wtp = 100000,
                          ref_strategy = NULL) {
  if (!is.list(scenarios) || is.null(names(scenarios))) {
    stop("scenarios must be a named list.", call. = FALSE)
  }
  # Fix the comparator once, from the base case, unless the caller pinned one.
  if (is.null(ref_strategy)) {
    ref_strategy <- run_deterministic(model, base_params, wtp = wtp)$ref
  }

  results <- lapply(names(scenarios), function(sc_name) {
    overrides <- scenarios[[sc_name]]
    params_df <- base_params

    for (pname in names(overrides)) {
      idx <- which(params_df$name == pname)
      if (length(idx) == 1) {
        params_df$base[idx] <- overrides[[pname]]
        params_df$mean[idx] <- overrides[[pname]]
      }
    }

    det <- tryCatch(
      run_deterministic(model, params_df, wtp = wtp, ref_strategy = ref_strategy),
      error = function(e) NULL
    )

    if (is.null(det)) {
      return(data.frame(Scenario = sc_name, Strategy = NA, Cost = NA, QALY = NA,
                        NMB = NA, ICER_vsRef = NA, Reference = NA,
                        stringsAsFactors = FALSE))
    }

    tab <- det$table
    tab$Scenario <- sc_name
    tab$Reference <- det$ref
    tab[, c("Scenario", "Strategy", "Cost", "QALY", "NMB", "ICER_vsRef", "Reference")]
  })

  do.call(rbind, results)
}

#' Compare Scenarios Summary
#'
#' Extracts the optimal strategy (highest NMB) per scenario for a compact
#' comparison view.
#'
#' @param scenario_results Data.frame from [run_scenarios()].
#' @return A data.frame with one row per scenario: Scenario, Optimal_Strategy,
#'   Cost, QALY, NMB.
#' @export
summarize_scenarios <- function(scenario_results) {
  scenarios <- unique(scenario_results$Scenario)

  summ <- lapply(scenarios, function(sc) {
    sub <- scenario_results[scenario_results$Scenario == sc, ]
    sub <- sub[!is.na(sub$NMB), ]
    if (nrow(sub) == 0) {
      return(data.frame(Scenario = sc, Optimal_Strategy = NA,
                        Cost = NA, QALY = NA, NMB = NA,
                        stringsAsFactors = FALSE))
    }
    best <- sub[which.max(sub$NMB), ]
    data.frame(Scenario = sc, Optimal_Strategy = best$Strategy,
               Cost = best$Cost, QALY = best$QALY, NMB = best$NMB,
               stringsAsFactors = FALSE)
  })

  do.call(rbind, summ)
}

#' Scenario Comparison Forest Plot
#'
#' Displays the ICER (or NMB) of a target strategy vs. reference across
#' scenarios as a forest-plot-style dot-and-line chart. Each row is a
#' scenario; a vertical reference line marks the WTP threshold (for ICER)
#' or zero (for NMB).
#'
#' @param scenario_results Data.frame from [run_scenarios()].
#' @param target Target strategy to compare vs. reference.
#' @param metric One of "ICER" or "NMB".
#' @param wtp WTP threshold (used for the reference line in ICER mode).
#' @param palette Optional palette from [cea_palette()].
#' @return A ggplot object.
#' @export
plot_scenario_forest <- function(scenario_results, target, metric = c("ICER", "NMB"),
                                 wtp = 100000, palette = NULL) {
  if (is.null(palette)) palette <- cea_palette("cream")
  metric <- match.arg(metric)

  df <- scenario_results[scenario_results$Strategy == target, ]
  if (nrow(df) == 0) {
    return(ggplot2::ggplot() +
             ggplot2::annotate("text", x = 0.5, y = 0.5,
                               label = paste0("Strategy '", target, "' not found."),
                               size = 5, color = "#999999") +
             ggplot2::theme_void())
  }

  if (metric == "ICER") {
    df$value <- df$ICER_vsRef
    x_label <- "ICER vs. reference ($/QALY)"
    ref_line <- wtp
    plot_title <- paste0("ICER of '", target, "' across scenarios")
    # Cap extreme ICERs for readability
    cap <- wtp * 5
    df$value <- ifelse(is.finite(df$value), pmin(df$value, cap), NA_real_)
    df$capped <- ifelse(!is.na(df$value) & df$value >= cap, TRUE, FALSE)
  } else {
    df$value <- df$NMB
    x_label <- "Net Monetary Benefit ($)"
    ref_line <- 0
    plot_title <- paste0("NMB of '", target, "' across scenarios")
    df$capped <- FALSE
  }

  df <- df[!is.na(df$value), ]
  if (nrow(df) == 0) {
    return(ggplot2::ggplot() +
             ggplot2::annotate("text", x = 0.5, y = 0.5,
                               label = "No finite values to plot.",
                               size = 5, color = "#999999") +
             ggplot2::theme_void())
  }

  df$Scenario <- factor(df$Scenario, levels = rev(df$Scenario))

  # Color: cost-effective (left of WTP for ICER, right of 0 for NMB) vs not
  if (metric == "ICER") {
    df$ce <- ifelse(df$value <= wtp, "Cost-effective", "Not cost-effective")
  } else {
    df$ce <- ifelse(df$value >= 0, "Cost-effective", "Not cost-effective")
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$value, y = .data$Scenario)) +
    ggplot2::geom_vline(xintercept = ref_line, linetype = "dashed", color = "#999999") +
    ggplot2::geom_segment(
      ggplot2::aes(x = ref_line, xend = .data$value, yend = .data$Scenario),
      color = "#CCCCCC", linewidth = 0.5
    ) +
    ggplot2::geom_point(
      ggplot2::aes(color = .data$ce), size = 3.5
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = ifelse(.data$capped, "> cap",
                                   formatC(round(.data$value), format = "f",
                                           big.mark = ",", digits = 0))),
      hjust = ifelse(df$value >= ref_line, -0.3, 1.3),
      size = 3, color = "#333333"
    ) +
    ggplot2::scale_color_manual(
      name = NULL,
      values = c("Cost-effective" = palette$primary,
                 "Not cost-effective" = palette$dominated)
    ) +
    ggplot2::labs(x = x_label, y = NULL, title = plot_title) +
    theme_cea() +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )

  p
}
