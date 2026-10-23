#' Run Subgroup Analysis
#'
#' Runs the CEA model for user-defined subgroups, each with its own parameter
#' overrides (e.g., different baseline risk, age-specific utilities). Returns
#' a data.frame suitable for forest-plot display and comparison.
#'
#' @param model A `cea_model` object.
#' @param base_params Base-case parameter data.frame.
#' @param subgroups A named list of subgroups. Each element is a named list
#'   of parameter overrides. Use `list()` for the overall population.
#' @param target Target strategy for ICER/INB computation.
#' @param ref Reference strategy.
#' @param wtp WTP threshold.
#' @return A data.frame with columns: Subgroup, Strategy, Cost, QALY, NMB,
#'   ICER, IncCost, IncQALY.
#' @export
#' @examples
#' \dontrun{
#' m <- cea_sick_sicker()
#' subgroups <- list(
#'   "Overall" = list(),
#'   "High risk" = list(p_HS_std = 0.08, p_S1S2_std = 0.10),
#'   "Low risk" = list(p_HS_std = 0.02, p_S1S2_std = 0.03),
#'   "Elderly" = list(u_H = 0.75, u_S1 = 0.55, p_HD_std = 0.01)
#' )
#' run_subgroups(m, m$default_params(), subgroups,
#'               target = "t1", ref = "std", wtp = 100000)
#' }
run_subgroups <- function(model, base_params, subgroups, target, ref,
                          wtp = 100000) {
  if (!is.list(subgroups) || is.null(names(subgroups))) {
    stop("subgroups must be a named list.", call. = FALSE)
  }

  strategies <- model$info()$strategies
  if (!target %in% strategies) {
    stop("target '", target, "' not in model strategies.", call. = FALSE)
  }
  if (!ref %in% strategies) {
    stop("ref '", ref, "' not in model strategies.", call. = FALSE)
  }

  results <- lapply(names(subgroups), function(sg_name) {
    overrides <- subgroups[[sg_name]]
    params_df <- base_params

    for (pname in names(overrides)) {
      idx <- which(params_df$name == pname)
      if (length(idx) == 1L) {
        params_df$base[idx] <- overrides[[pname]]
        params_df$mean[idx] <- overrides[[pname]]
      }
    }

    pars <- stats::setNames(as.list(params_df$base), params_df$name)

    res_ref <- safe_simulate(model, pars, ref)
    res_tgt <- safe_simulate(model, pars, target)

    if (is.null(res_ref) || is.null(res_tgt)) {
      return(data.frame(
        Subgroup = sg_name, Target = target, Reference = ref,
        Cost_Ref = NA_real_, Cost_Tgt = NA_real_,
        QALY_Ref = NA_real_, QALY_Tgt = NA_real_,
        IncCost = NA_real_, IncQALY = NA_real_,
        ICER = NA_real_, INB = NA_real_,
        stringsAsFactors = FALSE
      ))
    }

    inc_c <- res_tgt$cost - res_ref$cost
    inc_q <- res_tgt$qaly - res_ref$qaly
    icer <- if (inc_q > 0) inc_c / inc_q else NA_real_
    inb <- wtp * inc_q - inc_c

    data.frame(
      Subgroup = sg_name, Target = target, Reference = ref,
      Cost_Ref = res_ref$cost, Cost_Tgt = res_tgt$cost,
      QALY_Ref = res_ref$qaly, QALY_Tgt = res_tgt$qaly,
      IncCost = inc_c, IncQALY = inc_q,
      ICER = icer, INB = inb,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, results)
}

#' Subgroup Forest Plot
#'
#' Displays ICERs or INBs across subgroups as a forest-plot-style chart.
#' Each row is a subgroup; a vertical reference line marks the WTP threshold
#' (for ICER) or zero (for INB). Color indicates cost-effectiveness.
#'
#' @param subgroup_results Data.frame from [run_subgroups()].
#' @param metric One of "ICER" or "INB".
#' @param wtp WTP threshold.
#' @param palette Optional palette from [cea_palette()].
#' @return A ggplot object.
#' @export
plot_subgroup_forest <- function(subgroup_results, metric = c("ICER", "INB"),
                                 wtp = 100000, palette = NULL) {
  if (is.null(palette)) palette <- cea_palette("cream")
  metric <- match.arg(metric)

  df <- subgroup_results

  if (metric == "ICER") {
    df$value <- df$ICER
    ref_line <- wtp
    x_label <- "ICER ($/QALY)"
    plot_title <- paste0("ICER of '", df$Target[1], "' vs '", df$Reference[1],
                         "' by subgroup")
    # Cap extreme ICERs
    cap <- wtp * 5
    df$value <- ifelse(is.finite(df$value), pmin(df$value, cap), NA_real_)
    df$ce <- ifelse(!is.na(df$value) & df$value <= wtp,
                    "Cost-effective", "Not cost-effective")
  } else {
    df$value <- df$INB
    ref_line <- 0
    x_label <- "Incremental Net Benefit ($)"
    plot_title <- paste0("INB of '", df$Target[1], "' vs '", df$Reference[1],
                         "' by subgroup")
    df$ce <- ifelse(!is.na(df$value) & df$value >= 0,
                    "Cost-effective", "Not cost-effective")
  }

  df <- df[is.finite(df$value), ]
  if (nrow(df) == 0) {
    return(ggplot2::ggplot() +
             ggplot2::annotate("text", x = 0.5, y = 0.5,
                               label = "No finite values to plot.",
                               size = 5, color = "#999999") +
             ggplot2::theme_void())
  }

  df$Subgroup <- factor(df$Subgroup, levels = rev(df$Subgroup))

  fmt_label <- function(x) {
    formatC(round(x), format = "f", big.mark = ",", digits = 0)
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$value, y = .data$Subgroup)) +
    ggplot2::geom_vline(xintercept = ref_line, linetype = "dashed",
                        color = "#999999") +
    ggplot2::geom_segment(
      ggplot2::aes(x = ref_line, xend = .data$value, yend = .data$Subgroup),
      color = "#CCCCCC", linewidth = 0.5
    ) +
    ggplot2::geom_point(ggplot2::aes(color = .data$ce), size = 3.5) +
    ggplot2::geom_text(
      ggplot2::aes(label = fmt_label(.data$value)),
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
