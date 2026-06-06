#' Tornado Plot (Color by Bound)
#'
#' @param tor_df Data.frame from build_tornado_data() with columns: name, bound, value, base.
#' @param base_val Scalar base-case metric value.
#' @param metric_label Label for x-axis.
#' @param title Plot title.
#' @param palette Optional palette from [cea_palette()]. Defaults to classic.
#' @return A ggplot object or NULL if data is insufficient.
#' @export
plot_tornado <- function(tor_df, base_val, metric_label, title = NULL, palette = NULL) {
  if (is.null(tor_df) || !nrow(tor_df) || !is.finite(base_val)) return(NULL)
  if (is.null(palette)) palette <- cea_palette("classic")

  order_df <- stats::aggregate(
    abs(tor_df$value - tor_df$base),
    by = list(name = tor_df$name),
    FUN = max, na.rm = TRUE
  )
  names(order_df)[2] <- "width"
  order_df <- order_df[order(-order_df$width), ]
  tor_df$name <- factor(tor_df$name, levels = order_df$name)

  tor_df$xmin <- pmin(tor_df$base, tor_df$value)
  tor_df$xmax <- pmax(tor_df$base, tor_df$value)

  fmt_val <- function(x) {
    ifelse(is.finite(x), formatC(round(x, 0), big.mark = ",", format = "f", digits = 0), "NA")
  }

  p <- ggplot2::ggplot(tor_df, ggplot2::aes(y = .data$name)) +
    ggplot2::geom_segment(
      ggplot2::aes(x = .data$xmin, xend = .data$xmax,
                   yend = .data$name, color = .data$bound),
      linewidth = 5, lineend = "butt", alpha = 0.7
    ) +
    ggplot2::geom_vline(xintercept = base_val, linetype = "dashed", color = "#555555") +
    ggplot2::geom_text(
      ggplot2::aes(x = .data$value, label = fmt_val(.data$value)),
      hjust = ifelse(tor_df$value >= tor_df$base, -0.1, 1.1),
      size = 3, color = "#333333"
    ) +
    ggplot2::scale_color_manual(
      name = NULL,
      values = c("Lower bound" = palette$secondary, "Upper bound" = palette$primary)
    ) +
    ggplot2::labs(
      x = metric_label, y = NULL,
      title = if (!is.null(title)) title else "Tornado diagram"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
  p
}

#' Cost-Effectiveness Plane Plot
#'
#' @param inc_df Data.frame with columns: strategy, inc_qaly, inc_cost.
#' @param wtp WTP threshold for the diagonal line.
#' @param palette Optional palette from [cea_palette()].
#' @return A ggplot object.
#' @export
plot_ce_plane <- function(inc_df, wtp, palette = NULL) {
  if (is.null(palette)) palette <- cea_palette("classic")
  inc_df <- inc_df[is.finite(inc_df$inc_qaly) & is.finite(inc_df$inc_cost), ]

  ggplot2::ggplot(inc_df, ggplot2::aes(x = .data$inc_qaly, y = .data$inc_cost,
                                        color = .data$strategy)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3, color = "#999999") +
    ggplot2::geom_vline(xintercept = 0, linewidth = 0.3, color = "#999999") +
    ggplot2::geom_point(alpha = 0.4) +
    ggplot2::geom_abline(slope = wtp, intercept = 0, linetype = "dotted", color = "#555555") +
    scale_color_cea(palette) +
    ggplot2::labs(x = "\u0394 QALYs", y = "\u0394 Costs", color = "Strategy") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")
}

#' CEAC Plot
#'
#' @param ceac_long Data.frame with columns: WTP, Strategy, ProbCE.
#' @param palette Optional palette from [cea_palette()].
#' @return A ggplot object.
#' @export
plot_ceac <- function(ceac_long, palette = NULL) {
  if (is.null(palette)) palette <- cea_palette("classic")

  ggplot2::ggplot(ceac_long, ggplot2::aes(x = .data$WTP, y = .data$ProbCE,
                                           color = .data$Strategy)) +
    ggplot2::geom_line(linewidth = 0.9) +
    scale_color_cea(palette) +
    ggplot2::labs(x = "Willingness-to-pay", y = "Probability cost-effective",
                  color = "Strategy") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")
}

#' Cost-Effectiveness Frontier Plot
#'
#' @param classified_df Data.frame with columns: Strategy, Cost, QALY, Status.
#' @param title Plot title.
#' @param palette Optional palette from [cea_palette()].
#' @return A ggplot object.
#' @export
plot_frontier <- function(classified_df, title = "Cost-Effectiveness Frontier", palette = NULL) {
  if (is.null(palette)) palette <- cea_palette("classic")
  stopifnot(all(c("Strategy", "Cost", "QALY", "Status") %in% names(classified_df)))

  fr <- classified_df[classified_df$Status == "Frontier", ]
  fr <- fr[order(fr$QALY, fr$Cost), ]

  p <- ggplot2::ggplot(classified_df, ggplot2::aes(x = .data$QALY, y = .data$Cost)) +
    ggplot2::geom_point(ggplot2::aes(color = .data$Status, shape = .data$Status),
                        size = 3, alpha = 0.95)

  if (nrow(fr) >= 2) {
    p <- p + ggplot2::geom_path(
      data = fr, ggplot2::aes(x = .data$QALY, y = .data$Cost),
      linetype = "dashed", linewidth = 0.9, color = palette$frontier
    )
  }

  if (requireNamespace("ggrepel", quietly = TRUE)) {
    p <- p + ggrepel::geom_text_repel(
      ggplot2::aes(label = .data$Strategy),
      size = 4.5, max.overlaps = 100, box.padding = 0.3,
      min.segment.length = 0.0, seed = 2025, show.legend = FALSE
    )
  } else {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = .data$Strategy),
      size = 4, vjust = -0.5, show.legend = FALSE
    )
  }

  p + ggplot2::scale_color_manual(values = c(
    "Frontier" = palette$frontier,
    "Dominated" = palette$dominated,
    "Extendedly dominated" = palette$ext_dominated
  )) +
    ggplot2::scale_shape_manual(values = c(
      "Frontier" = 16, "Dominated" = 17, "Extendedly dominated" = 15
    )) +
    ggplot2::labs(x = "QALYs", y = "Costs", title = title, color = NULL, shape = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")
}

#' Threshold Analysis Plot
#'
#' @param grid_df Data.frame from threshold_analysis()$grid.
#' @param threshold Numeric threshold value (may be NA).
#' @param param_name Parameter name for x-axis label.
#' @param wtp WTP value.
#' @param mode "icer" or "inb".
#' @param palette Optional palette from [cea_palette()].
#' @return A ggplot object.
#' @export
plot_threshold <- function(grid_df, threshold, param_name, wtp,
                           mode = c("icer", "inb"), palette = NULL) {
  if (is.null(palette)) palette <- cea_palette("classic")
  mode <- match.arg(mode)

  if (mode == "icer") {
    y <- ifelse(is.finite(grid_df$delta_cost) & is.finite(grid_df$delta_qaly) &
                  grid_df$delta_qaly > 0,
                grid_df$delta_cost / grid_df$delta_qaly, NA_real_)
    y_label <- "ICER vs reference"
    hline <- wtp
    plot_title <- "ICER profile"
  } else {
    y <- ifelse(is.finite(grid_df$delta_cost) & is.finite(grid_df$delta_qaly),
                wtp * grid_df$delta_qaly - grid_df$delta_cost, NA_real_)
    y_label <- "Incremental Net Benefit"
    hline <- 0
    plot_title <- "INB profile"
  }

  df_plot <- grid_df
  df_plot$y <- y

  p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = .data$param_value, y = .data$y)) +
    ggplot2::geom_line(linewidth = 1, color = palette$primary) +
    ggplot2::geom_hline(yintercept = hline, linetype = "dashed", color = "#555555") +
    ggplot2::labs(x = param_name, y = y_label, title = plot_title) +
    ggplot2::theme_minimal(base_size = 12)

  if (is.finite(threshold)) {
    p <- p +
      ggplot2::geom_vline(xintercept = threshold, linetype = "dotted", color = palette$secondary) +
      ggplot2::annotate("text", x = threshold, y = hline,
                        label = paste0("Threshold \u2248 ", round(threshold, 3)),
                        vjust = -1, angle = 90, size = 5, color = "#333333")
  }
  p
}

#' Survival Plot
#'
#' @param surv_df Data.frame with columns: time, surv, strategy, curve.
#' @param curve_name Which curve to plot (e.g. "OS", "PFS").
#' @param envelope_df Optional PSA envelope data.frame with: time, strategy, median, lower, upper, curve.
#' @param facet Logical; facet by strategy?
#' @param palette Optional palette from [cea_palette()].
#' @return A ggplot object.
#' @export
plot_survival <- function(surv_df, curve_name = "OS", envelope_df = NULL,
                          facet = FALSE, palette = NULL) {
  if (is.null(palette)) palette <- cea_palette("classic")
  df <- surv_df[surv_df$curve == curve_name, ]
  df$surv <- pmin(pmax(df$surv, 0), 1)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$time, y = .data$surv,
                                         color = .data$strategy)) +
    ggplot2::geom_line(linewidth = 1) +
    scale_color_cea(palette) +
    ggplot2::labs(x = "Time", y = paste0(curve_name, " survival S(t)"),
                  color = "Strategy") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")

  if (!is.null(envelope_df)) {
    envc <- envelope_df[envelope_df$curve == curve_name, ]
    if (nrow(envc) > 0) {
      p <- p +
        ggplot2::geom_ribbon(
          data = envc,
          ggplot2::aes(x = .data$time, ymin = .data$lower, ymax = .data$upper,
                       fill = .data$strategy),
          alpha = 0.20, color = NA, inherit.aes = FALSE
        ) +
        scale_fill_cea(palette) +
        ggplot2::geom_line(
          data = envc,
          ggplot2::aes(x = .data$time, y = .data$median, color = .data$strategy),
          linewidth = 0.9, linetype = "solid", inherit.aes = FALSE
        )
    }
  }

  if (facet) p <- p + ggplot2::facet_wrap(~ strategy, scales = "free_y")
  p
}

#' Markov Trace Plot
#'
#' @param trace_df Data.frame with columns: strategy, time, state, prob.
#' @param state_filter Optional; show only this state (across strategies).
#' @param facet Logical; facet by strategy?
#' @param palette Optional palette from [cea_palette()].
#' @return A ggplot object.
#' @export
plot_trace <- function(trace_df, state_filter = NULL, facet = TRUE, palette = NULL) {
  if (is.null(palette)) palette <- cea_palette("classic")

  if (!is.null(state_filter)) {
    df <- trace_df[trace_df$state == state_filter, ]
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$time, y = .data$prob,
                                           color = .data$strategy)) +
      ggplot2::geom_line(linewidth = 1) +
      scale_color_cea(palette) +
      ggplot2::labs(x = "Time", y = paste0("P(", state_filter, ")"),
                    color = "Strategy") +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(legend.position = "bottom")
  } else {
    p <- ggplot2::ggplot(trace_df, ggplot2::aes(x = .data$time, y = .data$prob,
                                                 color = .data$state)) +
      ggplot2::geom_line(linewidth = 1) +
      scale_color_cea(palette) +
      ggplot2::labs(x = "Time", y = "State occupancy", color = "State") +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(legend.position = "bottom")
    if (facet) p <- p + ggplot2::facet_wrap(~ strategy, scales = "free_y")
  }
  p
}
