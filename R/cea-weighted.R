#' Run Weighted Comparator Analysis
#'
#' Combines multiple strategies into weighted composites (e.g., by market share,
#' clinical guideline mix, or formulary distribution) and compares them against
#' individual strategies or other composites. This is useful when:
#' \itemize{
#'   \item The relevant comparator is the current standard of care, which is a
#'     mix of treatments (e.g., 60\% drug A + 40\% drug B).
#'   \item A new intervention replaces a portfolio of existing treatments.
#'   \item You want to compare a single new drug vs. the weighted average of
#'     all current options.
#' }
#'
#' @param model A `cea_model` object.
#' @param params_df Parameter data.frame (with 'name' and 'base' columns).
#' @param composites A named list of composites. Each composite is a named
#'   numeric vector of strategy weights (must sum to 1). Example:
#'   \code{list("Current SoC" = c(std = 0.6, t1 = 0.3, t2 = 0.1))}.
#'   Individual strategies can also be included by giving them weight 1:
#'   \code{list("New Drug" = c(t4 = 1))}.
#' @param wtp WTP threshold.
#' @return A list with:
#'   \describe{
#'     \item{composites}{Data.frame with Composite, Cost, QALY, NMB columns}
#'     \item{pairwise}{Data.frame of all pairwise incremental comparisons}
#'     \item{details}{Data.frame with per-strategy contributions within each composite}
#'   }
#' @export
#' @examples
#' \dontrun{
#' m <- cea_sick_sicker()
#' composites <- list(
#'   "Current mix" = c(std = 0.6, t1 = 0.2, t2 = 0.1, t3 = 0.1),
#'   "New drug alone" = c(t4 = 1),
#'   "New protocol" = c(t4 = 0.7, t2 = 0.3)
#' )
#' run_weighted_analysis(m, m$default_params(), composites, wtp = 100000)
#' }
run_weighted_analysis <- function(model, params_df, composites, wtp = 100000) {
  if (!is.list(composites) || is.null(names(composites))) {
    stop("composites must be a named list of named numeric vectors.", call. = FALSE)
  }

  strategies <- model$info()$strategies
  par_list <- stats::setNames(as.list(params_df$base), params_df$name)

  # Simulate all individual strategies once
  strat_results <- stats::setNames(
    lapply(strategies, function(s) safe_simulate(model, par_list, s)),
    strategies
  )

  failed <- names(which(vapply(strat_results, is.null, logical(1))))
  if (length(failed)) {
    stop("Simulation failed for: ", paste(failed, collapse = ", "), call. = FALSE)
  }

  # Compute weighted composites
  detail_rows <- list()
  comp_rows <- list()

  for (comp_name in names(composites)) {
    weights <- composites[[comp_name]]

    if (!is.numeric(weights) || is.null(names(weights))) {
      stop("Composite '", comp_name, "' must be a named numeric vector.", call. = FALSE)
    }
    unknown <- setdiff(names(weights), strategies)
    if (length(unknown)) {
      stop("Composite '", comp_name, "' references unknown strategies: ",
           paste(unknown, collapse = ", "), call. = FALSE)
    }
    if (abs(sum(weights) - 1) > 0.01) {
      stop("Composite '", comp_name, "' weights must sum to 1 (got ",
           round(sum(weights), 4), ").", call. = FALSE)
    }

    total_cost <- 0
    total_qaly <- 0

    for (s in names(weights)) {
      w <- weights[[s]]
      if (w <= 0) next
      sc <- strat_results[[s]]$cost * w
      sq <- strat_results[[s]]$qaly * w

      total_cost <- total_cost + sc
      total_qaly <- total_qaly + sq

      detail_rows[[length(detail_rows) + 1L]] <- data.frame(
        Composite = comp_name, Strategy = s, Weight = w,
        Cost_Weighted = sc, QALY_Weighted = sq,
        stringsAsFactors = FALSE
      )
    }

    comp_rows[[length(comp_rows) + 1L]] <- data.frame(
      Composite = comp_name,
      Cost = total_cost, QALY = total_qaly,
      NMB = wtp * total_qaly - total_cost,
      stringsAsFactors = FALSE
    )
  }

  comp_df <- do.call(rbind, comp_rows)
  detail_df <- do.call(rbind, detail_rows)

  # Pairwise incremental comparisons
  n_comp <- nrow(comp_df)
  pair_rows <- list()
  if (n_comp >= 2) {
    for (i in seq_len(n_comp)) {
      for (j in seq_len(n_comp)) {
        if (i == j) next
        dc <- comp_df$Cost[j] - comp_df$Cost[i]
        dq <- comp_df$QALY[j] - comp_df$QALY[i]
        icer <- if (dq > 0) dc / dq else NA_real_
        inb <- wtp * dq - dc

        pair_rows[[length(pair_rows) + 1L]] <- data.frame(
          Reference = comp_df$Composite[i],
          Comparator = comp_df$Composite[j],
          IncCost = dc, IncQALY = dq,
          ICER = icer, INB = inb,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  pair_df <- if (length(pair_rows)) do.call(rbind, pair_rows) else {
    data.frame(Reference = character(0), Comparator = character(0),
               IncCost = numeric(0), IncQALY = numeric(0),
               ICER = numeric(0), INB = numeric(0),
               stringsAsFactors = FALSE)
  }

  list(composites = comp_df, pairwise = pair_df, details = detail_df)
}

#' Plot Weighted Comparator Results
#'
#' Bar chart comparing cost and QALY across composites, with strategy
#' composition shown as stacked segments.
#'
#' @param detail_df Data.frame from [run_weighted_analysis()]$details.
#' @param metric One of "Cost" or "QALY".
#' @param palette Optional palette from [cea_palette()].
#' @return A ggplot object.
#' @export
plot_weighted_comparison <- function(detail_df, metric = c("Cost", "QALY"),
                                     palette = NULL) {
  if (is.null(palette)) palette <- cea_palette("classic")
  metric <- match.arg(metric)

  col <- if (metric == "Cost") "Cost_Weighted" else "QALY_Weighted"
  y_label <- if (metric == "Cost") "Weighted Cost ($)" else "Weighted QALYs"

  ggplot2::ggplot(detail_df, ggplot2::aes(x = .data$Composite,
                                           y = .data[[col]],
                                           fill = .data$Strategy)) +
    ggplot2::geom_col(width = 0.6, alpha = 0.85) +
    scale_fill_cea(palette) +
    ggplot2::labs(x = NULL, y = y_label,
                  title = paste0("Weighted ", metric, " by Composite"),
                  fill = "Strategy") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom",
                   panel.grid.major.x = ggplot2::element_blank())
}

#' Plot Pairwise ICER/INB for Weighted Composites
#'
#' @param pairwise_df Data.frame from [run_weighted_analysis()]$pairwise.
#' @param metric One of "ICER" or "INB".
#' @param wtp WTP threshold.
#' @param palette Optional palette from [cea_palette()].
#' @return A ggplot object.
#' @export
plot_weighted_pairwise <- function(pairwise_df, metric = c("ICER", "INB"),
                                   wtp = 100000, palette = NULL) {
  if (is.null(palette)) palette <- cea_palette("classic")
  metric <- match.arg(metric)

  if (nrow(pairwise_df) == 0) {
    return(ggplot2::ggplot() +
             ggplot2::annotate("text", x = 0.5, y = 0.5,
                               label = "Need at least 2 composites for comparison.",
                               size = 5, color = "#999999") +
             ggplot2::theme_void())
  }

  df <- pairwise_df
  df$label <- paste0(df$Comparator, " vs\n", df$Reference)

  if (metric == "ICER") {
    df$value <- df$ICER
    ref_line <- wtp
    x_label <- "ICER ($/QALY)"
    df$ce <- ifelse(!is.na(df$value) & df$value <= wtp & df$value >= 0,
                    "Cost-effective", "Not cost-effective")
  } else {
    df$value <- df$INB
    ref_line <- 0
    x_label <- "Incremental Net Benefit ($)"
    df$ce <- ifelse(!is.na(df$value) & df$value >= 0,
                    "Cost-effective", "Not cost-effective")
  }

  df <- df[is.finite(df$value), ]
  if (nrow(df) == 0) {
    return(ggplot2::ggplot() +
             ggplot2::annotate("text", x = 0.5, y = 0.5,
                               label = "No finite values to display.",
                               size = 5, color = "#999999") +
             ggplot2::theme_void())
  }

  df$label <- factor(df$label, levels = rev(unique(df$label)))

  ggplot2::ggplot(df, ggplot2::aes(x = .data$value, y = .data$label)) +
    ggplot2::geom_vline(xintercept = ref_line, linetype = "dashed", color = "#999999") +
    ggplot2::geom_segment(
      ggplot2::aes(x = ref_line, xend = .data$value, yend = .data$label),
      color = "#CCCCCC", linewidth = 0.5
    ) +
    ggplot2::geom_point(ggplot2::aes(color = .data$ce), size = 3.5) +
    ggplot2::scale_color_manual(
      name = NULL,
      values = c("Cost-effective" = palette$primary,
                 "Not cost-effective" = palette$dominated)
    ) +
    ggplot2::labs(x = x_label, y = NULL,
                  title = paste0("Pairwise ", metric, " Comparison")) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom",
                   panel.grid.major.y = ggplot2::element_blank())
}
