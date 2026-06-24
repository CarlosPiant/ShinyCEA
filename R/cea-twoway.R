#' Two-Way Deterministic Sensitivity Analysis
#'
#' Varies two parameters simultaneously over a grid and computes the outcome
#' metric (INB or ICER) at each combination. Produces data suitable for
#' heatmap visualization with an optional contour showing the decision
#' boundary (where the optimal strategy switches).
#'
#' @param model A `cea_model` object.
#' @param base_pars Named list of base-case parameter values.
#' @param param1 Name of the first parameter.
#' @param param2 Name of the second parameter.
#' @param range1 Numeric vector of length 2: min and max for param1.
#' @param range2 Numeric vector of length 2: min and max for param2.
#' @param n_grid Number of grid points per dimension (default 25).
#' @param ref Reference strategy.
#' @param target Target strategy.
#' @param wtp WTP threshold.
#' @param metric One of "INB" or "ICER".
#' @return A list with:
#'   \describe{
#'     \item{grid}{Data.frame with param1, param2, and metric columns}
#'     \item{param1}{Name of first parameter}
#'     \item{param2}{Name of second parameter}
#'     \item{ref}{Reference strategy}
#'     \item{target}{Target strategy}
#'     \item{metric}{Metric used}
#'   }
#' @export
twoway_dsa <- function(model, base_pars, param1, param2,
                       range1, range2, n_grid = 25L,
                       ref, target, wtp = 100000,
                       metric = c("INB", "ICER")) {
  metric <- match.arg(metric)

  grid1 <- seq(range1[1], range1[2], length.out = n_grid)
  grid2 <- seq(range2[1], range2[2], length.out = n_grid)
  grid <- expand.grid(p1 = grid1, p2 = grid2)

  metric_fun <- if (metric == "INB") {
    function(ref_res, tgt_res) {
      wtp * (tgt_res$qaly - ref_res$qaly) - (tgt_res$cost - ref_res$cost)
    }
  } else {
    function(ref_res, tgt_res) {
      dq <- tgt_res$qaly - ref_res$qaly
      dc <- tgt_res$cost - ref_res$cost
      if (dq > 0) dc / dq else NA_real_
    }
  }

  values <- vapply(seq_len(nrow(grid)), function(i) {
    pars <- base_pars
    pars[[param1]] <- grid$p1[i]
    pars[[param2]] <- grid$p2[i]

    ref_res <- suppressMessages(suppressWarnings(
      tryCatch(model$simulate(pars, ref), error = function(e) NULL)
    ))
    tgt_res <- suppressMessages(suppressWarnings(
      tryCatch(model$simulate(pars, target), error = function(e) NULL)
    ))

    if (is.null(ref_res) || is.null(tgt_res)) return(NA_real_)
    if (!is.finite(ref_res$cost) || !is.finite(ref_res$qaly)) return(NA_real_)
    if (!is.finite(tgt_res$cost) || !is.finite(tgt_res$qaly)) return(NA_real_)

    suppressWarnings(as.numeric(metric_fun(ref_res, tgt_res)))
  }, numeric(1))

  result <- data.frame(
    param1 = grid$p1, param2 = grid$p2, value = values,
    stringsAsFactors = FALSE
  )
  names(result)[1:2] <- c(param1, param2)

  list(
    grid = result,
    param1 = param1, param2 = param2,
    # Store the true base-case values so plots can mark the actual base point
    # (not the grid midpoint).
    base1 = base_pars[[param1]], base2 = base_pars[[param2]],
    ref = ref, target = target,
    metric = metric, wtp = wtp
  )
}

#' Plot Two-Way DSA Heatmap
#'
#' Renders a filled contour (heatmap) of the two-way DSA grid. For INB, a
#' contour line at INB = 0 marks the decision boundary. For ICER, the contour
#' is at ICER = WTP.
#'
#' @param twoway_result Output from [twoway_dsa()].
#' @param show_contour Logical; show the decision boundary contour?
#' @param palette Optional palette from [cea_palette()].
#' @return A ggplot object.
#' @export
plot_twoway_dsa <- function(twoway_result, show_contour = TRUE, palette = NULL) {
  if (is.null(palette)) palette <- cea_palette("classic")

  df <- twoway_result$grid
  p1_name <- twoway_result$param1
  p2_name <- twoway_result$param2
  metric <- twoway_result$metric
  ref <- twoway_result$ref
  target <- twoway_result$target

  if (metric == "INB") {
    fill_label <- paste0("INB (", target, " vs ", ref, ")")
    contour_level <- 0
    subtitle <- "Cost-effective where INB > 0; dashed line = threshold; x = base case"
  } else {
    fill_label <- paste0("ICER (", target, " vs ", ref, ")")
    contour_level <- twoway_result$wtp
    subtitle <- paste0("Below WTP ($", formatC(twoway_result$wtp, big.mark = ","),
                       ") = cost-effective")
    # Cap extreme ICERs for visual clarity
    cap <- twoway_result$wtp * 3
    df$value <- ifelse(is.finite(df$value), pmin(df$value, cap), NA_real_)
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[p1_name]],
                                         y = .data[[p2_name]],
                                         fill = .data$value)) +
    ggplot2::geom_tile() +
    ggplot2::labs(
      x = p1_name, y = p2_name, fill = fill_label,
      title = paste0("Two-Way Sensitivity: ", p1_name, " vs ", p2_name),
      subtitle = subtitle
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "right")

  if (metric == "INB") {
    p <- p + ggplot2::scale_fill_gradient2(
      low = palette$dominated, mid = "white", high = palette$primary,
      midpoint = 0, na.value = "#EEEEEE"
    )
  } else {
    p <- p + ggplot2::scale_fill_gradient2(
      low = palette$primary, mid = "white", high = palette$dominated,
      midpoint = contour_level, na.value = "#EEEEEE"
    )
  }

  if (show_contour) {
    df_clean <- df[is.finite(df$value), ]
    if (nrow(df_clean) > 4) {
      p <- p + ggplot2::geom_contour(
        data = df_clean,
        ggplot2::aes(z = .data$value),
        breaks = contour_level,
        color = "black", linewidth = 1, linetype = "dashed"
      )
    }
  }

  # Mark the TRUE base case (stored by twoway_dsa). Fall back to the grid
  # midpoint only for older result objects that lack base1/base2.
  base_x <- twoway_result$base1
  base_y <- twoway_result$base2
  if (is.null(base_x)) base_x <- df[[p1_name]][ceiling(nrow(df) / 2)]
  if (is.null(base_y)) base_y <- df[[p2_name]][ceiling(nrow(df) / 2)]
  p <- p + ggplot2::annotate(
    "point", x = base_x,
    y = base_y,
    shape = 4, size = 3, color = "black", stroke = 1.5
  )

  p
}
