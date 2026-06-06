#' Compute Expected Value of Perfect Information (EVPI)
#'
#' Calculates EVPI across a grid of WTP thresholds from PSA results.
#' EVPI equals the expected maximum NMB with perfect information minus
#' the maximum expected NMB with current information.
#'
#' @param psa_results Data.frame from [run_psa()]$results.
#' @param strategies Character vector of strategy names.
#' @param wtp_max Maximum WTP for the grid.
#' @param wtp_step Step size for the WTP grid.
#' @return A data.frame with columns WTP and EVPI.
#' @export
compute_evpi <- function(psa_results, strategies, wtp_max = 200000, wtp_step = 2000) {
  wtp_grid <- seq(0, wtp_max, by = wtp_step)
  n_sim <- nrow(psa_results)

  evpi_vec <- vapply(wtp_grid, function(w) {
    nmb <- vapply(strategies, function(s) {
      w * psa_results[[paste0(s, ".q")]] - psa_results[[paste0(s, ".cost")]]
    }, numeric(n_sim))

    if (!is.matrix(nmb)) nmb <- matrix(nmb, nrow = 1)

    perfect_info <- mean(apply(nmb, 1, max))
    current_info <- max(colMeans(nmb))
    max(0, perfect_info - current_info)
  }, numeric(1))

  data.frame(WTP = wtp_grid, EVPI = evpi_vec, stringsAsFactors = FALSE)
}

#' Compute Expected Value of Partial Perfect Information (EVPPI)
#'
#' Estimates per-parameter EVPPI using a non-parametric GAM regression
#' approach (Strong et al. 2014). For each parameter, fits a GAM of
#' NMB on the parameter draws and computes the reduction in decision
#' uncertainty.
#'
#' @param psa_results Data.frame from [run_psa()]$results.
#' @param psa_draws Data.frame from [run_psa()]$draws (parameter draws).
#' @param strategies Character vector of strategy names.
#' @param wtp Scalar WTP threshold.
#' @param param_names Optional subset of parameter names to evaluate.
#'   Defaults to all columns in psa_draws.
#' @return A data.frame with columns Parameter and EVPPI, sorted by EVPPI descending.
#' @export
compute_evppi <- function(psa_results, psa_draws, strategies, wtp = 100000,
                          param_names = NULL) {
  if (is.null(psa_draws) || ncol(psa_draws) == 0) {
    return(data.frame(Parameter = character(0), EVPPI = numeric(0),
                      stringsAsFactors = FALSE))
  }

  if (is.null(param_names)) param_names <- colnames(psa_draws)
  n_sim <- nrow(psa_results)

  nmb_mat <- vapply(strategies, function(s) {
    wtp * psa_results[[paste0(s, ".q")]] - psa_results[[paste0(s, ".cost")]]
  }, numeric(n_sim))

  if (!is.matrix(nmb_mat)) nmb_mat <- matrix(nmb_mat, nrow = 1)

  current_info <- max(colMeans(nmb_mat))

  evppi_vec <- vapply(param_names, function(pname) {
    x <- psa_draws[[pname]]
    if (is.null(x) || all(is.na(x)) || stats::sd(x, na.rm = TRUE) < 1e-12) {
      return(0)
    }

    tryCatch({
      conditional_means <- vapply(seq_len(ncol(nmb_mat)), function(j) {
        fit <- stats::loess(nmb_mat[, j] ~ x,
                            span = min(0.75, max(0.3, 10 / sqrt(n_sim))))
        stats::predict(fit)
      }, numeric(n_sim))

      if (!is.matrix(conditional_means)) {
        conditional_means <- matrix(conditional_means, nrow = n_sim)
      }

      perfect_param <- mean(apply(conditional_means, 1, max, na.rm = TRUE),
                            na.rm = TRUE)
      max(0, perfect_param - current_info)
    }, error = function(e) 0)
  }, numeric(1))

  out <- data.frame(Parameter = param_names, EVPPI = evppi_vec,
                    stringsAsFactors = FALSE)
  out[order(-out$EVPPI), ]
}

#' Compute Expected Loss by Strategy
#'
#' For each strategy at each WTP, the expected loss is the difference between
#' that strategy's expected NMB and the maximum expected NMB across strategies
#' with perfect information. The optimal strategy at a given WTP is the one
#' with the lowest expected loss. Unlike CEAC, expected loss reflects the
#' magnitude of the wrong decision, not just its probability.
#'
#' @param psa_results Data.frame from [run_psa()]$results.
#' @param strategies Character vector of strategy names.
#' @param wtp_max Maximum WTP for the grid.
#' @param wtp_step Step size for the WTP grid.
#' @return A data.frame with columns WTP, Strategy, and Expected_Loss.
#' @export
compute_expected_loss <- function(psa_results, strategies,
                                  wtp_max = 200000, wtp_step = 2000) {
  wtp_grid <- seq(0, wtp_max, by = wtp_step)
  n_sim <- nrow(psa_results)

  results <- lapply(wtp_grid, function(w) {
    # NMB matrix: rows = simulations, cols = strategies
    nmb <- vapply(strategies, function(s) {
      w * psa_results[[paste0(s, ".q")]] - psa_results[[paste0(s, ".cost")]]
    }, numeric(n_sim))

    if (!is.matrix(nmb)) nmb <- matrix(nmb, nrow = 1)

    # Per-iteration maximum NMB (perfect information)
    max_nmb <- apply(nmb, 1, max)

    # Expected loss per strategy = E[max_NMB - NMB_s]
    el <- vapply(seq_along(strategies), function(j) {
      mean(max_nmb - nmb[, j])
    }, numeric(1))

    data.frame(WTP = w, Strategy = strategies, Expected_Loss = el,
               stringsAsFactors = FALSE)
  })

  do.call(rbind, results)
}

#' Plot Expected Loss
#'
#' Plots the expected opportunity loss for each strategy across WTP thresholds.
#' The optimal strategy at each WTP is the one closest to zero. Recommended as
#' a complement or alternative to the CEAC (Phelps & Mushlin, Hunink et al.).
#'
#' @param el_df Data.frame from [compute_expected_loss()] with columns
#'   WTP, Strategy, Expected_Loss.
#' @param palette Optional palette from [cea_palette()].
#' @return A ggplot object.
#' @export
plot_expected_loss <- function(el_df, palette = NULL) {
  if (is.null(palette)) palette <- cea_palette("classic")

  ggplot2::ggplot(el_df, ggplot2::aes(x = .data$WTP, y = .data$Expected_Loss,
                                       color = .data$Strategy)) +
    ggplot2::geom_line(linewidth = 0.9) +
    scale_color_cea(palette) +
    ggplot2::labs(x = "Willingness-to-pay ($/QALY)",
                  y = "Expected Loss ($)",
                  title = "Expected Opportunity Loss by Strategy",
                  color = "Strategy") +
    ggplot2::scale_y_continuous(labels = function(x) {
      ifelse(x >= 1000,
             paste0("$", formatC(x / 1000, format = "f", digits = 1), "k"),
             paste0("$", formatC(x, format = "f", digits = 0)))
    }) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")
}

#' Plot EVPI Across WTP Thresholds
#'
#' @param evpi_df Data.frame from [compute_evpi()] with columns WTP and EVPI.
#' @param palette Optional palette from [cea_palette()].
#' @return A ggplot object.
#' @export
plot_evpi <- function(evpi_df, palette = NULL) {
  if (is.null(palette)) palette <- cea_palette("classic")

  ggplot2::ggplot(evpi_df, ggplot2::aes(x = .data$WTP, y = .data$EVPI)) +
    ggplot2::geom_line(linewidth = 1, color = palette$primary) +
    ggplot2::geom_area(alpha = 0.15, fill = palette$primary) +
    ggplot2::labs(x = "Willingness-to-pay ($/QALY)",
                  y = "EVPI ($)",
                  title = "Expected Value of Perfect Information") +
    ggplot2::scale_y_continuous(labels = function(x) {
      ifelse(x >= 1000, paste0("$", formatC(x / 1000, format = "f", digits = 0), "k"),
             paste0("$", formatC(x, format = "f", digits = 0)))
    }) +
    ggplot2::theme_minimal(base_size = 12)
}

#' Plot EVPPI Bar Chart
#'
#' @param evppi_df Data.frame from [compute_evppi()] with columns Parameter and EVPPI.
#' @param top_n Show only the top N parameters. Default 15.
#' @param palette Optional palette from [cea_palette()].
#' @return A ggplot object.
#' @export
plot_evppi <- function(evppi_df, top_n = 15, palette = NULL) {
  if (is.null(palette)) palette <- cea_palette("classic")

  df <- utils::head(evppi_df[order(-evppi_df$EVPPI), ], top_n)
  df <- df[df$EVPPI > 0, ]
  if (nrow(df) == 0) {
    return(ggplot2::ggplot() +
             ggplot2::annotate("text", x = 0.5, y = 0.5,
                               label = "No parameters with EVPPI > 0",
                               size = 5, color = "#999999") +
             ggplot2::theme_void())
  }

  df$Parameter <- factor(df$Parameter, levels = rev(df$Parameter))

  ggplot2::ggplot(df, ggplot2::aes(x = .data$EVPPI, y = .data$Parameter)) +
    ggplot2::geom_col(fill = palette$primary, alpha = 0.8, width = 0.6) +
    ggplot2::labs(x = "EVPPI ($)", y = NULL,
                  title = "Expected Value of Partial Perfect Information") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}
