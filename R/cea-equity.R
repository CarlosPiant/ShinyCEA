#' Run Distributional Cost-Effectiveness Analysis
#'
#' Runs the model across subgroups defined by parameter overrides, population
#' shares, and baseline health levels. Computes per-subgroup and
#' population-weighted outcomes for each strategy, enabling equity analysis.
#'
#' @param model A `cea_model` object.
#' @param base_params Base-case parameter data.frame.
#' @param subgroups A named list of subgroups. Each element is a list with:
#'   \describe{
#'     \item{overrides}{Named list of parameter overrides}
#'     \item{share}{Population share (all shares must sum to 1)}
#'     \item{baseline_qaly}{Baseline (no-treatment) per-capita health for equity weighting}
#'   }
#' @param wtp WTP threshold.
#' @return A list with:
#'   \describe{
#'     \item{by_subgroup}{Data.frame with Subgroup, Strategy, Cost, QALY, NMB,
#'       Share, Baseline_QALY}
#'     \item{population}{Data.frame with Strategy, Cost, QALY, NMB
#'       (population-weighted)}
#'     \item{strategies}{Character vector of strategy names}
#'     \item{wtp}{WTP used}
#'   }
#' @export
run_distributional_cea <- function(model, base_params, subgroups, wtp = 100000) {
  if (!is.list(subgroups) || is.null(names(subgroups))) {
    stop("subgroups must be a named list.", call. = FALSE)
  }

  # Validate shares sum to 1
  shares <- vapply(subgroups, function(sg) {
    sh <- sg$share
    if (is.null(sh) || !is.finite(sh)) stop("Each subgroup must have a numeric 'share'.", call. = FALSE)
    sh
  }, numeric(1))
  if (abs(sum(shares) - 1) > 0.01) {
    stop("Subgroup shares must sum to 1 (got ", round(sum(shares), 4), ").", call. = FALSE)
  }

  strategies <- model$info()$strategies
  pars_base <- stats::setNames(as.list(base_params$base), base_params$name)

  rows <- list()
  for (sg_name in names(subgroups)) {
    sg <- subgroups[[sg_name]]
    overrides <- if (!is.null(sg$overrides)) sg$overrides else list()
    share <- sg$share
    bl_qaly <- if (!is.null(sg$baseline_qaly)) sg$baseline_qaly else NA_real_

    # Apply overrides
    params_df <- base_params
    for (pname in names(overrides)) {
      idx <- which(params_df$name == pname)
      if (length(idx) == 1L) {
        params_df$base[idx] <- overrides[[pname]]
        params_df$mean[idx] <- overrides[[pname]]
      }
    }
    pars <- stats::setNames(as.list(params_df$base), params_df$name)

    for (s in strategies) {
      res <- safe_simulate(model, pars, s)
      if (is.null(res)) {
        rows[[length(rows) + 1L]] <- data.frame(
          Subgroup = sg_name, Strategy = s,
          Cost = NA_real_, QALY = NA_real_, NMB = NA_real_,
          Share = share, Baseline_QALY = bl_qaly,
          stringsAsFactors = FALSE
        )
      } else {
        rows[[length(rows) + 1L]] <- data.frame(
          Subgroup = sg_name, Strategy = s,
          Cost = res$cost, QALY = res$qaly,
          NMB = wtp * res$qaly - res$cost,
          Share = share, Baseline_QALY = bl_qaly,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  by_subgroup <- do.call(rbind, rows)

  # Population-weighted averages
  pop_rows <- lapply(strategies, function(s) {
    sub <- by_subgroup[by_subgroup$Strategy == s, ]
    data.frame(
      Strategy = s,
      Cost = sum(sub$Cost * sub$Share, na.rm = TRUE),
      QALY = sum(sub$QALY * sub$Share, na.rm = TRUE),
      NMB = sum(sub$NMB * sub$Share, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  population <- do.call(rbind, pop_rows)

  list(
    by_subgroup = by_subgroup,
    subgroups = subgroups,
    population = population,
    strategies = strategies,
    wtp = wtp
  )
}

#' Calculate Equity-Weighted Net Monetary Benefit
#'
#' Applies equity weights to subgroup-level QALYs using a social welfare
#' function. Three weighting schemes are available:
#' \describe{
#'   \item{atkinson}{Atkinson SWF with inequality aversion parameter epsilon.
#'     epsilon=0 is utilitarian (standard CEA); as epsilon increases, more
#'     weight goes to worse-off groups (Cookson et al., 2017).}
#'   \item{proportional}{Weight inversely proportional to baseline health:
#'     worse-off groups get higher weight.}
#'   \item{custom}{User supplies a named numeric vector of weights per subgroup.}
#' }
#'
#' @param dcea_result Output from [run_distributional_cea()].
#' @param method One of "atkinson", "proportional", or "custom".
#' @param epsilon Inequality aversion parameter for Atkinson (default 1).
#'   0 = utilitarian, higher = more equity-focused.
#' @param custom_weights Named numeric vector of equity weights per subgroup
#'   (only used when method = "custom").
#' @return A data.frame with Strategy, Equity_Weighted_QALY, Equity_Weighted_NMB,
#'   and the standard (unweighted) values for comparison.
#' @export
calculate_equity_weighted_nmb <- function(dcea_result,
                                           method = c("atkinson", "proportional", "custom"),
                                           epsilon = 1,
                                           custom_weights = NULL) {
  method <- match.arg(method)
  df <- dcea_result$by_subgroup
  wtp <- dcea_result$wtp
  strategies <- dcea_result$strategies

  subgroups <- unique(df$Subgroup)
  shares <- df$Share[match(subgroups, df$Subgroup)]
  baselines <- df$Baseline_QALY[match(subgroups, df$Subgroup)]

  # Compute equity weights per subgroup
  eq_weights <- switch(method,
    "atkinson" = {
      if (any(is.na(baselines))) {
        warning("Baseline QALYs missing; using equal weights.", call. = FALSE)
        rep(1, length(subgroups))
      } else {
        # Atkinson marginal weight: dW/dh = h^(-epsilon)
        baselines^(-epsilon)
      }
    },
    "proportional" = {
      if (any(is.na(baselines))) {
        warning("Baseline QALYs missing; using equal weights.", call. = FALSE)
        rep(1, length(subgroups))
      } else {
        # Weight inversely proportional to baseline
        max_bl <- max(baselines, na.rm = TRUE)
        (max_bl + 0.01) / (baselines + 0.01)
      }
    },
    "custom" = {
      if (is.null(custom_weights)) {
        stop("custom_weights required when method='custom'.", call. = FALSE)
      }
      vapply(subgroups, function(sg) {
        if (sg %in% names(custom_weights)) custom_weights[[sg]] else 1
      }, numeric(1))
    }
  )

  # Normalize weights so population-weighted sum = 1
  norm_factor <- sum(shares * eq_weights)
  eq_weights_norm <- eq_weights / norm_factor

  names(eq_weights_norm) <- subgroups

  result <- lapply(strategies, function(s) {
    sub <- df[df$Strategy == s, ]
    std_qaly <- sum(sub$QALY * sub$Share, na.rm = TRUE)
    std_nmb <- sum(sub$NMB * sub$Share, na.rm = TRUE)

    eq_qaly <- sum(sub$QALY * sub$Share * eq_weights_norm[sub$Subgroup], na.rm = TRUE)
    eq_nmb <- sum((wtp * sub$QALY - sub$Cost) * sub$Share *
                    eq_weights_norm[sub$Subgroup], na.rm = TRUE)

    data.frame(
      Strategy = s,
      QALY = std_qaly, NMB = std_nmb,
      Equity_Weighted_QALY = eq_qaly,
      Equity_Weighted_NMB = eq_nmb,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, result)
  attr(out, "weights") <- eq_weights_norm
  attr(out, "method") <- method
  attr(out, "epsilon") <- epsilon
  out
}

#' Plot NMB by Subgroup
#'
#' Grouped bar chart showing NMB per subgroup per strategy.
#'
#' @param dcea_result Output from [run_distributional_cea()].
#' @param metric One of "NMB", "QALY", or "Cost".
#' @param palette Optional palette from [cea_palette()].
#' @return A ggplot object.
#' @export
plot_nmb_by_subgroup <- function(dcea_result, metric = c("NMB", "QALY", "Cost"),
                                  palette = NULL) {
  if (is.null(palette)) palette <- cea_palette("cream")
  metric <- match.arg(metric)

  df <- dcea_result$by_subgroup
  pop <- dcea_result$population

  y_col <- metric
  y_label <- switch(metric,
    "NMB" = "Net Monetary Benefit ($)",
    "QALY" = "QALYs",
    "Cost" = "Cost ($)"
  )

  pop_avg <- stats::setNames(pop[[metric]], pop$Strategy)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$Subgroup,
                                         y = .data[[y_col]],
                                         fill = .data$Strategy)) +
    ggplot2::geom_col(position = "dodge", width = 0.7, alpha = 0.85) +
    scale_fill_cea(palette) +
    ggplot2::labs(x = NULL, y = y_label,
                  title = paste0(metric, " by Subgroup and Strategy"),
                  fill = "Strategy") +
    theme_cea() +
    ggplot2::theme(legend.position = "bottom",
                   axis.text.x = ggplot2::element_text(angle = 25, hjust = 1))

  p
}

#' Plot Health Distribution Across Subgroups
#'
#' Visualizes how QALYs (or health shortfall) are distributed across
#' subgroups under each strategy. Includes a Lorenz-curve-style
#' visualization when ordered by baseline health.
#'
#' @param dcea_result Output from [run_distributional_cea()].
#' @param strategy Strategy to plot (single string).
#' @param show_equal Logical; show the "equal distribution" reference line?
#' @param palette Optional palette from [cea_palette()].
#' @return A ggplot object.
#' @export
plot_health_distribution <- function(dcea_result, strategy = NULL,
                                     show_equal = TRUE, palette = NULL) {
  if (is.null(palette)) palette <- cea_palette("cream")

  df <- dcea_result$by_subgroup
  if (is.null(strategy)) strategy <- dcea_result$strategies[1]
  df <- df[df$Strategy == strategy, ]

  # Order by baseline QALY (poorest health first)
  if (!all(is.na(df$Baseline_QALY))) {
    df <- df[order(df$Baseline_QALY), ]
  }

  # Compute cumulative population share and cumulative QALY share
  df$cum_pop <- cumsum(df$Share)
  total_qaly <- sum(df$QALY * df$Share, na.rm = TRUE)
  df$qaly_share <- (df$QALY * df$Share) / total_qaly
  df$cum_qaly <- cumsum(df$qaly_share)

  # Lorenz curve data
  lorenz <- rbind(
    data.frame(cum_pop = 0, cum_qaly = 0, stringsAsFactors = FALSE),
    df[, c("cum_pop", "cum_qaly")]
  )

  p <- ggplot2::ggplot(lorenz, ggplot2::aes(x = .data$cum_pop,
                                              y = .data$cum_qaly)) +
    ggplot2::geom_line(color = palette$primary, linewidth = 1.2) +
    ggplot2::geom_point(color = palette$primary, size = 3)

  if (show_equal) {
    p <- p + ggplot2::geom_abline(slope = 1, intercept = 0,
                                   linetype = "dashed", color = "#999999")
  }

  p <- p +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$cum_qaly, ymax = .data$cum_pop),
                          fill = palette$dominated, alpha = 0.15) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(round(x * 100), "%"),
                                limits = c(0, 1)) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(round(x * 100), "%"),
                                limits = c(0, 1)) +
    ggplot2::labs(
      x = "Cumulative population share (ordered by baseline health)",
      y = "Cumulative QALY share",
      title = paste0("Health Distribution: ", strategy),
      subtitle = "Lorenz curve (area between = inequality)"
    ) +
    theme_cea()

  p
}

#' Calculate Health Concentration Index
#'
#' Computes the concentration index measuring the degree to which health
#' benefits are concentrated among advantaged or disadvantaged subgroups.
#' C = 0 is perfectly equal; C < 0 is pro-poor (benefits go to worse-off);
#' C > 0 is pro-rich (Wagstaff, 2002; O'Donnell et al., 2008).
#'
#' @param dcea_result Output from [run_distributional_cea()].
#' @param strategy Strategy to evaluate (single string).
#' @param rank_by What to rank subgroups by: "baseline" (baseline_qaly, default)
#'   or a parameter name from the subgroup overrides.
#' @return A list with `$index` (scalar), `$interpretation` (string), and
#'   `$by_subgroup` (data.frame with ranks and contributions).
#' @export
calculate_concentration_index <- function(dcea_result, strategy = NULL,
                                           rank_by = "baseline") {
  df <- dcea_result$by_subgroup
  if (is.null(strategy)) strategy <- dcea_result$strategies[1]
  df <- df[df$Strategy == strategy, ]

  # Rank by baseline health, or by the value of a subgroup override (D10).
  if (rank_by == "baseline") {
    if (all(is.na(df$Baseline_QALY))) {
      return(list(index = NA_real_,
                  interpretation = "Cannot compute: baseline QALY not provided.",
                  by_subgroup = df))
    }
    df <- df[order(df$Baseline_QALY), ]
  } else {
    defs <- dcea_result$subgroups
    key <- vapply(df$Subgroup, function(nm) {
      v <- defs[[nm]]$overrides[[rank_by]]
      if (is.null(v)) NA_real_ else as.numeric(v)
    }, numeric(1))
    if (all(is.na(key))) {
      stop("`rank_by` must be \"baseline\" or the name of a parameter overridden in the ",
           "subgroups; `", rank_by, "` is not overridden in any subgroup.", call. = FALSE)
    }
    df <- df[order(key), ]
  }

  n_groups <- nrow(df)
  shares <- df$Share
  qalys <- df$QALY

  # Fractional rank: midpoint of cumulative share range
  cum_shares <- cumsum(shares)
  R <- cum_shares - shares / 2

  # Mean health
  h_bar <- sum(shares * qalys, na.rm = TRUE)

  if (abs(h_bar) < 1e-12) {
    return(list(index = 0, interpretation = "Mean health is zero.", by_subgroup = df))
  }

  # Concentration index: C = (2 / h_bar) * sum(s_g * (R_g - 0.5) * h_g)
  C <- (2 / h_bar) * sum(shares * (R - 0.5) * qalys, na.rm = TRUE)

  interpretation <- if (abs(C) < 0.02) {
    "Approximately equal distribution (|C| < 0.02)."
  } else if (C < 0) {
    paste0("Pro-disadvantaged (C = ", round(C, 4),
           "): health benefits concentrated among worse-off groups.")
  } else {
    paste0("Pro-advantaged (C = ", round(C, 4),
           "): health benefits concentrated among better-off groups.")
  }

  df$Rank <- R
  df$Contribution <- shares * (R - 0.5) * qalys

  list(index = C, interpretation = interpretation, by_subgroup = df)
}

#' Plot Equity-Efficiency Frontier
#'
#' Plots strategies on a plane where x = total population NMB (efficiency)
#' and y = equity metric (e.g., equity-weighted NMB or negative concentration
#' index). Strategies in the northeast are both efficient and equitable.
#'
#' @param dcea_result Output from [run_distributional_cea()].
#' @param epsilon_values Numeric vector of inequality aversion values to
#'   plot for the Atkinson frontier. Default c(0, 0.5, 1, 2, 5).
#' @param palette Optional palette from [cea_palette()].
#' @return A ggplot object.
#' @export
equity_frontier <- function(dcea_result, epsilon_values = c(0, 0.5, 1, 2, 5),
                            palette = NULL) {
  if (is.null(palette)) palette <- cea_palette("cream")

  strategies <- dcea_result$strategies
  wtp <- dcea_result$wtp

  # For each epsilon, compute equity-weighted NMB per strategy
  rows <- list()
  for (eps in epsilon_values) {
    ew <- calculate_equity_weighted_nmb(dcea_result, method = "atkinson",
                                         epsilon = eps)
    for (i in seq_len(nrow(ew))) {
      rows[[length(rows) + 1L]] <- data.frame(
        Strategy = ew$Strategy[i],
        Epsilon = eps,
        Standard_NMB = ew$NMB[i],
        Equity_NMB = ew$Equity_Weighted_NMB[i],
        stringsAsFactors = FALSE
      )
    }
  }
  df <- do.call(rbind, rows)

  # Also compute concentration index per strategy
  ci_rows <- lapply(strategies, function(s) {
    ci <- calculate_concentration_index(dcea_result, strategy = s)
    data.frame(Strategy = s, C_Index = ci$index, stringsAsFactors = FALSE)
  })
  ci_df <- do.call(rbind, ci_rows)

  # Plot: x = standard NMB (eps=0), y = equity-weighted NMB at each epsilon
  df_wide <- df[df$Epsilon == 0, c("Strategy", "Standard_NMB")]
  names(df_wide)[2] <- "Efficiency"

  # For the y-axis, use the highest non-zero epsilon as the equity measure
  max_eps <- max(epsilon_values[epsilon_values > 0])
  df_equity <- df[df$Epsilon == max_eps, c("Strategy", "Equity_NMB")]
  names(df_equity)[2] <- "Equity"

  plot_df <- merge(df_wide, df_equity, by = "Strategy")
  plot_df <- merge(plot_df, ci_df, by = "Strategy")

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$Efficiency,
                                              y = .data$Equity)) +
    ggplot2::geom_point(ggplot2::aes(color = .data$Strategy), size = 4) +
    scale_color_cea(palette)

  if (requireNamespace("ggrepel", quietly = TRUE)) {
    p <- p + ggrepel::geom_text_repel(
      ggplot2::aes(label = .data$Strategy),
      size = 4, show.legend = FALSE, seed = 2025
    )
  } else {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = .data$Strategy),
      vjust = -0.8, size = 3.5, show.legend = FALSE
    )
  }

  p <- p +
    ggplot2::labs(
      x = paste0("Population NMB (efficiency, ", "epsilon", "=0)"),
      y = paste0("Equity-weighted NMB (", "epsilon", "=", max_eps, ")"),
      title = "Equity-Efficiency Frontier",
      subtitle = paste0("Northeast = more efficient AND more equitable | ",
                         "C-index shown as point label"),
      color = "Strategy"
    ) +
    theme_cea() +
    ggplot2::theme(legend.position = "bottom")

  p
}
