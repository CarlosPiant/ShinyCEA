#' Fit a Regression Metamodel to PSA Output
#'
#' Fits a metamodel (linear, polynomial, or GAM) that approximates the
#' relationship between input parameters and an outcome (cost, QALY, NMB,
#' or NHB) from probabilistic sensitivity analysis draws.  The fitted
#' surface can then be evaluated cheaply with [predict.cea_metamodel()]
#' instead of re-running the simulation model.
#'
#' @param psa_results Data.frame from [run_psa()]$results.
#' @param psa_draws Data.frame from [run_psa()]$draws (one row per
#'   retained simulation, columns = sampled parameter values).
#' @param strategies Character vector of strategy names.
#' @param params Character vector of parameter names to include.
#'   Defaults to all columns in `psa_draws`.
#' @param outcome One of "cost", "eff" (QALY), "nmb", or "nhb".
#' @param wtp Willingness-to-pay threshold (required for nmb/nhb).
#' @param type Regression type: "linear", "poly", or "gam".
#' @param poly_order Polynomial degree when `type = "poly"` (default 2).
#' @param gam_k Basis dimension for GAM smooths (default -1 = auto).
#' @return A `cea_metamodel` object (a list with class attribute).
#' @export
#' @examples
#' m <- cea_sick_sicker()
#' set.seed(42)
#' psa <- run_psa(m, m$default_params(), n_sim = 200)
#' mm <- cea_metamodel(psa$results, psa$draws, m$info()$strategies,
#'                     outcome = "nmb", wtp = 100000)
#' print(mm)
cea_metamodel <- function(psa_results, psa_draws, strategies,
                          params = NULL, outcome = c("cost", "eff", "nmb", "nhb"),
                          wtp = NULL, type = c("linear", "poly", "gam"),
                          poly_order = 2L, gam_k = -1) {
  outcome <- match.arg(outcome)
  type    <- match.arg(type)

  if (outcome %in% c("nmb", "nhb") && is.null(wtp)) {
    stop("wtp is required for nmb/nhb outcomes.", call. = FALSE)
  }

  if (is.null(params)) params <- names(psa_draws)
  stopifnot(length(params) >= 1L, all(params %in% names(psa_draws)))

  # Build outcome matrix: one column per strategy
  y <- .mm_outcome_matrix(psa_results, strategies, outcome, wtp)
  x <- psa_draws[, params, drop = FALSE]

  # Fit one model per strategy

  mods <- stats::setNames(
    lapply(strategies, function(s) {
      .mm_fit(x, y[, s], type, poly_order, gam_k)
    }),
    strategies
  )

  # R-squared per strategy (goodness of fit)
  r2 <- vapply(strategies, function(s) {
    ss_res <- sum((y[, s] - stats::predict(mods[[s]], newdata = x))^2)
    ss_tot <- sum((y[, s] - mean(y[, s]))^2)
    if (ss_tot == 0) 1 else 1 - ss_res / ss_tot
  }, numeric(1))

  structure(
    list(mods = mods, strategies = strategies, params = params,
         outcome = outcome, wtp = wtp, type = type,
         poly_order = poly_order, gam_k = gam_k,
         r_squared = r2, x = x, y = y, n = nrow(x)),
    class = "cea_metamodel"
  )
}

# ---------- internal: extract outcome columns from PSA results ----------
.mm_outcome_matrix <- function(psa_results, strategies, outcome, wtp) {
  cost_cols <- paste0(strategies, ".cost")
  qaly_cols <- paste0(strategies, ".q")
  costs  <- as.matrix(psa_results[, cost_cols, drop = FALSE])
  qalys  <- as.matrix(psa_results[, qaly_cols, drop = FALSE])
  colnames(costs) <- colnames(qalys) <- strategies

  switch(outcome,
    cost = costs,
    eff  = qalys,
    nmb  = qalys * wtp - costs,
    nhb  = qalys - costs / wtp
  )
}

# ---------- internal: fit a single regression ----------
.mm_fit <- function(x, y, type, poly_order, gam_k) {
  df <- data.frame(y = y, x, check.names = FALSE)
  pnames <- names(x)

  if (type == "gam") {
    if (!requireNamespace("mgcv", quietly = TRUE)) {
      stop("Package 'mgcv' is required for GAM metamodels.", call. = FALSE)
    }
    # s() smooth for each parameter
    terms <- paste0("s(`", pnames, "`, k = ", gam_k, ")")
    fml <- stats::as.formula(paste("y ~", paste(terms, collapse = " + ")))
    mgcv::gam(fml, data = df)
  } else if (type == "poly") {
    # Polynomial of specified order for each parameter
    terms <- paste0("stats::poly(`", pnames, "`, ", poly_order, ")")
    fml <- stats::as.formula(paste("y ~", paste(terms, collapse = " + ")))
    stats::lm(fml, data = df)
  } else {
    # Linear regression
    terms <- paste0("`", pnames, "`")
    fml <- stats::as.formula(paste("y ~", paste(terms, collapse = " + ")))
    stats::lm(fml, data = df)
  }
}

#' Predict from a CEA Metamodel
#'
#' Generates predictions across a grid of parameter values.  For
#' one-parameter prediction, a sequence from min to max is used.
#' For two parameters, a grid of `nsamp^2` points is created.
#'
#' @param object A `cea_metamodel` from [cea_metamodel()].
#' @param params Character vector of 1 or 2 parameter names to vary.
#'   Others are held at their PSA mean.  Defaults to the first
#'   parameter in the metamodel.
#' @param ranges Optional named list of length-2 numeric vectors
#'   giving the range for each parameter.  Defaults to observed range.
#' @param nsamp Number of points per parameter (default 100).
#' @param ... Ignored.
#' @return A data.frame with columns for each varied parameter,
#'   the predicted outcome per strategy, and a "Strategy" column
#'   in long format when a single parameter is varied.
#' @export
predict.cea_metamodel <- function(object, params = NULL,
                                  ranges = NULL, nsamp = 100L, ...) {
  if (is.null(params)) params <- object$params[1]
  stopifnot(length(params) %in% 1:2, all(params %in% object$params))

  x_mean <- as.data.frame(lapply(object$x, mean))

  # Build prediction grid
  seqs <- lapply(params, function(p) {
    rng <- if (!is.null(ranges) && p %in% names(ranges)) {
      ranges[[p]]
    } else {
      range(object$x[[p]])
    }
    seq(rng[1], rng[2], length.out = nsamp)
  })
  names(seqs) <- params

  if (length(params) == 1L) {
    newdata <- x_mean[rep(1, nsamp), , drop = FALSE]
    newdata[[params]] <- seqs[[1]]

    preds <- stats::setNames(
      lapply(object$strategies, function(s) {
        as.numeric(stats::predict(object$mods[[s]], newdata = newdata))
      }),
      object$strategies
    )
    out <- data.frame(param_val = seqs[[1]])
    names(out) <- params
    for (s in object$strategies) out[[s]] <- preds[[s]]

    # Also return long format
    long <- do.call(rbind, lapply(object$strategies, function(s) {
      data.frame(param = seqs[[1]], outcome = preds[[s]],
                 Strategy = s, stringsAsFactors = FALSE)
    }))
    names(long)[1] <- params
    attr(out, "long") <- long
  } else {
    grid <- expand.grid(seqs)
    newdata <- x_mean[rep(1, nrow(grid)), , drop = FALSE]
    newdata[[params[1]]] <- grid[[1]]
    newdata[[params[2]]] <- grid[[2]]

    for (s in object$strategies) {
      grid[[s]] <- stats::predict(object$mods[[s]], newdata = newdata)
    }
    out <- grid
  }
  out
}

#' Plot a One-Way Metamodel Prediction
#'
#' @param mm A `cea_metamodel` object.
#' @param param Parameter name to vary (default: first).
#' @param range Optional length-2 numeric range.
#' @param nsamp Number of prediction points.
#' @param palette Color palette from [cea_palette()].
#' @return A ggplot2 object.
#' @export
plot_metamodel <- function(mm, param = NULL, range = NULL,
                           nsamp = 100L, palette = cea_palette()) {
  if (is.null(param)) param <- mm$params[1]
  ranges <- if (!is.null(range)) stats::setNames(list(range), param)
  pred <- stats::predict(mm, params = param, ranges = ranges, nsamp = nsamp)
  long <- attr(pred, "long")

  outcome_label <- switch(mm$outcome,
    cost = "Cost", eff = "QALY", nmb = "NMB", nhb = "NHB")

  ggplot2::ggplot(long, ggplot2::aes(
    x = .data[[param]], y = .data$outcome, color = .data$Strategy)) +
    ggplot2::geom_line(linewidth = 1) +
    scale_color_cea(palette) +
    ggplot2::labs(x = param, y = outcome_label,
                  title = paste("Metamodel:", outcome_label, "vs", param),
                  subtitle = paste0("Type: ", mm$type,
                                    " | R\u00b2 range: ",
                                    round(min(mm$r_squared), 3), " - ",
                                    round(max(mm$r_squared), 3))) +
    theme_cea()
}

#' Plot a Two-Way Metamodel Prediction (Contour/Heatmap)
#'
#' @param mm A `cea_metamodel` object.
#' @param params Character vector of length 2.
#' @param strategy Which strategy to plot.
#' @param nsamp Grid resolution per axis.
#' @param palette Color palette from [cea_palette()].
#' @return A ggplot2 object.
#' @export
plot_metamodel_twoway <- function(mm, params, strategy,
                                  nsamp = 50L, palette = cea_palette()) {
  stopifnot(length(params) == 2, strategy %in% mm$strategies)
  pred <- stats::predict(mm, params = params, nsamp = nsamp)

  ggplot2::ggplot(pred, ggplot2::aes(
    x = .data[[params[1]]], y = .data[[params[2]]],
    fill = .data[[strategy]])) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(
      low = palette$dominated, mid = "white", high = palette$primary,
      midpoint = stats::median(pred[[strategy]], na.rm = TRUE)) +
    ggplot2::labs(x = params[1], y = params[2],
                  fill = mm$outcome,
                  title = paste("Metamodel:", strategy),
                  subtitle = paste0(mm$outcome, " surface (", mm$type, ")")) +
    theme_cea()
}

#' @export
print.cea_metamodel <- function(x, ...) {
  cat("CEA Metamodel\n")
  cat("  Outcome:", x$outcome, "\n")
  cat("  Type:", x$type, "\n")
  cat("  Parameters:", length(x$params), "\n")
  cat("  Strategies:", paste(x$strategies, collapse = ", "), "\n")
  cat("  N (PSA draws):", x$n, "\n")
  cat("  R-squared:\n")
  for (s in x$strategies) {
    cat("    ", s, ":", round(x$r_squared[s], 4), "\n")
  }
  if (!is.null(x$wtp)) cat("  WTP:", x$wtp, "\n")
  invisible(x)
}
