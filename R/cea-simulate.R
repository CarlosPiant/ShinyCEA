#' Safe Simulate Wrapper
#'
#' Runs model$simulate() with error handling, returning NULL on failure.
#'
#' @param model A `cea_model` object.
#' @param pars Named list of parameter values.
#' @param strategy Character string naming the strategy.
#' @return A list with cost, qaly, and optionally trace/survival, or NULL on failure.
#' @export
safe_simulate <- function(model, pars, strategy) {
  out <- tryCatch(
    suppressMessages(suppressWarnings(
      model$simulate(pars, strategy)
    )),
    error = function(e) NULL
  )
  if (is.null(out)) return(NULL)

  cost <- suppressWarnings(as.numeric(out$cost))
  qaly <- suppressWarnings(as.numeric(out$qaly))
  if (!is.finite(cost) || !is.finite(qaly)) return(NULL)

  keep <- list(cost = cost, qaly = qaly)
  if (!is.null(out$trace)) keep$trace <- out$trace
  if (!is.null(out$survival)) keep$survival <- out$survival
  keep
}

#' Run Deterministic Analysis
#'
#' @param model A `cea_model` object.
#' @param params_df Parameter data.frame (with 'name' and 'base' columns).
#' @param wtp Willingness-to-pay threshold.
#' @param ref_strategy Optional reference strategy. If NULL, auto-selects max NMB.
#' @return A list with results, table, reference, frontier analysis, etc.
#' @export
run_deterministic <- function(model, params_df, wtp = 100000, ref_strategy = NULL) {
  strategies <- model$info()$strategies
  par_list <- stats::setNames(as.list(params_df$base), params_df$name)

  res <- stats::setNames(vector("list", length(strategies)), strategies)
  for (s in strategies) {
    res[[s]] <- safe_simulate(model, par_list, s)
    if (is.null(res[[s]])) {
      stop("Simulation failed for strategy '", s, "'. Check parameters.", call. = FALSE)
    }
  }

  tab <- cea_table(res, wtp)

  ref <- if (!is.null(ref_strategy) && ref_strategy %in% strategies) {
    ref_strategy
  } else {
    tab$Strategy[which.max(tab$NMB)]
  }

  ref_cost <- res[[ref]]$cost
  ref_qaly <- res[[ref]]$qaly

  fa <- frontier_analysis(tab[, c("Strategy", "Cost", "QALY")])

  tab$IncCost_vsRef <- tab$Cost - ref_cost
  tab$IncQALY_vsRef <- tab$QALY - ref_qaly
  tab$ICER_vsRef <- ifelse(tab$IncQALY_vsRef > 0,
                           tab$IncCost_vsRef / tab$IncQALY_vsRef,
                           NA_real_)
  tab <- tab[order(-tab$NMB), ]

  list(
    res = res,
    table = tab,
    ref = ref,
    pars = par_list,
    frontier = fa$frontier,
    icers = fa$icers,
    frontier_status = fa$classified,
    strategies = strategies
  )
}

#' Run Probabilistic Sensitivity Analysis
#'
#' @param model A `cea_model` object.
#' @param params_df Parameter data.frame.
#' @param n_sim Number of PSA iterations.
#' @param progress_callback Optional function(i, n) called each iteration for progress reporting.
#' @return A list with `results` (data.frame of costs/qalys), `draws` (parameter draws), and `dropped` count.
#' @export
run_psa <- function(model, params_df, n_sim = 1000, progress_callback = NULL) {
  strategies <- model$info()$strategies
  col_names <- as.vector(rbind(paste0(strategies, ".cost"), paste0(strategies, ".q")))
  M <- matrix(NA_real_, nrow = n_sim, ncol = length(col_names),
              dimnames = list(NULL, col_names))

  include_vec <- params_df$include_psa
  include_vec[is.na(include_vec)] <- FALSE
  keep_params <- if (any(include_vec)) params_df$name[include_vec] else params_df$name

  draws_rows <- vector("list", n_sim)
  used <- 0L
  dropped <- 0L

  for (i in seq_len(n_sim)) {
    draw <- vapply(seq_len(nrow(params_df)),
                   function(k) sample_param_row(params_df[k, ]),
                   numeric(1))
    names(draw) <- params_df$name

    row_vec <- numeric(length(col_names))
    ok <- TRUE
    for (s in strategies) {
      out <- safe_simulate(model, as.list(draw), s)
      if (is.null(out)) { ok <- FALSE; break }
      row_vec[col_names == paste0(s, ".cost")] <- out$cost
      row_vec[col_names == paste0(s, ".q")] <- out$qaly
    }

    if (!ok || any(!is.finite(row_vec))) {
      dropped <- dropped + 1L
      next
    }

    used <- used + 1L
    M[used, ] <- row_vec

    if (length(keep_params)) {
      vals <- vapply(keep_params,
                     function(nm) suppressWarnings(as.numeric(draw[[nm]])),
                     numeric(1))
      draws_rows[[used]] <- vals
    }

    if (!is.null(progress_callback)) progress_callback(i, n_sim)
  }

  results <- if (used > 0) {
    as.data.frame(M[seq_len(used), , drop = FALSE], check.names = FALSE)
  } else {
    as.data.frame(matrix(numeric(0), nrow = 0, ncol = length(col_names),
                         dimnames = list(NULL, col_names)), check.names = FALSE)
  }

  draws <- if (used > 0 && length(keep_params)) {
    as.data.frame(do.call(rbind, draws_rows[seq_len(used)]), check.names = FALSE)
  } else {
    NULL
  }

  list(results = results, draws = draws, dropped = dropped, used = used)
}

#' Compute CEAC from PSA Results
#'
#' @param psa_results Data.frame from run_psa()$results.
#' @param strategies Character vector of strategy names.
#' @param wtp_max Maximum WTP for the grid.
#' @param wtp_step Step size for WTP grid.
#' @return A data.frame in long format with columns WTP, Strategy, ProbCE.
#' @export
compute_ceac <- function(psa_results, strategies, wtp_max = 200000, wtp_step = 5000) {
  wtp_grid <- seq(0, wtp_max, by = wtp_step)

  probs <- vapply(wtp_grid, function(w) {
    nmb <- vapply(strategies, function(s) {
      w * psa_results[[paste0(s, ".q")]] - psa_results[[paste0(s, ".cost")]]
    }, numeric(nrow(psa_results)))

    if (is.matrix(nmb)) {
      win <- apply(nmb, 1, which.max)
    } else {
      win <- which.max(nmb)
    }

    tab <- table(factor(win, levels = seq_along(strategies)))
    as.numeric(tab) / sum(tab)
  }, numeric(length(strategies)))

  probs_df <- as.data.frame(t(probs))
  colnames(probs_df) <- strategies
  probs_df$WTP <- wtp_grid

  long <- stats::reshape(
    probs_df,
    direction = "long",
    varying = strategies,
    v.names = "ProbCE",
    timevar = "Strategy",
    times = strategies,
    idvar = "WTP"
  )
  rownames(long) <- NULL
  long
}
