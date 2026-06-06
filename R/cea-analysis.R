#' Build CEA Summary Table
#'
#' @param res_list Named list of simulation results (each with $cost, $qaly).
#' @param wtp Willingness-to-pay threshold.
#' @return A data.frame with Strategy, Cost, QALY, NMB.
#' @export
cea_table <- function(res_list, wtp) {
  df <- lapply(names(res_list), function(nm) {
    r <- res_list[[nm]]
    data.frame(
      Strategy = nm,
      Cost = as.numeric(r$cost),
      QALY = as.numeric(r$qaly),
      NMB = as.numeric(wtp) * as.numeric(r$qaly) - as.numeric(r$cost),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, df)
}

#' Frontier Analysis (Dominance and Extended Dominance)
#'
#' @param df_cq A data.frame with columns Strategy, Cost, QALY.
#' @return A list with frontier, icers, and classified data.frames.
#' @export
frontier_analysis <- function(df_cq) {
  stopifnot(all(c("Strategy", "Cost", "QALY") %in% names(df_cq)))

  d0 <- df_cq[!duplicated(df_cq$Strategy), ]
  d <- d0[order(d0$QALY, d0$Cost), ]

  # Strict dominance
  is_dom <- logical(nrow(d))
  for (i in seq_len(nrow(d))) {
    for (j in seq_len(nrow(d))) {
      if (j == i) next
      if (d$Cost[j] <= d$Cost[i] && d$QALY[j] >= d$QALY[i] &&
          (d$Cost[j] < d$Cost[i] || d$QALY[j] > d$QALY[i])) {
        is_dom[i] <- TRUE
        break
      }
    }
  }

  dominated_strats <- d$Strategy[is_dom]
  keep_df <- d[!is_dom, ]

  # Extended dominance via sequential ICER monotonicity
  ext_removed <- character(0)
  if (nrow(keep_df) >= 3) {
    repeat {
      dC <- diff(keep_df$Cost)
      dQ <- diff(keep_df$QALY)
      icer <- ifelse(dQ > 0, dC / dQ, Inf)
      bad <- which(icer[-length(icer)] >= icer[-1])
      if (length(bad) == 0) break
      rm_pos <- bad[1] + 1
      ext_removed <- c(ext_removed, keep_df$Strategy[rm_pos])
      keep_df <- keep_df[-rm_pos, , drop = FALSE]
      if (nrow(keep_df) < 3) break
    }
  }

  frontier_df <- keep_df

  # Sequential ICERs along frontier
  if (nrow(frontier_df) >= 2) {
    icers <- do.call(rbind, lapply(2:nrow(frontier_df), function(i) {
      dC <- frontier_df$Cost[i] - frontier_df$Cost[i - 1]
      dQ <- frontier_df$QALY[i] - frontier_df$QALY[i - 1]
      data.frame(
        Strategy = frontier_df$Strategy[i],
        dC = dC, dQ = dQ,
        ICER = if (dQ > 0) dC / dQ else NA_real_,
        stringsAsFactors = FALSE
      )
    }))
  } else {
    icers <- data.frame(Strategy = character(0), dC = numeric(0),
                        dQ = numeric(0), ICER = numeric(0),
                        stringsAsFactors = FALSE)
  }

  classified <- d0
  classified$Status <- ifelse(
    classified$Strategy %in% frontier_df$Strategy, "Frontier",
    ifelse(classified$Strategy %in% ext_removed, "Extendedly dominated",
           "Dominated")
  )

  list(frontier = frontier_df, icers = icers, classified = classified)
}

#' One-Way Threshold Analysis
#'
#' @param model A `cea_model` object.
#' @param base_pars Named list of base-case parameters.
#' @param param_name Name of the parameter to vary.
#' @param ref Reference strategy.
#' @param target Target strategy.
#' @param range_min Minimum parameter value.
#' @param range_max Maximum parameter value.
#' @param n_points Number of grid points.
#' @param wtp WTP threshold.
#' @param mode "icer" or "inb".
#' @return A list with grid (data.frame), threshold value, and message.
#' @export
threshold_analysis <- function(model, base_pars, param_name, ref, target,
                               range_min, range_max, n_points = 30,
                               wtp = 100000, mode = c("icer", "inb")) {
  mode <- match.arg(mode)
  grid <- seq(range_min, range_max, length.out = n_points)

  res_list <- lapply(grid, function(x) {
    pars <- base_pars
    pars[[param_name]] <- x
    ref_res <- safe_simulate(model, pars, ref)
    targ_res <- safe_simulate(model, pars, target)

    if (is.null(ref_res) || is.null(targ_res)) {
      return(data.frame(param_value = x, delta_cost = NA_real_, delta_qaly = NA_real_,
                        stringsAsFactors = FALSE))
    }

    data.frame(
      param_value = x,
      delta_cost = targ_res$cost - ref_res$cost,
      delta_qaly = targ_res$qaly - ref_res$qaly,
      stringsAsFactors = FALSE
    )
  })

  td <- do.call(rbind, res_list)

  # Compute y-series and find crossing
  if (mode == "icer") {
    y <- ifelse(is.finite(td$delta_cost) & is.finite(td$delta_qaly) & td$delta_qaly > 0,
                td$delta_cost / td$delta_qaly, NA_real_)
    target_level <- wtp
  } else {
    y <- ifelse(is.finite(td$delta_cost) & is.finite(td$delta_qaly),
                wtp * td$delta_qaly - td$delta_cost, NA_real_)
    target_level <- 0
  }

  td$y <- y
  td2 <- td[is.finite(td$y), ]
  td2 <- td2[order(td2$param_value), ]

  threshold <- NA_real_
  message <- NULL

  if (nrow(td2) >= 2) {
    diff_vals <- td2$y - target_level
    sgn <- sign(diff_vals)
    idx <- which(utils::head(sgn, -1) * utils::tail(sgn, -1) <= 0 &
                   !is.na(utils::head(sgn, -1)) & !is.na(utils::tail(sgn, -1)))

    if (length(idx) > 0) {
      i <- idx[1]
      x1 <- td2$param_value[i]; x2 <- td2$param_value[i + 1]
      y1 <- td2$y[i]; y2 <- td2$y[i + 1]
      if (all(is.finite(c(x1, x2, y1, y2))) && y2 != y1) {
        threshold <- x1 + (target_level - y1) * (x2 - x1) / (y2 - y1)
      }
    } else {
      if (all(diff_vals < 0, na.rm = TRUE)) {
        message <- if (mode == "icer") {
          "ICER is below WTP over the whole range."
        } else {
          "INB is negative over the whole range."
        }
      } else if (all(diff_vals > 0, na.rm = TRUE)) {
        message <- if (mode == "icer") {
          "ICER is above WTP over the whole range."
        } else {
          "INB is positive over the whole range."
        }
      } else {
        message <- "No clear crossing found in this range."
      }
    }
  } else {
    message <- "Not enough finite points to compute threshold."
  }

  list(grid = td, threshold = threshold, message = message)
}

#' Build Tornado Data (One-Way Sensitivity)
#'
#' @param params_df Parameter data.frame (filtered to include_tornado == TRUE).
#' @param base_pars Named list of base-case parameters.
#' @param model A `cea_model` object.
#' @param ref Reference strategy.
#' @param target Target strategy.
#' @param metric Either "INB" or "ICER".
#' @param wtp WTP threshold.
#' @return A data.frame with columns: name, bound, value, base. Attribute "diag" has diagnostics.
#' @export
build_tornado_data <- function(params_df, base_pars, model, ref, target,
                               metric = c("INB", "ICER"), wtp = 100000) {
  metric <- match.arg(metric)

  metric_fun <- if (metric == "INB") {
    function(ref_res, tgt_res) {
      (wtp * (tgt_res$qaly - ref_res$qaly)) - (tgt_res$cost - ref_res$cost)
    }
  } else {
    function(ref_res, tgt_res) {
      dQ <- tgt_res$qaly - ref_res$qaly
      dC <- tgt_res$cost - ref_res$cost
      if (dQ > 0) dC / dQ else NA_real_
    }
  }

  safe_metric <- function(pars) {
    tryCatch({
      ref_res <- model$simulate(pars, ref)
      tgt_res <- model$simulate(pars, target)
      suppressWarnings(as.numeric(metric_fun(ref_res, tgt_res)))
    }, error = function(e) NA_real_)
  }

  base_val <- safe_metric(base_pars)

  rows <- list()
  n_eligible <- 0L
  n_no_bounds <- 0L
  n_bad_eval <- 0L

  for (i in seq_len(nrow(params_df))) {
    nm <- params_df$name[i]
    lo <- suppressWarnings(as.numeric(params_df$lower[i]))
    hi <- suppressWarnings(as.numeric(params_df$upper[i]))

    if (!is.finite(lo) && !is.finite(hi)) {
      n_no_bounds <- n_no_bounds + 1L
      next
    }

    v_lo <- NA_real_
    if (is.finite(lo)) {
      p_lo <- base_pars; p_lo[[nm]] <- lo
      v_lo <- safe_metric(p_lo)
    }

    v_hi <- NA_real_
    if (is.finite(hi)) {
      p_hi <- base_pars; p_hi[[nm]] <- hi
      v_hi <- safe_metric(p_hi)
    }

    recs <- data.frame(
      name = nm,
      bound = c("Lower bound", "Upper bound"),
      value = c(v_lo, v_hi),
      base = base_val,
      stringsAsFactors = FALSE
    )

    keep <- is.finite(recs$value) & is.finite(recs$base)
    if (!any(keep)) {
      n_bad_eval <- n_bad_eval + 1L
      next
    }
    n_eligible <- n_eligible + 1L
    rows[[length(rows) + 1L]] <- recs[keep, , drop = FALSE]
  }

  tor <- do.call(rbind, rows)
  if (is.null(tor)) {
    tor <- data.frame(name = character(0), bound = character(0),
                      value = numeric(0), base = numeric(0),
                      stringsAsFactors = FALSE)
  }

  attr(tor, "diag") <- list(
    base_is_finite = is.finite(base_val),
    n_candidates = nrow(params_df),
    n_eligible = n_eligible,
    n_no_bounds = n_no_bounds,
    n_bad_eval = n_bad_eval
  )
  tor
}

#' Validate Parameters and Model (Health Checks)
#'
#' @param model A `cea_model` object.
#' @param df Parameter data.frame.
#' @param wtp WTP threshold.
#' @return A data.frame with columns: check, status, details, fix_hint.
#' @export
validate_params_and_model <- function(model, df, wtp = 100000) {
  rows <- list()
  add <- function(check, status, details = "", fix_hint = "") {
    rows[[length(rows) + 1L]] <<- data.frame(
      check = check, status = status, details = details, fix_hint = fix_hint,
      stringsAsFactors = FALSE
    )
  }

  if (is.null(df) || !nrow(df)) {
    add("Parameter table present", "FAIL", "No rows.", "Load a model or CSV.")
    return(do.call(rbind, rows))
  }
  add("Parameter table present", "PASS", sprintf("%d rows.", nrow(df)))

  required_cols <- c("name", "distribution", "mean", "lower", "upper",
                     "base", "include_psa", "include_tornado")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols)) {
    add("Required columns", "FAIL", paste("Missing:", paste(missing_cols, collapse = ", ")))
  } else {
    add("Required columns", "PASS")
  }

  bad_name <- is.na(df$name) | trimws(df$name) == ""
  if (any(bad_name, na.rm = TRUE)) {
    add("Parameter names", "FAIL", sprintf("Empty names: %d.", sum(bad_name, na.rm = TRUE)))
  } else if (any(duplicated(df$name))) {
    add("Parameter names", "WARN", "Duplicate names detected.")
  } else {
    add("Parameter names", "PASS")
  }

  reversed <- !is.na(df$lower) & !is.na(df$upper) & (df$lower > df$upper)
  if (any(reversed, na.rm = TRUE)) {
    add("Bounds order", "WARN", sprintf("Reversed in %d row(s).", sum(reversed, na.rm = TRUE)))
  } else {
    add("Bounds order", "PASS")
  }

  supported <- c("fixed", "none", "normal", "lognormal", "beta", "gamma",
                 "uniform", "triangular", "pert")
  bad_dist <- !(tolower(df$distribution) %in% supported)
  if (any(bad_dist, na.rm = TRUE)) {
    add("Distribution types", "WARN", sprintf("%d unsupported.", sum(bad_dist, na.rm = TRUE)))
  } else {
    add("Distribution types", "PASS")
  }

  pars <- stats::setNames(as.list(df$base), df$name)
  strategies <- tryCatch(model$info()$strategies, error = function(e) character(0))

  if (!length(strategies)) {
    add("Model strategies", "FAIL", "No strategies found.")
  } else {
    for (s in strategies) {
      out <- safe_simulate(model, pars, s)
      if (is.null(out)) {
        add(paste0("Simulate: ", s), "FAIL", "Failed or non-finite values.")
      } else {
        add(paste0("Simulate: ", s), "PASS")

        tr <- out$trace
        if (!is.null(tr) && is.matrix(tr) && nrow(tr) > 0) {
          rs <- rowSums(tr)
          if (any(abs(rs - 1) > 1e-4, na.rm = TRUE)) {
            add(paste0("Trace rows sum ~1: ", s), "WARN", "Some rows deviate from 1.")
          }
          dcol <- which(grepl("^d$|death|dead", tolower(colnames(tr))))
          if (length(dcol) >= 1) {
            dv <- tr[, dcol[1]]
            if (!all(diff(dv) >= -1e-8, na.rm = TRUE)) {
              add(paste0("Death monotone: ", s), "WARN", "Death not monotone non-decreasing.")
            }
          }
        }

        sv <- out$survival
        if (!is.null(sv) && is.data.frame(sv) && all(c("time", "surv") %in% names(sv))) {
          bad_sv <- !is.finite(sv$surv) | sv$surv < -1e-9 | sv$surv > 1 + 1e-9
          if (any(bad_sv, na.rm = TRUE)) {
            add(paste0("Survival [0,1]: ", s), "WARN", "Values outside [0,1].")
          }
        }
      }
    }
  }

  do.call(rbind, rows)
}
