# Survival and trace data from a deterministic run.
#
# These functions hold the data shaping behind the Survival and Traces tabs of
# the app. They live at package level (not inside the server) so that the same
# calls can be used in an analysis script, e.g. the code exported by the app.

# Cycle length in years, read from the parameter table. Models name it
# `cycle_length` (years) or `cycle_length_months`; without either, 1 year.
.cea_cycle_length <- function(params_df) {
  cl <- tryCatch({
    if (is.null(params_df)) {
      1
    } else if ("cycle_length" %in% params_df$name) {
      as.numeric(params_df$base[params_df$name == "cycle_length"][1])
    } else if ("cycle_length_months" %in% params_df$name) {
      # months -> years
      as.numeric(params_df$base[params_df$name == "cycle_length_months"][1]) / 12
    } else {
      1
    }
  }, error = function(e) 1)
  if (is.na(cl) || cl <= 0) 1 else cl
}

# Survival curves of one strategy: the model's own `survival` data frame when
# it returns one, otherwise overall survival derived from the death column of
# the trace.
.cea_extract_surv <- function(res, cycle_len) {
  empty <- data.frame(time = numeric(0), surv = numeric(0), curve = character(0),
                      stringsAsFactors = FALSE)
  if ("survival" %in% names(res) && is.data.frame(res$survival)) {
    df <- res$survival
    if (!all(c("time", "surv") %in% names(df))) return(empty)
    if (!"curve" %in% names(df)) df$curve <- "OS"
    return(df[, c("time", "surv", "curve")])
  }
  if ("trace" %in% names(res) && is.matrix(res$trace)) {
    dcol <- which(grepl("^d$|death|dead", tolower(colnames(res$trace))))
    if (length(dcol) > 0) {
      os <- 1 - res$trace[, dcol[1]]
      time <- seq(0, nrow(res$trace) - 1) * cycle_len
      return(data.frame(time = time, surv = as.numeric(os), curve = "OS",
                        stringsAsFactors = FALSE))
    }
  }
  empty
}

# State occupancy implied by survival curves, for models without a trace:
# Alive/Death from OS alone, or PF/Progressed/Death when PFS is also given.
.cea_trace_from_surv <- function(surv_df) {
  empty <- data.frame(time = numeric(0), state = character(0), prob = numeric(0),
                      stringsAsFactors = FALSE)
  if (!all(c("time", "surv", "curve") %in% names(surv_df))) return(empty)
  os <- surv_df[tolower(surv_df$curve) == "os", ]
  pfs <- surv_df[tolower(surv_df$curve) == "pfs", ]
  os <- os[order(os$time), ]
  if (nrow(os) == 0) return(empty)
  if (nrow(pfs) == 0) {
    death <- pmin(pmax(1 - os$surv, 0), 1)
    return(rbind(
      data.frame(time = os$time, state = "Alive", prob = os$surv, stringsAsFactors = FALSE),
      data.frame(time = os$time, state = "Death", prob = death, stringsAsFactors = FALSE)
    ))
  }
  pfs <- pfs[order(pfs$time), ]
  # PFS on the OS time grid, so the three states add up at every time point
  pfs_interp <- if (!identical(os$time, pfs$time)) {
    stats::approx(pfs$time, pfs$surv, xout = os$time, rule = 2, ties = "ordered")$y
  } else {
    pfs$surv
  }
  pf <- pmin(pmax(pfs_interp, 0), 1)
  prog <- pmin(pmax(os$surv - pfs_interp, 0), 1)
  death <- pmin(pmax(1 - os$surv, 0), 1)
  rbind(
    data.frame(time = os$time, state = "PF", prob = pf, stringsAsFactors = FALSE),
    data.frame(time = os$time, state = "Progressed", prob = prog, stringsAsFactors = FALSE),
    data.frame(time = os$time, state = "Death", prob = death, stringsAsFactors = FALSE)
  )
}

#' Survival Curves from a Deterministic Run
#'
#' Collects the survival curves of every strategy of a deterministic run into
#' one data frame, ready for [plot_survival()]. A model's own `survival` output
#' is used when present; otherwise overall survival is derived from the death
#' column of its cohort trace.
#'
#' @param det_result Result of [run_deterministic()].
#' @param params_df Optional parameter table. Used only to read the cycle
#'   length (`cycle_length` in years or `cycle_length_months`) when survival
#'   has to be derived from a trace; defaults to 1 year.
#' @return A data frame with columns `time`, `surv`, `curve`, `strategy`, or
#'   `NULL` when no strategy provides survival information.
#' @export
#' @examples
#' m <- cea_sick_sicker()
#' det <- run_deterministic(m, m$default_params())
#' head(cea_survival_data(det, m$default_params()))
cea_survival_data <- function(det_result, params_df = NULL) {
  cl <- .cea_cycle_length(params_df)
  out <- lapply(names(det_result$res), function(s) {
    df <- .cea_extract_surv(det_result$res[[s]], cl)
    if (nrow(df) == 0) return(NULL)
    df$strategy <- s
    df
  })
  do.call(rbind, out[!vapply(out, is.null, logical(1))])
}

#' State Occupancy (Cohort Trace) from a Deterministic Run
#'
#' Collects the cohort trace of every strategy of a deterministic run into one
#' long data frame, ready for [plot_trace()]. For models that return survival
#' curves instead of a trace, state occupancy is derived from the curves.
#'
#' @inheritParams cea_survival_data
#' @return A data frame with columns `strategy`, `time`, `state`, `prob`, or
#'   `NULL` when no strategy provides a trace or survival curves.
#' @export
#' @examples
#' m <- cea_sick_sicker()
#' det <- run_deterministic(m, m$default_params())
#' head(cea_trace_data(det, m$default_params()))
cea_trace_data <- function(det_result, params_df = NULL) {
  cl <- .cea_cycle_length(params_df)
  out <- lapply(names(det_result$res), function(s) {
    r <- det_result$res[[s]]
    if (!is.null(r$trace) && is.matrix(r$trace) && nrow(r$trace) > 0) {
      tr <- r$trace
      d <- as.data.frame(tr)
      d$time <- seq(0, nrow(tr) - 1) * cl
      # wide (one column per state) -> long (one row per time and state)
      d_long <- stats::reshape(d, direction = "long",
                               varying = setdiff(names(d), "time"),
                               v.names = "prob", timevar = "state",
                               times = setdiff(names(d), "time"),
                               idvar = "time")
      d_long$strategy <- s
      rownames(d_long) <- NULL
      return(d_long[, c("strategy", "time", "state", "prob")])
    }
    if (!is.null(r$survival) && is.data.frame(r$survival) && nrow(r$survival) > 0) {
      dt <- .cea_trace_from_surv(r$survival)
      if (nrow(dt) > 0) {
        dt$strategy <- s
        return(dt[, c("strategy", "time", "state", "prob")])
      }
    }
    NULL
  })
  do.call(rbind, out[!vapply(out, is.null, logical(1))])
}
