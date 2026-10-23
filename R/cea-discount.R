#' Compute Discount Weight Vector
#'
#' Returns a vector of discount weights for each cycle, suitable for
#' element-wise multiplication with per-cycle costs or QALYs.
#'
#' @param n_cycles Number of cycles (length of the returned vector).
#' @param rate Annual discount rate (e.g., 0.03 for 3%).
#' @param cycle_length Cycle length in years (e.g., 1 for annual, 1/12 for monthly).
#' @param first_cycle Integer; 0 = first weight at t=0 (start), 1 = first weight at t=1.
#'   Use 1 when discounting occupancy-based payoffs (trace rows 2..N).
#' @return A numeric vector of length `n_cycles`.
#' @export
#' @examples
#' # 10 annual cycles at 3%
#' cea_discount_weights(10, rate = 0.03, cycle_length = 1)
#'
#' # 120 monthly cycles at 3%
#' cea_discount_weights(120, rate = 0.03, cycle_length = 1/12)
cea_discount_weights <- function(n_cycles, rate = 0.03, cycle_length = 1,
                                 first_cycle = 1L) {
  if (!is.finite(rate) || rate < 0) rate <- 0
  if (rate == 0) return(rep(1, n_cycles))

  cycle_rate <- (1 + rate)^cycle_length - 1
  t_start <- seq(from = first_cycle, length.out = n_cycles)
  1 / (1 + cycle_rate)^t_start
}

# Rescale a per-cycle probability calibrated for a cycle of length `from` to
# a cycle of length `to`, assuming a constant rate within the cycle:
# p' = 1 - (1 - p)^(to / from). Used by the bundled models when the user
# overrides `cycle_length` (review finding E13).
.rescale_prob <- function(p, from, to) {
  if (!is.finite(from) || !is.finite(to) || from <= 0 || to <= 0) return(p)
  1 - (1 - pmin(pmax(p, 0), 1))^(to / from)
}

#' Apply Discounting to Per-Cycle Payoffs
#'
#' Multiplies a vector of per-cycle values by discount weights and returns
#' the sum. Convenience wrapper around [cea_discount_weights()].
#'
#' @param values Numeric vector of per-cycle payoffs (costs or QALYs).
#' @param rate Annual discount rate.
#' @param cycle_length Cycle length in years.
#' @return A single discounted total (scalar).
#' @export
cea_discounted_sum <- function(values, rate = 0.03, cycle_length = 1) {
  n <- length(values)
  if (n == 0) return(0)
  w <- cea_discount_weights(n, rate = rate, cycle_length = cycle_length)
  sum(values * w, na.rm = TRUE)
}
