#' Sick-Sicker Example Model (4-State Markov)
#'
#' A classic 4-state cohort Markov model: Healthy -> Sick -> Sicker -> Death.
#' Includes 5 strategies (std + 4 treatments) with relative risk modifiers.
#'
#' @return A `cea_model` object.
#' @export
#' @examples
#' model <- cea_sick_sicker()
#' print(model)
cea_sick_sicker <- function() {
  info <- function() {
    list(
      key = "sick_sicker",
      name = "Sick-Sicker (4-state Markov)",
      strategies = c("std", "t1", "t2", "t3", "t4")
    )
  }

  params <- function() {
    do.call(rbind, list(
      cea_param("time_horizon", 10, 5, 15, distribution = "fixed", include_psa = FALSE, include_tornado = FALSE),
      cea_param("cycle_length", 0.25, 0.25, 0.5, distribution = "fixed", include_psa = FALSE, include_tornado = FALSE),
      cea_param("p_HS_std", 0.04, 0.02, 0.08, distribution = "beta"),
      cea_param("p_HD_std", 0.005, 0.002, 0.01, distribution = "beta"),
      cea_param("p_S1S2_std", 0.06, 0.03, 0.12, distribution = "beta"),
      cea_param("p_S1D_std", 0.02, 0.01, 0.04, distribution = "beta"),
      cea_param("p_S2D_std", 0.08, 0.04, 0.15, distribution = "beta"),
      cea_param("rr_prog_t1", 0.85, 0.70, 0.98, distribution = "lognormal"),
      cea_param("rr_prog_t2", 0.75, 0.60, 0.92, distribution = "lognormal"),
      cea_param("rr_prog_t3", 0.90, 0.75, 1.05, distribution = "lognormal"),
      cea_param("rr_prog_t4", 0.65, 0.50, 0.85, distribution = "lognormal"),
      cea_param("rr_death_t1", 0.95, 0.80, 1.05, distribution = "lognormal"),
      cea_param("rr_death_t2", 0.90, 0.75, 1.02, distribution = "lognormal"),
      cea_param("rr_death_t3", 1.00, 0.85, 1.10, distribution = "lognormal"),
      cea_param("rr_death_t4", 0.88, 0.70, 1.00, distribution = "lognormal"),
      cea_param("u_H", 0.90, 0.85, 0.95, distribution = "beta"),
      cea_param("u_S1", 0.75, 0.65, 0.85, distribution = "beta"),
      cea_param("u_S2", 0.55, 0.45, 0.70, distribution = "beta"),
      cea_param("c_H", 200, 100, 300, distribution = "gamma"),
      cea_param("c_S1", 800, 500, 1200, distribution = "gamma"),
      cea_param("c_S2", 2200, 1500, 3500, distribution = "gamma"),
      cea_param("drug_std", 0, 0, 0, distribution = "fixed", include_psa = FALSE),
      cea_param("drug_t1", 400, 200, 600, distribution = "gamma"),
      cea_param("drug_t2", 600, 300, 900, distribution = "gamma"),
      cea_param("drug_t3", 300, 150, 500, distribution = "gamma"),
      cea_param("drug_t4", 700, 400, 1000, distribution = "gamma"),
      cea_param("d_cost", 0.03, 0, 0.08, distribution = "fixed", include_psa = FALSE, include_tornado = TRUE),
      cea_param("d_qaly", 0.03, 0, 0.08, distribution = "fixed", include_psa = FALSE, include_tornado = TRUE)
    ))
  }

  simulate <- function(pars, strategy) {
    strategy <- match.arg(strategy, c("std", "t1", "t2", "t3", "t4"))
    .null_coalesce <- function(a, b) if (is.null(a)) b else a

    horizon <- as.numeric(.null_coalesce(pars[["time_horizon"]], 10))
    cl <- as.numeric(.null_coalesce(pars[["cycle_length"]], 0.25))
    n_cycles <- max(1L, as.integer(round(horizon / cl)))

    p_HS <- as.numeric(.null_coalesce(pars[["p_HS_std"]], 0.04))
    p_HD <- as.numeric(.null_coalesce(pars[["p_HD_std"]], 0.005))
    p_S1S2 <- as.numeric(.null_coalesce(pars[["p_S1S2_std"]], 0.06))
    p_S1D <- as.numeric(.null_coalesce(pars[["p_S1D_std"]], 0.02))
    p_S2D <- as.numeric(.null_coalesce(pars[["p_S2D_std"]], 0.08))

    if (strategy != "std") {
      rr_prog <- as.numeric(.null_coalesce(pars[[paste0("rr_prog_", strategy)]], 1))
      rr_death <- as.numeric(.null_coalesce(pars[[paste0("rr_death_", strategy)]], 1))
      p_HS <- pmin(0.999, p_HS * rr_prog)
      p_S1S2 <- pmin(0.999, p_S1S2 * rr_prog)
      p_S1D <- pmin(0.999, p_S1D * rr_death)
      p_S2D <- pmin(0.999, p_S2D * rr_death)
    }

    P <- rbind(
      c(pmax(0, 1 - p_HS - p_HD), p_HS, 0, p_HD),
      c(0, pmax(0, 1 - p_S1S2 - p_S1D), p_S1S2, p_S1D),
      c(0, 0, pmax(0, 1 - p_S2D), p_S2D),
      c(0, 0, 0, 1.0)
    )

    states <- c("H", "S1", "S2", "D")
    trace <- matrix(0, nrow = n_cycles + 1, ncol = 4, dimnames = list(NULL, states))
    trace[1, ] <- c(1, 0, 0, 0)
    for (t in seq_len(n_cycles)) {
      trace[t + 1, ] <- as.numeric(trace[t, ] %*% P)
    }

    u_H <- as.numeric(.null_coalesce(pars[["u_H"]], 0.90))
    u_S1 <- as.numeric(.null_coalesce(pars[["u_S1"]], 0.75))
    u_S2 <- as.numeric(.null_coalesce(pars[["u_S2"]], 0.55))
    c_H <- as.numeric(.null_coalesce(pars[["c_H"]], 200))
    c_S1 <- as.numeric(.null_coalesce(pars[["c_S1"]], 800))
    c_S2 <- as.numeric(.null_coalesce(pars[["c_S2"]], 2200))
    drug <- as.numeric(.null_coalesce(pars[[paste0("drug_", strategy)]], 0))

    util_vec <- c(u_H, u_S1, u_S2, 0)
    cost_vec <- c(c_H + drug, c_S1 + drug, c_S2 + drug, 0)

    occ <- trace[-1, , drop = FALSE]
    d_c <- as.numeric(.null_coalesce(pars[["d_cost"]], 0.03))
    d_q <- as.numeric(.null_coalesce(pars[["d_qaly"]], 0.03))
    w_c <- cea_discount_weights(n_cycles, rate = d_c, cycle_length = cl)
    w_q <- cea_discount_weights(n_cycles, rate = d_q, cycle_length = cl)
    qaly <- sum((occ %*% util_vec) * w_q) * cl
    cost <- sum((occ %*% cost_vec) * w_c) * cl

    time <- seq(0, n_cycles) * cl
    os <- 1 - trace[, "D"]
    survival <- data.frame(time = time, surv = as.numeric(os), curve = "OS",
                           stringsAsFactors = FALSE)

    list(cost = as.numeric(cost), qaly = as.numeric(qaly),
         trace = trace, survival = survival)
  }

  new_cea_model(info = info, params = params, simulate = simulate)
}
