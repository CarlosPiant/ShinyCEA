#' Progression-Free / Progressed / Death Example Model (3-State Markov)
#'
#' A 3-state cohort Markov model: PF -> Progressed -> Death.
#' Includes 6 strategies (std + 5 treatments) with hazard ratio modifiers
#' applied to transition probabilities. Returns both PFS and OS survival curves
#' derived from the Markov trace.
#'
#' @return A `cea_model` object.
#' @export
#' @examples
#' model <- cea_pf_prog_death()
#' print(model)
cea_pf_prog_death <- function() {
  info <- function() {
    list(
      key = "pf_prog_death",
      name = "PF -> Progressed -> Death (3-state Markov)",
      strategies = c("std", "t1", "t2", "t3", "t4", "t5")
    )
  }

  params <- function() {
    do.call(rbind, list(
      cea_param("time_horizon", 10, 5, 15, distribution = "fixed", include_psa = FALSE, include_tornado = FALSE),
      cea_param("cycle_length", 1/12, 1/12, 1/6, distribution = "fixed", include_psa = FALSE, include_tornado = FALSE),
      cea_param("p_PF_prog_std", 0.08, 0.04, 0.12, distribution = "beta"),
      cea_param("p_PF_death_std", 0.003, 0.001, 0.01, distribution = "beta"),
      cea_param("p_Prog_death_std", 0.06, 0.03, 0.12, distribution = "beta"),
      cea_param("hr_prog_t1", 0.80, 0.65, 0.95, distribution = "lognormal"),
      cea_param("hr_prog_t2", 0.70, 0.55, 0.88, distribution = "lognormal"),
      cea_param("hr_prog_t3", 0.85, 0.70, 1.00, distribution = "lognormal"),
      cea_param("hr_prog_t4", 0.75, 0.60, 0.90, distribution = "lognormal"),
      cea_param("hr_prog_t5", 0.65, 0.50, 0.85, distribution = "lognormal"),
      cea_param("hr_death_t1", 0.95, 0.80, 1.05, distribution = "lognormal"),
      cea_param("hr_death_t2", 0.90, 0.75, 1.02, distribution = "lognormal"),
      cea_param("hr_death_t3", 1.00, 0.85, 1.10, distribution = "lognormal"),
      cea_param("hr_death_t4", 0.92, 0.78, 1.05, distribution = "lognormal"),
      cea_param("hr_death_t5", 0.88, 0.72, 1.02, distribution = "lognormal"),
      cea_param("u_PF", 0.84, 0.75, 0.90, distribution = "beta"),
      cea_param("u_Prog", 0.62, 0.50, 0.75, distribution = "beta"),
      cea_param("c_PF", 900, 600, 1200, distribution = "gamma"),
      cea_param("c_Prog", 2200, 1500, 3500, distribution = "gamma"),
      cea_param("drug_std", 0, 0, 0, distribution = "fixed", include_psa = FALSE),
      cea_param("drug_t1", 800, 500, 1000, distribution = "gamma"),
      cea_param("drug_t2", 1200, 800, 1500, distribution = "gamma"),
      cea_param("drug_t3", 700, 400, 900, distribution = "gamma"),
      cea_param("drug_t4", 1300, 900, 1700, distribution = "gamma"),
      cea_param("drug_t5", 1500, 1100, 2000, distribution = "gamma"),
      cea_param("d_cost", 0.03, 0, 0.08, distribution = "fixed", include_psa = FALSE, include_tornado = TRUE),
      cea_param("d_qaly", 0.03, 0, 0.08, distribution = "fixed", include_psa = FALSE, include_tornado = TRUE)
    ))
  }

  simulate <- function(pars, strategy) {
    strategy <- match.arg(strategy, c("std", "t1", "t2", "t3", "t4", "t5"))
    .nc <- function(a, b) if (is.null(a)) b else a

    horizon <- as.numeric(.nc(pars[["time_horizon"]], 10))
    cl <- as.numeric(.nc(pars[["cycle_length"]], 1/12))
    n_cycles <- max(1L, as.integer(round(horizon / cl)))

    p_PF_prog <- as.numeric(.nc(pars[["p_PF_prog_std"]], 0.08))
    p_PF_death <- as.numeric(.nc(pars[["p_PF_death_std"]], 0.003))
    p_Prog_death <- as.numeric(.nc(pars[["p_Prog_death_std"]], 0.06))

    if (strategy != "std") {
      hr_p <- as.numeric(.nc(pars[[paste0("hr_prog_", strategy)]], 1))
      hr_d <- as.numeric(.nc(pars[[paste0("hr_death_", strategy)]], 1))
      p_PF_prog <- pmin(0.999, p_PF_prog * hr_p)
      p_PF_death <- pmin(0.999, p_PF_death * hr_d)
      p_Prog_death <- pmin(0.999, p_Prog_death * hr_d)
    }

    P <- rbind(
      c(pmax(0, 1 - p_PF_prog - p_PF_death), p_PF_prog, p_PF_death),
      c(0, pmax(0, 1 - p_Prog_death), p_Prog_death),
      c(0, 0, 1.0)
    )

    states <- c("PF", "Prog", "Death")
    trace <- matrix(0, nrow = n_cycles + 1, ncol = 3, dimnames = list(NULL, states))
    trace[1, ] <- c(1, 0, 0)
    for (t in seq_len(n_cycles)) {
      trace[t + 1, ] <- as.numeric(trace[t, ] %*% P)
    }

    u_PF <- as.numeric(.nc(pars[["u_PF"]], 0.84))
    u_Prog <- as.numeric(.nc(pars[["u_Prog"]], 0.62))
    c_PF <- as.numeric(.nc(pars[["c_PF"]], 900))
    c_Prog <- as.numeric(.nc(pars[["c_Prog"]], 2200))
    drug <- as.numeric(.nc(pars[[paste0("drug_", strategy)]], 0))

    util_vec <- c(u_PF, u_Prog, 0)
    cost_vec <- c(c_PF + drug, c_Prog + drug, 0)

    occ <- trace[-1, , drop = FALSE]
    d_c <- as.numeric(.nc(pars[["d_cost"]], 0.03))
    d_q <- as.numeric(.nc(pars[["d_qaly"]], 0.03))
    w_c <- cea_discount_weights(n_cycles, rate = d_c, cycle_length = cl)
    w_q <- cea_discount_weights(n_cycles, rate = d_q, cycle_length = cl)
    qaly <- sum((occ %*% util_vec) * w_q) * cl
    cost <- sum((occ %*% cost_vec) * w_c) * cl

    time <- seq(0, n_cycles) * cl
    os <- 1 - trace[, "Death"]
    pfs <- trace[, "PF"]

    survival <- rbind(
      data.frame(time = time, surv = as.numeric(os), curve = "OS", stringsAsFactors = FALSE),
      data.frame(time = time, surv = as.numeric(pfs), curve = "PFS", stringsAsFactors = FALSE)
    )

    list(cost = as.numeric(cost), qaly = as.numeric(qaly),
         trace = trace, survival = survival)
  }

  new_cea_model(info = info, params = params, simulate = simulate)
}
