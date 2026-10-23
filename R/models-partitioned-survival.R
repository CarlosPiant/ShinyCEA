#' Partitioned Survival Example Model (Weibull PFS/OS)
#'
#' A partitioned survival model with Weibull PFS and OS curves.
#' Includes 6 strategies (std + 5 treatments) with hazard ratio modifiers.
#'
#' @return A `cea_model` object.
#' @export
#' @examples
#' model <- cea_partitioned_survival()
#' print(model)
cea_partitioned_survival <- function() {
  info <- function() {
    list(
      key = "ps_weibull",
      name = "Partitioned Survival (Weibull PFS/OS)",
      strategies = c("std", "t1", "t2", "t3", "t4", "t5")
    )
  }

  params <- function() {
    do.call(rbind, list(
      cea_param("time_horizon", 8, 5, 12, distribution = "fixed", include_psa = FALSE, include_tornado = FALSE),
      cea_param("cycle_length", 1/12, 1/12, 1/6, distribution = "fixed", include_psa = FALSE, include_tornado = FALSE),
      cea_param("pfs_shape_std", 1.4, 1.0, 2.0, distribution = "lognormal"),
      cea_param("pfs_scale_std", 2.0, 1.0, 3.0, distribution = "lognormal"),
      cea_param("os_shape_std", 1.3, 1.0, 1.8, distribution = "lognormal"),
      cea_param("os_scale_std", 3.5, 2.0, 5.0, distribution = "lognormal"),
      cea_param("hr_pfs_t1", 0.80, 0.65, 0.95, distribution = "lognormal"),
      cea_param("hr_pfs_t2", 0.72, 0.58, 0.90, distribution = "lognormal"),
      cea_param("hr_pfs_t3", 0.88, 0.75, 1.02, distribution = "lognormal"),
      cea_param("hr_pfs_t4", 0.76, 0.62, 0.92, distribution = "lognormal"),
      cea_param("hr_pfs_t5", 0.70, 0.55, 0.88, distribution = "lognormal"),
      cea_param("hr_os_t1", 0.95, 0.80, 1.05, distribution = "lognormal"),
      cea_param("hr_os_t2", 0.90, 0.78, 1.02, distribution = "lognormal"),
      cea_param("hr_os_t3", 1.00, 0.85, 1.10, distribution = "lognormal"),
      cea_param("hr_os_t4", 0.92, 0.78, 1.04, distribution = "lognormal"),
      cea_param("hr_os_t5", 0.88, 0.75, 1.00, distribution = "lognormal"),
      cea_param("u_PF", 0.82, 0.70, 0.90, distribution = "beta"),
      cea_param("u_Prog", 0.60, 0.45, 0.75, distribution = "beta"),
      cea_param("c_PF", 1100, 700, 1400, distribution = "gamma"),
      cea_param("c_Prog", 2800, 1800, 4000, distribution = "gamma"),
      cea_param("drug_std", 0, 0, 0, distribution = "fixed", include_psa = FALSE),
      cea_param("drug_t1", 1500, 1000, 2000, distribution = "gamma"),
      cea_param("drug_t2", 1800, 1300, 2400, distribution = "gamma"),
      cea_param("drug_t3", 1200, 800, 1600, distribution = "gamma"),
      cea_param("drug_t4", 1700, 1200, 2200, distribution = "gamma"),
      cea_param("drug_t5", 2000, 1500, 2600, distribution = "gamma"),
      cea_param("d_cost", 0.03, 0, 0.08, distribution = "fixed", include_psa = FALSE, include_tornado = TRUE),
      cea_param("d_qaly", 0.03, 0, 0.08, distribution = "fixed", include_psa = FALSE, include_tornado = TRUE)
    ))
  }

  simulate <- function(pars, strategy) {
    strategy <- match.arg(strategy, c("std", "t1", "t2", "t3", "t4", "t5"))
    .nc <- function(a, b) if (is.null(a)) b else a

    horizon <- as.numeric(.nc(pars[["time_horizon"]], 8))
    cl <- as.numeric(.nc(pars[["cycle_length"]], 1/12))
    tgrid <- seq(0, horizon, by = cl)

    ps <- as.numeric(.nc(pars[["pfs_shape_std"]], 1.4))
    pl <- as.numeric(.nc(pars[["pfs_scale_std"]], 2.0))
    os_shape <- as.numeric(.nc(pars[["os_shape_std"]], 1.3))
    ol <- as.numeric(.nc(pars[["os_scale_std"]], 3.5))

    hr_pfs <- if (strategy == "std") 1 else as.numeric(.nc(pars[[paste0("hr_pfs_", strategy)]], 1))
    hr_os <- if (strategy == "std") 1 else as.numeric(.nc(pars[[paste0("hr_os_", strategy)]], 1))

    pfs_scale_t <- pl / (hr_pfs)^(1/ps)
    os_scale_t <- ol / (hr_os)^(1/os_shape)

    weib_S <- function(t, shape, scale) exp(-(pmax(t, 0) / scale)^shape)

    S_pfs <- weib_S(tgrid, shape = ps, scale = pfs_scale_t)
    S_os <- weib_S(tgrid, shape = os_shape, scale = os_scale_t)

    # Progression-free survival cannot exceed overall survival (E13).
    PF <- pmax(pmin(S_pfs, S_os), 0)
    Prog <- pmax(S_os - PF, 0)

    u_PF <- as.numeric(.nc(pars[["u_PF"]], 0.82))
    u_Prog <- as.numeric(.nc(pars[["u_Prog"]], 0.60))
    c_PF <- as.numeric(.nc(pars[["c_PF"]], 1100))
    c_Prog <- as.numeric(.nc(pars[["c_Prog"]], 2800))
    drug <- as.numeric(.nc(pars[[paste0("drug_", strategy)]], 0))

    d_c <- as.numeric(.nc(pars[["d_cost"]], 0.03))
    d_q <- as.numeric(.nc(pars[["d_qaly"]], 0.03))
    # One cycle per interval between grid points, valued at the end of the
    # cycle like the Markov models (the grid used to add one extra cycle, E13).
    n_c <- length(tgrid) - 1L
    w_c <- cea_discount_weights(n_c, rate = d_c, cycle_length = cl)
    w_q <- cea_discount_weights(n_c, rate = d_q, cycle_length = cl)
    qaly <- sum((PF[-1] * u_PF + Prog[-1] * u_Prog) * w_q) * cl
    cost <- sum((PF[-1] * (c_PF + drug) + Prog[-1] * (c_Prog + drug)) * w_c) * cl

    survival <- rbind(
      data.frame(time = tgrid, surv = as.numeric(S_os), curve = "OS", stringsAsFactors = FALSE),
      data.frame(time = tgrid, surv = as.numeric(PF), curve = "PFS", stringsAsFactors = FALSE)
    )

    list(cost = as.numeric(cost), qaly = as.numeric(qaly),
         trace = NULL, survival = survival)
  }

  new_cea_model(
    info = info, params = params, simulate = simulate,
    composites = list(
      "Standard" = c(std = 1),
      "Current mix" = c(std = 0.20, t1 = 0.25, t2 = 0.20, t3 = 0.15, t4 = 0.10, t5 = 0.10),
      "Best combo" = c(t5 = 0.60, t2 = 0.40)
    ),
    scenarios = list(
      "Base case" = list(),
      "No discounting" = list(d_cost = 0, d_qaly = 0),
      "Short horizon (5yr)" = list(time_horizon = 5),
      "Optimistic PFS" = list(hr_pfs_t5 = 0.50),
      "Pessimistic OS" = list(hr_os_t5 = 0.95)
    ),
    subgroups = list(
      "Overall" = list(),
      "Good prognosis" = list(os_scale_std = 4.5, pfs_scale_std = 2.5),
      "Poor prognosis" = list(os_scale_std = 2.0, pfs_scale_std = 1.0)
    )
  )
}
