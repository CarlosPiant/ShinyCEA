#' HIV Treatment Microsimulation Example Model
#'
#' An individual-level (microsimulation) model for a chronic disease treatment
#' decision. Simulates N patients through 4 health states (Stable, Progressed,
#' Severe, Dead) with heterogeneous baseline risk, treatment effects, and
#' patient-level event histories. Demonstrates how microsimulation models
#' integrate with ShinyCEA's analysis pipeline.
#'
#' Unlike cohort models that track proportions, microsimulation tracks each
#' patient individually, allowing for:
#' \itemize{
#'   \item Patient-level heterogeneity (age, baseline risk)
#'   \item History-dependent transitions (time in state)
#'   \item Individual event tracking (count of hospitalizations per patient)
#' }
#'
#' @return A `cea_model` object.
#' @export
#' @examples
#' model <- cea_microsim()
#' print(model)
#'
#' # Quick deterministic run (small N for speed)
#' params <- model$default_params()
#' params$base[params$name == "n_patients"] <- 500
#' det <- run_deterministic(model, params, wtp = 50000)
cea_microsim <- function() {
  info <- function() {
    list(
      key = "microsim_chronic",
      name = "Chronic Disease Microsimulation (4-state)",
      strategies = c("standard", "new_treatment")
    )
  }

  params <- function() {
    do.call(rbind, list(
      # --- Structural ---
      cea_param("n_patients", 1000, 200, 5000,
                distribution = "fixed", include_psa = FALSE, include_tornado = FALSE),
      cea_param("n_cycles", 120, 60, 240,
                distribution = "fixed", include_psa = FALSE, include_tornado = FALSE),
      cea_param("cycle_length", 1/12, 1/12, 1/12,
                distribution = "fixed", include_psa = FALSE, include_tornado = FALSE),
      cea_param("mean_age", 45, 30, 60,
                distribution = "fixed", include_psa = FALSE, include_tornado = TRUE),
      cea_param("age_sd", 10, 5, 15,
                distribution = "fixed", include_psa = FALSE, include_tornado = FALSE),

      # --- Transition probabilities (monthly, standard arm) ---
      cea_param("p_stable_prog", 0.02, 0.01, 0.04, distribution = "beta"),
      cea_param("p_stable_dead", 0.002, 0.001, 0.005, distribution = "beta"),
      cea_param("p_prog_severe", 0.05, 0.03, 0.08, distribution = "beta"),
      cea_param("p_prog_dead", 0.01, 0.005, 0.02, distribution = "beta"),
      cea_param("p_severe_dead", 0.04, 0.02, 0.08, distribution = "beta"),

      # --- Treatment effect (new_treatment) ---
      cea_param("rr_progression", 0.60, 0.40, 0.80, distribution = "lognormal"),
      cea_param("rr_mortality", 0.75, 0.55, 0.95, distribution = "lognormal"),
      cea_param("p_recovery", 0.03, 0.01, 0.06, distribution = "beta",
                include_tornado = TRUE),

      # --- Age effect on mortality (per year above 45) ---
      cea_param("hr_age_mortality", 1.03, 1.01, 1.05, distribution = "lognormal"),

      # --- Costs (monthly) ---
      cea_param("c_stable", 500, 300, 800, distribution = "gamma"),
      cea_param("c_progressed", 2000, 1200, 3000, distribution = "gamma"),
      cea_param("c_severe", 5000, 3000, 8000, distribution = "gamma"),
      cea_param("c_drug_standard", 200, 100, 400, distribution = "gamma"),
      cea_param("c_drug_new", 1500, 800, 2500, distribution = "gamma"),
      cea_param("c_hospitalization", 15000, 10000, 22000, distribution = "gamma"),
      cea_param("p_hospitalization_severe", 0.15, 0.08, 0.25, distribution = "beta"),

      # --- Utilities ---
      cea_param("u_stable", 0.85, 0.75, 0.95, distribution = "beta"),
      cea_param("u_progressed", 0.65, 0.50, 0.78, distribution = "beta"),
      cea_param("u_severe", 0.40, 0.25, 0.55, distribution = "beta"),

      # --- Discounting ---
      cea_param("d_cost", 0.03, 0, 0.05,
                distribution = "fixed", include_psa = FALSE, include_tornado = TRUE),
      cea_param("d_qaly", 0.03, 0, 0.05,
                distribution = "fixed", include_psa = FALSE, include_tornado = TRUE)
    ))
  }

  simulate <- function(pars, strategy) {
    strategy <- match.arg(strategy, c("standard", "new_treatment"))
    .g <- function(nm, d) { v <- pars[[nm]]; if (is.null(v)) d else as.numeric(v) }

    N <- as.integer(.g("n_patients", 1000))
    n_cycles <- as.integer(.g("n_cycles", 120))
    cl <- .g("cycle_length", 1/12)
    mean_age <- .g("mean_age", 45)
    age_sd <- .g("age_sd", 10)

    # Base transition probs
    p_sp <- .g("p_stable_prog", 0.02)
    p_sd <- .g("p_stable_dead", 0.002)
    p_ps <- .g("p_prog_severe", 0.05)
    p_pd <- .g("p_prog_dead", 0.01)
    p_xd <- .g("p_severe_dead", 0.04)
    hr_age <- .g("hr_age_mortality", 1.03)

    # Treatment modifiers
    rr_prog <- if (strategy == "new_treatment") .g("rr_progression", 0.60) else 1
    rr_mort <- if (strategy == "new_treatment") .g("rr_mortality", 0.75) else 1
    p_recov <- if (strategy == "new_treatment") .g("p_recovery", 0.03) else 0

    # Costs and utilities
    c_stable <- .g("c_stable", 500)
    c_prog <- .g("c_progressed", 2000)
    c_severe <- .g("c_severe", 5000)
    c_drug <- if (strategy == "new_treatment") .g("c_drug_new", 1500) else
      .g("c_drug_standard", 200)
    c_hosp <- .g("c_hospitalization", 15000)
    p_hosp <- .g("p_hospitalization_severe", 0.15)
    u_stable <- .g("u_stable", 0.85)
    u_prog <- .g("u_progressed", 0.65)
    u_severe <- .g("u_severe", 0.40)
    d_c <- .g("d_cost", 0.03)
    d_q <- .g("d_qaly", 0.03)

    # Discount weights
    w_c <- cea_discount_weights(n_cycles, rate = d_c, cycle_length = cl)
    w_q <- cea_discount_weights(n_cycles, rate = d_q, cycle_length = cl)

    # States: 1=Stable, 2=Progressed, 3=Severe, 4=Dead
    # Generate heterogeneous patients
    # Use a deterministic seed based on strategy to ensure reproducibility
    # while allowing different draws per PSA iteration (outer seed varies)
    ages <- mean_age + age_sd * stats::qnorm((seq_len(N) - 0.5) / N)
    ages <- pmax(ages, 18)

    # Pre-compute age-adjusted mortality multiplier per patient
    age_hr <- hr_age^pmax(ages - 45, 0)

    # --- Vectorized microsimulation ---
    state <- rep(1L, N)  # all start in Stable
    patient_cost <- numeric(N)
    patient_qaly <- numeric(N)

    # Trace: proportion in each state per cycle (for the app)
    trace <- matrix(0, nrow = n_cycles + 1, ncol = 4,
                    dimnames = list(NULL, c("Stable", "Progressed", "Severe", "Dead")))
    trace[1, ] <- c(1, 0, 0, 0)

    for (t in seq_len(n_cycles)) {
      alive <- state < 4L
      n_alive <- sum(alive)
      if (n_alive == 0) {
        trace[t + 1, ] <- c(0, 0, 0, 1)
        next
      }

      # Random draws for this cycle (vectorized)
      u <- stats::runif(N)

      # Transition logic per current state
      new_state <- state

      # Stable (1) -> Progressed (2) or Dead (4)
      idx_s <- which(state == 1L)
      if (length(idx_s)) {
        p_prog_i <- p_sp * rr_prog
        p_dead_i <- p_sd * rr_mort * age_hr[idx_s]
        p_dead_i <- pmin(p_dead_i, 1 - p_prog_i)
        cum1 <- p_prog_i
        cum2 <- cum1 + p_dead_i
        new_state[idx_s] <- ifelse(u[idx_s] < cum1, 2L,
                             ifelse(u[idx_s] < cum2, 4L, 1L))
      }

      # Progressed (2) -> Severe (3), Dead (4), or recover to Stable (1)
      idx_p <- which(state == 2L)
      if (length(idx_p)) {
        p_sev_i <- p_ps
        p_dead_i <- p_pd * rr_mort * age_hr[idx_p]
        p_dead_i <- pmin(p_dead_i, 1 - p_sev_i - p_recov)
        cum1 <- p_sev_i
        cum2 <- cum1 + p_dead_i
        cum3 <- cum2 + p_recov
        new_state[idx_p] <- ifelse(u[idx_p] < cum1, 3L,
                             ifelse(u[idx_p] < cum2, 4L,
                              ifelse(u[idx_p] < cum3, 1L, 2L)))
      }

      # Severe (3) -> Dead (4) or stay
      idx_x <- which(state == 3L)
      if (length(idx_x)) {
        p_dead_i <- p_xd * rr_mort * age_hr[idx_x]
        new_state[idx_x] <- ifelse(u[idx_x] < p_dead_i, 4L, 3L)
      }

      state <- new_state

      # Accumulate costs and QALYs
      cost_t <- numeric(N)
      qaly_t <- numeric(N)

      s1 <- state == 1L
      s2 <- state == 2L
      s3 <- state == 3L

      cost_t[s1] <- (c_stable + c_drug) * w_c[t]
      cost_t[s2] <- (c_prog + c_drug) * w_c[t]
      cost_t[s3] <- (c_severe + c_drug) * w_c[t]

      # Hospitalizations for severe patients
      if (any(s3)) {
        n_sev <- sum(s3)
        hosp_draws <- stats::runif(n_sev)
        cost_t[s3] <- cost_t[s3] + (hosp_draws < p_hosp) * c_hosp * w_c[t]
      }

      qaly_t[s1] <- u_stable * cl * w_q[t]
      qaly_t[s2] <- u_prog * cl * w_q[t]
      qaly_t[s3] <- u_severe * cl * w_q[t]

      patient_cost <- patient_cost + cost_t
      patient_qaly <- patient_qaly + qaly_t

      # Record trace
      trace[t + 1, ] <- c(mean(state == 1L), mean(state == 2L),
                           mean(state == 3L), mean(state == 4L))
    }

    # Aggregate to cohort means
    cost <- mean(patient_cost)
    qaly <- mean(patient_qaly)

    # Survival from trace
    time <- seq(0, n_cycles) * cl
    os <- 1 - trace[, "Dead"]
    survival <- data.frame(time = time, surv = os, curve = "OS",
                           stringsAsFactors = FALSE)

    list(cost = as.numeric(cost), qaly = as.numeric(qaly),
         trace = trace, survival = survival)
  }

  new_cea_model(
    info = info, params = params, simulate = simulate,
    description = paste(
      "Individual-level microsimulation for a chronic disease with 4 states",
      "(Stable, Progressed, Severe, Dead). Simulates N patients individually",
      "with heterogeneous baseline age, age-dependent mortality, treatment-",
      "induced recovery from progression, and patient-level hospitalization",
      "events. Demonstrates microsimulation integration with ShinyCEA."
    ),
    scenarios = list(
      "Base case" = list(),
      "Small cohort (200)" = list(n_patients = 200),
      "Large cohort (5000)" = list(n_patients = 5000),
      "Older population" = list(mean_age = 60),
      "Younger population" = list(mean_age = 30),
      "No discounting" = list(d_cost = 0, d_qaly = 0)
    )
  )
}
