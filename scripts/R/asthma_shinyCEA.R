# =============================================================================
# Asthma Gene Therapy vs Dupilumab — ShinyCEA Pure Markov Implementation
#
# 10-state Markov model rewritten as pure R matrix operations (no twig).
# Same states, transitions, costs, and utilities as the original twig model.
#
# States: Controlled, Uncontrolled, mod_office, mod_ED, sev_office, sev_ED,
#         sev_hosp, death_asthma, death_other, cured
# Strategies: DUP, GeneTherapy
# =============================================================================

library(ShinyCEA)

# ---------------------------------------------------------------------------
# Life table helper
# ---------------------------------------------------------------------------
.asthma_life_table <- function(init_age = 18, end_age = 99, cycles_per_year = 12) {
  p_yearly <- c(
    0.005446, 0.000403, 0.000254, 0.000192, 0.000161, 0.000143, 0.000130,
    0.000119, 0.000107, 0.000095, 0.000090, 0.000100, 0.000136, 0.000205,
    0.000299, 0.000405, 0.000513, 0.000623, 0.000731, 0.000837, 0.000949,
    0.001065, 0.001170, 0.001259, 0.001335, 0.001406, 0.001480, 0.001560,
    0.001651, 0.001749, 0.001849, 0.001947, 0.002040, 0.002128, 0.002216,
    0.002308, 0.002409, 0.002519, 0.002638, 0.002768, 0.002916, 0.003078,
    0.003244, 0.003410, 0.003587, 0.003792, 0.004036, 0.004315, 0.004625,
    0.004959, 0.005308, 0.005686, 0.006118, 0.006620, 0.007184, 0.007766,
    0.008369, 0.009042, 0.009795, 0.010606, 0.011467, 0.012333, 0.013173,
    0.013981, 0.014798, 0.015666, 0.016726, 0.017853, 0.019122, 0.020526,
    0.021919, 0.023536, 0.025372, 0.027616, 0.029889, 0.033726, 0.036933,
    0.041016, 0.044758, 0.049530, 0.054120, 0.059483, 0.065401, 0.072224,
    0.080609, 0.089139, 0.099586, 0.111021, 0.123484, 0.137001, 0.151584,
    0.167229, 0.183913, 0.201590, 0.220190, 0.239623, 0.259772, 0.280504,
    0.301662, 0.323082, 1)

  rate <- -log(1 - p_yearly)
  p_cycle <- 1 - exp(-rate / cycles_per_year)

  ages <- 0:100
  keep <- ages >= init_age & ages <= end_age
  p_per_age <- p_cycle[keep]
  rep(p_per_age, each = cycles_per_year)
}

# ===========================================================================
# Model definition
# ===========================================================================

asthma_model <- new_cea_model(

  info = function() {
    list(
      key = "asthma_gene_therapy",
      name = "Asthma: Dupilumab vs Gene Therapy (10-state Markov)",
      strategies = c("DUP", "GeneTherapy")
    )
  },

  params = function() {
    do.call(rbind, list(
      # --- Structural ---
      cea_param("time_horizon", 2, 1, 10,
                distribution = "fixed", include_psa = FALSE, include_tornado = TRUE),
      cea_param("initial_age", 18, 12, 65,
                distribution = "fixed", include_psa = FALSE, include_tornado = FALSE),
      cea_param("gene_therapy_cure_cycles", 120, 12, 9999,
                distribution = "fixed", include_psa = FALSE, include_tornado = TRUE),
      cea_param("d_cost", 0.003, 0, 0.01,
                distribution = "fixed", include_psa = FALSE, include_tornado = TRUE),
      cea_param("d_qaly", 0.003, 0, 0.01,
                distribution = "fixed", include_psa = FALSE, include_tornado = TRUE),

      # --- Death probabilities ---
      cea_param("pr_death_uncon", 0.000306, 0.0001, 0.001, distribution = "beta"),
      cea_param("pr_death_ed", 0.00137, 0.0005, 0.003, distribution = "beta"),
      cea_param("pr_death_hosp", 0.0019, 0.0008, 0.004, distribution = "beta"),

      # --- From Controlled ---
      cea_param("pr_con_un", 0.03, 0.01, 0.06, distribution = "beta"),
      cea_param("pr_con_mod_office", 0.039, 0.02, 0.06, distribution = "beta"),
      cea_param("pr_con_mod_ed", 0.001, 0.0005, 0.003, distribution = "beta"),
      cea_param("pr_con_seve_office", 0.01876, 0.01, 0.03, distribution = "beta"),
      cea_param("pr_con_seve_ed", 0.00058, 0.0002, 0.001, distribution = "beta"),
      cea_param("pr_con_seve_hosp", 0.00066, 0.0003, 0.001, distribution = "beta"),

      # --- From Uncontrolled ---
      cea_param("pr_un_con", 0.291, 0.20, 0.40, distribution = "beta"),
      cea_param("pr_un_mod_office", 0.075, 0.04, 0.12, distribution = "beta"),
      cea_param("pr_un_mod_ed", 0.002, 0.001, 0.005, distribution = "beta"),
      cea_param("pr_un_seve_office", 0.03664, 0.02, 0.06, distribution = "beta"),
      cea_param("pr_un_seve_ed", 0.00113, 0.0005, 0.003, distribution = "beta"),
      cea_param("pr_un_seve_hosp", 0.00129, 0.0005, 0.003, distribution = "beta"),
      cea_param("pr_un_cured", 0.50, 0.30, 0.70, distribution = "beta"),

      # --- From Moderate exacerbations ---
      cea_param("pr_mod_office_con", 0.536, 0.40, 0.65, distribution = "beta"),
      cea_param("pr_mod_office_un", 0.268, 0.15, 0.40, distribution = "beta"),
      cea_param("pr_mod_ed_con", 0.5355, 0.40, 0.65, distribution = "beta"),
      cea_param("pr_mod_ed_un", 0.2675, 0.15, 0.40, distribution = "beta"),

      # --- From Severe exacerbations ---
      cea_param("pr_office_con", 0.4285, 0.30, 0.55, distribution = "beta"),
      cea_param("pr_office_un", 0.4285, 0.30, 0.55, distribution = "beta"),
      cea_param("pr_ed_con", 0.44, 0.30, 0.55, distribution = "beta"),
      cea_param("pr_ed_un", 0.44, 0.30, 0.55, distribution = "beta"),
      cea_param("pr_hosp_con", 0.39, 0.25, 0.50, distribution = "beta"),
      cea_param("pr_hosp_un", 0.39, 0.25, 0.50, distribution = "beta"),

      # --- Costs (monthly) ---
      cea_param("c_soc", 638, 400, 900, distribution = "gamma"),
      cea_param("c_office", 1604, 1000, 2200, distribution = "gamma"),
      cea_param("c_ed", 2161, 1400, 3000, distribution = "gamma"),
      cea_param("c_hosp", 9442, 6000, 14000, distribution = "gamma"),
      cea_param("c_dup", 2321.66, 1500, 3200, distribution = "gamma"),
      cea_param("c_gene_therapy_one_time", 100000, 50000, 200000, distribution = "gamma"),

      # --- Utilities (annual values, converted to monthly in simulate) ---
      cea_param("u_con", 0.907, 0.80, 0.95, distribution = "beta"),
      cea_param("u_uncon", 0.795, 0.65, 0.90, distribution = "beta"),
      cea_param("u_office", 0.535, 0.40, 0.65, distribution = "beta"),
      cea_param("u_ed", 0.45, 0.30, 0.55, distribution = "beta"),
      cea_param("u_hosp", 0.326, 0.20, 0.45, distribution = "beta"),
      cea_param("u_cured", 1.0, 0.90, 1.0,
                distribution = "fixed", include_psa = FALSE, include_tornado = FALSE)
    ))
  },

  simulate = function(pars, strategy) {
    strategy <- match.arg(strategy, c("DUP", "GeneTherapy"))
    .g <- function(nm, default) {
      v <- pars[[nm]]; if (is.null(v)) default else as.numeric(v)
    }

    time_horizon <- .g("time_horizon", 2)
    cl <- 1 / 12
    n_cycles <- as.integer(round(time_horizon / cl))
    init_age <- as.integer(.g("initial_age", 18))
    cure_cycles <- as.integer(.g("gene_therapy_cure_cycles", 120))

    # Life table
    lt <- .asthma_life_table(init_age, min(init_age + time_horizon + 1, 100), 12)
    if (length(lt) < n_cycles) lt <- c(lt, rep(lt[length(lt)], n_cycles - length(lt)))

    # States: 1=Con, 2=Uncon, 3=mod_off, 4=mod_ED, 5=sev_off, 6=sev_ED,
    #         7=sev_hosp, 8=death_asthma, 9=death_other, 10=cured
    n_states <- 10L
    state_names <- c("Controlled", "Uncontrolled", "mod_office", "mod_ED",
                     "sev_office", "sev_ED", "sev_hosp",
                     "death_asthma", "death_other", "cured")

    trace <- matrix(0, nrow = n_cycles + 1, ncol = n_states,
                    dimnames = list(NULL, state_names))
    trace[1, "Uncontrolled"] <- 1  # start in Uncontrolled

    # Extract parameters
    pr_death_uncon <- .g("pr_death_uncon", 0.000306)
    pr_death_ed <- .g("pr_death_ed", 0.00137)
    pr_death_hosp <- .g("pr_death_hosp", 0.0019)

    for (t in seq_len(n_cycles)) {
      p_bg_death <- lt[t]  # background mortality this cycle

      # Build transition matrix for this cycle
      P <- matrix(0, nrow = n_states, ncol = n_states)

      # --- Event 1: Death ---
      # Asthma death probabilities by state
      p_asthma_death <- numeric(n_states)
      p_asthma_death[2] <- pr_death_uncon   # Uncontrolled
      p_asthma_death[4] <- pr_death_ed      # mod_ED
      p_asthma_death[6] <- pr_death_ed      # sev_ED
      p_asthma_death[7] <- pr_death_hosp    # sev_hosp

      # Background death probabilities by state
      p_other_death <- numeric(n_states)
      p_other_death[1] <- p_bg_death   # Controlled
      p_other_death[3] <- p_bg_death   # mod_office
      p_other_death[5] <- p_bg_death   # sev_office
      p_other_death[10] <- p_bg_death  # cured

      # Absorbing death states stay
      P[8, 8] <- 1  # death_asthma
      P[9, 9] <- 1  # death_other

      # Probability of surviving (no death)
      p_survive <- 1 - p_asthma_death - p_other_death

      # --- Event 2: Cure (GeneTherapy only, cycle 1) ---
      p_cure <- numeric(n_states)
      if (strategy == "GeneTherapy" && t == 1) {
        p_cure[2] <- .g("pr_un_cured", 0.50)  # Uncontrolled -> cured
      }

      # --- Event 3: Asthma transitions (conditional on surviving and not cured) ---
      # From Controlled (state 1)
      pr_con_out <- c(.g("pr_con_un", 0.03),
                      .g("pr_con_mod_office", 0.039), .g("pr_con_mod_ed", 0.001),
                      .g("pr_con_seve_office", 0.01876), .g("pr_con_seve_ed", 0.00058),
                      .g("pr_con_seve_hosp", 0.00066))
      # targets: Uncon, mod_off, mod_ED, sev_off, sev_ED, sev_hosp
      p_con_stay <- max(0, 1 - sum(pr_con_out))

      # From Uncontrolled (state 2)
      pr_un_out <- c(.g("pr_un_con", 0.291),
                     .g("pr_un_mod_office", 0.075), .g("pr_un_mod_ed", 0.002),
                     .g("pr_un_seve_office", 0.03664), .g("pr_un_seve_ed", 0.00113),
                     .g("pr_un_seve_hosp", 0.00129))
      p_un_stay <- max(0, 1 - sum(pr_un_out))

      # From mod_office (state 3)
      p_mo_con <- .g("pr_mod_office_con", 0.536)
      p_mo_un <- .g("pr_mod_office_un", 0.268)
      p_mo_stay <- max(0, 1 - p_mo_con - p_mo_un)

      # From mod_ED (state 4)
      p_me_con <- .g("pr_mod_ed_con", 0.5355)
      p_me_un <- .g("pr_mod_ed_un", 0.2675)
      p_me_stay <- max(0, 1 - p_me_con - p_me_un)

      # From sev_office (state 5)
      p_so_con <- .g("pr_office_con", 0.4285)
      p_so_un <- .g("pr_office_un", 0.4285)
      p_so_stay <- max(0, 1 - p_so_con - p_so_un)

      # From sev_ED (state 6)
      p_se_con <- .g("pr_ed_con", 0.44)
      p_se_un <- .g("pr_ed_un", 0.44)
      p_se_stay <- max(0, 1 - p_se_con - p_se_un)

      # From sev_hosp (state 7)
      p_sh_con <- .g("pr_hosp_con", 0.39)
      p_sh_un <- .g("pr_hosp_un", 0.39)
      p_sh_stay <- max(0, 1 - p_sh_con - p_sh_un)

      # From cured (state 10): relapse to Uncontrolled after cure_cycles
      p_cure_relapse <- 0
      if (strategy == "GeneTherapy" && t > cure_cycles) {
        p_cure_relapse <- 1  # relapse to Uncontrolled
      }

      # --- Build full transition matrix ---
      # The twig event cascade: P(transition) = P(death) + P(survive) * P(cure) +
      #   P(survive) * P(not_cure) * P(asthma_transition)
      # Simplified per state:

      for (s in c(1:7, 10)) {  # alive states
        p_ad <- p_asthma_death[s]
        p_od <- p_other_death[s]
        p_surv <- max(0, 1 - p_ad - p_od)
        p_c <- p_cure[s]
        p_nc <- 1 - p_c  # prob not cured given survival

        # Death transitions
        P[s, 8] <- p_ad           # -> death_asthma
        P[s, 9] <- p_od           # -> death_other

        # Cure transition (survive * cure probability)
        if (s == 10) {
          # From cured: relapse or stay
          P[s, 10] <- p_surv * (1 - p_cure_relapse)  # stay cured
          P[s, 2] <- p_surv * p_cure_relapse          # relapse to Uncontrolled
        } else {
          P[s, 10] <- P[s, 10] + p_surv * p_c  # -> cured

          # Asthma transitions (survive * not_cured * transition_prob)
          p_base <- p_surv * p_nc

          if (s == 1) {  # Controlled
            P[s, 1] <- p_base * p_con_stay
            P[s, 2] <- p_base * pr_con_out[1]
            P[s, 3] <- p_base * pr_con_out[2]
            P[s, 4] <- p_base * pr_con_out[3]
            P[s, 5] <- p_base * pr_con_out[4]
            P[s, 6] <- p_base * pr_con_out[5]
            P[s, 7] <- p_base * pr_con_out[6]
          } else if (s == 2) {  # Uncontrolled
            P[s, 1] <- p_base * pr_un_out[1]
            P[s, 2] <- p_base * p_un_stay
            P[s, 3] <- p_base * pr_un_out[2]
            P[s, 4] <- p_base * pr_un_out[3]
            P[s, 5] <- p_base * pr_un_out[4]
            P[s, 6] <- p_base * pr_un_out[5]
            P[s, 7] <- p_base * pr_un_out[6]
          } else if (s == 3) {  # mod_office
            P[s, 1] <- p_base * p_mo_con
            P[s, 2] <- p_base * p_mo_un
            P[s, 3] <- p_base * p_mo_stay
          } else if (s == 4) {  # mod_ED
            P[s, 1] <- p_base * p_me_con
            P[s, 2] <- p_base * p_me_un
            P[s, 4] <- p_base * p_me_stay
          } else if (s == 5) {  # sev_office
            P[s, 1] <- p_base * p_so_con
            P[s, 2] <- p_base * p_so_un
            P[s, 5] <- p_base * p_so_stay
          } else if (s == 6) {  # sev_ED
            P[s, 1] <- p_base * p_se_con
            P[s, 2] <- p_base * p_se_un
            P[s, 6] <- p_base * p_se_stay
          } else if (s == 7) {  # sev_hosp
            P[s, 1] <- p_base * p_sh_con
            P[s, 2] <- p_base * p_sh_un
            P[s, 7] <- p_base * p_sh_stay
          }
        }
      }

      trace[t + 1, ] <- as.numeric(trace[t, ] %*% P)
    }

    # --- Payoffs ---
    occ <- trace[-1, , drop = FALSE]

    # Costs per cycle
    drug_cost <- .g("c_dup", 2321.66)  # both arms pay DUP
    c_soc <- .g("c_soc", 638)
    cost_vec <- c(
      c_soc + drug_cost,               # Controlled
      c_soc + drug_cost,               # Uncontrolled
      .g("c_office", 1604) + drug_cost, # mod_office
      .g("c_ed", 2161) + drug_cost,     # mod_ED
      .g("c_office", 1604) + drug_cost, # sev_office
      .g("c_ed", 2161) + drug_cost,     # sev_ED
      .g("c_hosp", 9442) + drug_cost,   # sev_hosp
      0, 0,                             # death_asthma, death_other
      0                                 # cured
    )

    # Utilities per cycle (annual / 12)
    u_con <- .g("u_con", 0.907) / 12
    u_uncon <- .g("u_uncon", 0.795) / 12
    u_office <- .g("u_office", 0.535) / 12
    u_ed <- .g("u_ed", 0.45) / 12
    u_hosp <- .g("u_hosp", 0.326) / 12
    u_cured <- .g("u_cured", 1.0) / 12
    util_vec <- c(u_con, u_uncon, u_office, u_ed, u_office, u_ed, u_hosp,
                  0, 0, u_cured)

    # Discounting
    d_c <- .g("d_cost", 0.003)
    d_q <- .g("d_qaly", 0.003)
    w_c <- 1 / (1 + d_c)^(seq_len(n_cycles) - 1)
    w_q <- 1 / (1 + d_q)^(seq_len(n_cycles) - 1)

    cost <- sum((occ %*% cost_vec) * w_c)
    qaly <- sum((occ %*% util_vec) * w_q)

    # Gene therapy one-time cost (cycle 1, cured state)
    if (strategy == "GeneTherapy") {
      cost <- cost + .g("c_gene_therapy_one_time", 100000) * trace[2, "cured"]
    }

    # Survival
    total_death <- trace[, "death_asthma"] + trace[, "death_other"]
    os <- 1 - total_death
    time <- seq(0, n_cycles) * cl
    survival <- data.frame(time = time, surv = os, curve = "OS",
                           stringsAsFactors = FALSE)

    list(cost = as.numeric(cost), qaly = as.numeric(qaly),
         trace = trace, survival = survival)
  },

  description = paste(
    "10-state Markov cohort model comparing Dupilumab (biologic) vs",
    "Gene Therapy for moderate-to-severe asthma. States: Controlled,",
    "Uncontrolled, moderate exacerbations (office/ED), severe exacerbations",
    "(office/ED/hospital), asthma death, other-cause death, and cured.",
    "Monthly cycles with age-dependent background mortality from US life tables.",
    "Gene therapy has a one-time cost and induces a cured state with",
    "potential relapse after a configurable number of cycles."
  ),

  composites = list(
    "DUP alone" = c(DUP = 1),
    "GeneTherapy alone" = c(GeneTherapy = 1),
    "50/50 mix" = c(DUP = 0.5, GeneTherapy = 0.5),
    "80% DUP / 20% Gene" = c(DUP = 0.8, GeneTherapy = 0.2)
  ),

  scenarios = list(
    "Base case" = list(),
    "No discounting" = list(d_cost = 0, d_qaly = 0),
    "Longer horizon (5yr)" = list(time_horizon = 5),
    "Permanent cure" = list(gene_therapy_cure_cycles = 9999),
    "10-year cure" = list(gene_therapy_cure_cycles = 120),
    "Cheap gene therapy ($50k)" = list(c_gene_therapy_one_time = 50000),
    "Expensive gene therapy ($200k)" = list(c_gene_therapy_one_time = 200000)
  )
)

# ===========================================================================
# Launch
# ===========================================================================
cat("Asthma model loaded (pure Markov, no twig dependency).\n")
print(asthma_model)
run_cea_app(asthma_model)
