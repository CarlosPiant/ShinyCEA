# =============================================================================
# PSMA-2DG-D Cost-Effectiveness Analysis — ShinyCEA Implementation
#
# Replicates the 3-state partitioned survival model from:
# "Cost-Effectiveness of PSMA-Targeted 2-Deoxyglucose-Based Dendrimer
#  Nanomedicine in Post-ARPI mCRPC" (Pineda, UW 2026)
#
# States: PFS, Progressed Disease (PD), Death
# Strategies: BSC, Cabozantinib, Cabazitaxel, Lu-177 PSMA-617, PSMA-2DG-D
# Baseline: Weibull OS/PFS from VISION control arm
# Treatment effects: Proportional hazards via Bucher ITC
# =============================================================================

library(ShinyCEA)

psma_model <- new_cea_model(

  info = function() {
    list(
      key = "psma2dgd_mcrpc",
      name = "PSMA-2DG-D in Post-ARPI mCRPC (Partitioned Survival)",
      strategies = c("BSC", "Cabozantinib", "Cabazitaxel", "Lu177_PSMA", "PSMA_2DGD")
    )
  },

  params = function() {
    # Define all model parameters with their distributions for PSA and tornado analysis.
    do.call(rbind, list(
      # --- Structural ---
      cea_param("time_horizon", 20, 5, 30,
                distribution = "fixed", include_psa = FALSE, include_tornado = FALSE),
      cea_param("cycle_length", 1/12, 1/12, 1/12,
                distribution = "fixed", include_psa = FALSE, include_tornado = FALSE),
      cea_param("d_cost", 0.03, 0.0, 0.05,
                distribution = "fixed", include_psa = FALSE, include_tornado = TRUE),
      cea_param("d_qaly", 0.03, 0.0, 0.05,
                distribution = "fixed", include_psa = FALSE, include_tornado = TRUE),

      # --- Baseline Weibull (BSC, VISION control arm) ---
      cea_param("pfs_shape", 1.10, 0.90, 1.30, distribution = "lognormal"),
      cea_param("pfs_scale", 0.35, 0.28, 0.42, distribution = "lognormal"),
      cea_param("os_shape", 1.20, 1.00, 1.40, distribution = "lognormal"),
      cea_param("os_scale", 1.05, 0.85, 1.25, distribution = "lognormal"),

      # --- HR(OS) vs BSC ---
      cea_param("hr_os_BSC", 1.00, 1.00, 1.00,
                distribution = "fixed", include_psa = FALSE, include_tornado = FALSE),
      cea_param("hr_os_Cabozantinib", 0.73, 0.60, 0.90, distribution = "lognormal"),
      cea_param("hr_os_Cabazitaxel", 0.72, 0.58, 0.88, distribution = "lognormal"),
      cea_param("hr_os_Lu177_PSMA", 0.62, 0.50, 0.75, distribution = "lognormal"),
      cea_param("hr_os_PSMA_2DGD", 0.60, 0.45, 0.75, distribution = "lognormal"),

      # --- HR(PFS) vs BSC ---
      cea_param("hr_pfs_BSC", 1.00, 1.00, 1.00,
                distribution = "fixed", include_psa = FALSE, include_tornado = FALSE),
      cea_param("hr_pfs_Cabozantinib", 0.48, 0.35, 0.65, distribution = "lognormal"),
      cea_param("hr_pfs_Cabazitaxel", 0.74, 0.58, 0.92, distribution = "lognormal"),
      cea_param("hr_pfs_Lu177_PSMA", 0.40, 0.30, 0.55, distribution = "lognormal"),
      cea_param("hr_pfs_PSMA_2DGD", 0.50, 0.35, 0.65, distribution = "lognormal"),

      # --- Discontinuation rate per cycle ---
      cea_param("disc_BSC", 0.00, 0.00, 0.00,
                distribution = "fixed", include_psa = FALSE, include_tornado = FALSE),
      cea_param("disc_Cabozantinib", 0.033, 0.015, 0.055, distribution = "beta"),
      cea_param("disc_Cabazitaxel", 0.050, 0.025, 0.075, distribution = "beta"),
      cea_param("disc_Lu177_PSMA", 0.040, 0.020, 0.065, distribution = "beta"),
      cea_param("disc_PSMA_2DGD", 0.015, 0.005, 0.030, distribution = "beta"),

      # --- Drug costs (monthly) ---
      cea_param("c_drug_BSC", 500, 300, 700, distribution = "gamma"),
      cea_param("c_drug_Cabozantinib", 18700, 12000, 25000, distribution = "gamma"),
      cea_param("c_drug_Cabazitaxel", 11000, 7000, 15000, distribution = "gamma"),
      cea_param("c_drug_Lu177_PSMA", 45000, 30000, 60000, distribution = "gamma"),
      cea_param("c_drug_PSMA_2DGD", 26000, 15000, 40000, distribution = "gamma"),

      # --- Administration costs (monthly) ---
      cea_param("c_admin_BSC", 200, 200, 200,
                distribution = "fixed", include_psa = FALSE, include_tornado = FALSE),
      cea_param("c_admin_Cabozantinib", 300, 300, 300,
                distribution = "fixed", include_psa = FALSE, include_tornado = FALSE),
      cea_param("c_admin_Cabazitaxel", 800, 800, 800,
                distribution = "fixed", include_psa = FALSE, include_tornado = FALSE),
      cea_param("c_admin_Lu177_PSMA", 1500, 1500, 1500,
                distribution = "fixed", include_psa = FALSE, include_tornado = FALSE),
      cea_param("c_admin_PSMA_2DGD", 300, 300, 300,
                distribution = "fixed", include_psa = FALSE, include_tornado = FALSE),

      # --- PSMA-PET monitoring (PSMA-2DG-D only) ---
      cea_param("c_monitoring_PSMA_2DGD", 333, 200, 500, distribution = "gamma"),

      # --- Health state costs ---
      cea_param("c_PFS", 3000, 1800, 4200, distribution = "gamma"),
      cea_param("c_PD", 5500, 3300, 7700, distribution = "gamma"),
      cea_param("c_terminal", 50000, 50000, 50000,
                distribution = "fixed", include_psa = FALSE, include_tornado = FALSE),

      # --- AE rates per cycle (total Grade >=3) ---
      cea_param("ae_rate_BSC", 0.08, 0.04, 0.12, distribution = "beta"),
      cea_param("ae_rate_Cabozantinib", 0.28, 0.20, 0.36, distribution = "beta"),
      cea_param("ae_rate_Cabazitaxel", 0.31, 0.22, 0.40, distribution = "beta"),
      cea_param("ae_rate_Lu177_PSMA", 0.11, 0.06, 0.16, distribution = "beta"),
      cea_param("ae_rate_PSMA_2DGD", 0.10, 0.05, 0.15, distribution = "beta"),

      # --- AE cost per event (weighted average) ---
      cea_param("c_ae_event", 5000, 3000, 8000, distribution = "gamma"),

      # --- AE disutility per event ---
      cea_param("du_ae_event", 0.05, 0.02, 0.10, distribution = "beta"),

      # --- Utilities ---
      cea_param("u_PFS", 0.68, 0.58, 0.78, distribution = "beta"),
      cea_param("u_PD", 0.52, 0.40, 0.64, distribution = "beta")
    ))
  },

  simulate = function(pars, strategy) {
    strategy <- match.arg(strategy,
      c("BSC", "Cabozantinib", "Cabazitaxel", "Lu177_PSMA", "PSMA_2DGD"))
    .g <- function(nm, d) { v <- pars[[nm]]; if (is.null(v)) d else as.numeric(v) }

    horizon <- .g("time_horizon", 20)
    cl <- .g("cycle_length", 1/12)
    n_cycles <- as.integer(round(horizon / cl))
    tgrid <- seq(0, horizon, by = cl)

    # Baseline Weibull
    pfs_sh <- .g("pfs_shape", 1.10)
    pfs_sc <- .g("pfs_scale", 0.35)
    os_sh <- .g("os_shape", 1.20)
    os_sc <- .g("os_scale", 1.05)

    # HRs for this strategy
    hr_os <- .g(paste0("hr_os_", strategy), 1)
    hr_pfs <- .g(paste0("hr_pfs_", strategy), 1)

    # PH adjustment: scale_tx = scale_bsc / HR^(1/shape)
    pfs_sc_tx <- pfs_sc / hr_pfs^(1 / pfs_sh)
    os_sc_tx <- os_sc / hr_os^(1 / os_sh)

    # Weibull S(t)
    S_pfs <- exp(-(pmax(tgrid, 0) / pfs_sc_tx)^pfs_sh)
    S_os <- exp(-(pmax(tgrid, 0) / os_sc_tx)^os_sh)

    # Ensure OS >= PFS
    S_os <- pmax(S_os, S_pfs)

    # State occupancy
    PF <- pmax(S_pfs, 0)
    PD <- pmax(S_os - S_pfs, 0)
    Death <- pmin(pmax(1 - S_os, 0), 1)

    # Half-cycle correction (trapezoidal)
    n_t <- length(tgrid)
    PF_hcc <- (PF[-n_t] + PF[-1]) / 2
    PD_hcc <- (PD[-n_t] + PD[-1]) / 2
    Death_hcc <- (Death[-n_t] + Death[-1]) / 2

    # Treatment discontinuation
    disc_rate <- .g(paste0("disc_", strategy), 0)
    on_tx <- exp(-disc_rate * seq_len(n_cycles))  # fraction still on treatment

    # Costs
    c_drug <- .g(paste0("c_drug_", strategy), 0)
    c_admin <- .g(paste0("c_admin_", strategy), 0)
    c_monitor <- if (strategy == "PSMA_2DGD") .g("c_monitoring_PSMA_2DGD", 333) else 0
    c_drug_bsc <- .g("c_drug_BSC", 500)
    c_admin_bsc <- .g("c_admin_BSC", 200)
    c_pfs <- .g("c_PFS", 3000)
    c_pd <- .g("c_PD", 5500)
    c_terminal <- .g("c_terminal", 50000)

    ae_rate <- .g(paste0("ae_rate_", strategy), 0)
    c_ae <- .g("c_ae_event", 5000)

    # Per-cycle drug+admin cost: on_tx fraction pays active drug, rest pays BSC
    cost_drug_per_cycle <- on_tx * (c_drug + c_admin + c_monitor) +
                           (1 - on_tx) * (c_drug_bsc + c_admin_bsc)

    # Per-cycle AE cost (only while on treatment)
    cost_ae_per_cycle <- on_tx * ae_rate * c_ae

    # Per-cycle total cost
    cost_per_cycle <- PF_hcc * (cost_drug_per_cycle + c_pfs + cost_ae_per_cycle) +
                      PD_hcc * c_pd

    # Terminal care cost (applied to new deaths each cycle)
    new_deaths <- diff(Death)
    new_deaths <- pmax(new_deaths, 0)
    cost_per_cycle <- cost_per_cycle + new_deaths * c_terminal

    # Utilities
    u_pfs <- .g("u_PFS", 0.68)
    u_pd <- .g("u_PD", 0.52)
    du_ae <- .g("du_ae_event", 0.05)

    # Per-cycle QALY (annual utility * cycle_length)
    qaly_per_cycle <- (PF_hcc * (u_pfs - on_tx * ae_rate * du_ae) +
                       PD_hcc * u_pd) * cl

    # Discounting
    d_c <- .g("d_cost", 0.03)
    d_q <- .g("d_qaly", 0.03)
    w_c <- cea_discount_weights(n_cycles, rate = d_c, cycle_length = cl)
    w_q <- cea_discount_weights(n_cycles, rate = d_q, cycle_length = cl)

    cost <- sum(cost_per_cycle * w_c)
    qaly <- sum(qaly_per_cycle * w_q)

    # Survival curves for the app
    survival <- rbind(
      data.frame(time = tgrid, surv = S_os, curve = "OS", stringsAsFactors = FALSE),
      data.frame(time = tgrid, surv = S_pfs, curve = "PFS", stringsAsFactors = FALSE)
    )

    list(cost = as.numeric(cost), qaly = as.numeric(qaly),
         trace = NULL, survival = survival)
  },

  description = paste(
    "Three-state partitioned survival model for PSMA-targeted 2-deoxyglucose-based",
    "dendrimer nanomedicine (PSMA-2DG-D) in post-ARPI metastatic castration-resistant",
    "prostate cancer (mCRPC). Compares PSMA-2DG-D against BSC, cabozantinib, cabazitaxel,",
    "and Lu-177 PSMA-617. Weibull baseline survival from VISION control arm with",
    "proportional-hazards treatment effects via Bucher indirect comparison.",
    "Includes treatment discontinuation, adverse-event costs/disutilities,",
    "and terminal care costs. Monthly cycles, 20-year horizon, 3% discounting.",
    "US healthcare payer perspective, 2026 USD."
  ),

  # --- Predefined Weighted Composites (from Table 8, market shares) ---
  composites = list(
    "PSMA-2DG-D" = c(PSMA_2DGD = 1),
    "Post-ARPI Basket (US 2024)" = c(BSC = 0.10, Cabozantinib = 0.30,
                                      Cabazitaxel = 0.25, Lu177_PSMA = 0.35),
    "High Lu-177 share (55%)" = c(BSC = 0.05, Cabozantinib = 0.20,
                                   Cabazitaxel = 0.20, Lu177_PSMA = 0.55),
    "Low Lu-177 share (15%)" = c(BSC = 0.15, Cabozantinib = 0.40,
                                  Cabazitaxel = 0.30, Lu177_PSMA = 0.15),
    "Cabo-dominant market" = c(BSC = 0.10, Cabozantinib = 0.60,
                                Cabazitaxel = 0.20, Lu177_PSMA = 0.10),
    "PSMA-2DG-D protocol" = c(PSMA_2DGD = 0.80, BSC = 0.20)
  ),

  # --- Predefined Scenarios (from Tables 9-10) ---
  scenarios = list(
    "Base case" = list(),
    "No discounting" = list(d_cost = 0, d_qaly = 0),
    "Optimistic HR (0.45)" = list(hr_os_PSMA_2DGD = 0.45),
    "Conservative HR (0.75)" = list(hr_os_PSMA_2DGD = 0.75),
    "Target price ($16,900)" = list(c_drug_PSMA_2DGD = 16900),
    "Low price ($11,000)" = list(c_drug_PSMA_2DGD = 11000),
    "High price ($40,000)" = list(c_drug_PSMA_2DGD = 40000)
  ),

  # --- Predefined Subgroups ---
  subgroups = list(
    "Overall" = list(),
    "Strong responders (HR OS 0.45)" = list(hr_os_PSMA_2DGD = 0.45,
                                             hr_pfs_PSMA_2DGD = 0.35),
    "Moderate responders (HR OS 0.60)" = list(hr_os_PSMA_2DGD = 0.60,
                                               hr_pfs_PSMA_2DGD = 0.50),
    "Weak responders (HR OS 0.75)" = list(hr_os_PSMA_2DGD = 0.75,
                                           hr_pfs_PSMA_2DGD = 0.65),
    "High AE burden" = list(ae_rate_PSMA_2DGD = 0.20),
    "Low discontinuation" = list(disc_PSMA_2DGD = 0.005)
  )
)

# ===========================================================================
# Verification: replicate base-case results from the report
# ===========================================================================
cat("\n=== PSMA-2DG-D Model Verification ===\n")
print(psma_model)

det <- run_deterministic(psma_model, psma_model$default_params(), wtp = 100000)
cat("\nDeterministic results (WTP = $100,000/QALY):\n")
cat("Reference:", det$ref, "\n\n")

# Display results matching Table 4 format
for (s in det$strategies) {
  row <- det$table[det$table$Strategy == s, ]
  cat(sprintf("%-15s  Cost: $%9s  QALY: %.3f  NMB: $%9s\n",
              s,
              formatC(round(row$Cost), format = "f", big.mark = ",", digits = 0),
              row$QALY,
              formatC(round(row$NMB), format = "f", big.mark = ",", digits = 0)))
}

# Pairwise ICER vs PSMA-2DG-D
cat("\nPairwise ICERs (PSMA-2DG-D vs each comparator):\n")
psma_cost <- det$table$Cost[det$table$Strategy == "PSMA_2DGD"]
psma_qaly <- det$table$QALY[det$table$Strategy == "PSMA_2DGD"]
for (s in setdiff(det$strategies, "PSMA_2DGD")) {
  comp_cost <- det$table$Cost[det$table$Strategy == s]
  comp_qaly <- det$table$QALY[det$table$Strategy == s]
  dc <- psma_cost - comp_cost
  dq <- psma_qaly - comp_qaly
  icer <- if (dq > 1e-6) dc / dq else if (dc < 0) -Inf else Inf
  cat(sprintf("  vs %-15s  dCost: $%9s  dQALY: %.3f  ICER: %s\n",
              s,
              formatC(round(dc), format = "f", big.mark = ",", digits = 0),
              dq,
              if (is.finite(icer)) paste0("$", formatC(round(icer), format = "f",
                big.mark = ",", digits = 0), "/QALY")
              else if (dc < 0) "Cost-saving" else "Dominated"))
}

# Weighted basket comparison
cat("\n=== Weighted Basket Analysis ===\n")
composites <- list(
  "PSMA-2DG-D" = c(PSMA_2DGD = 1),
  "Weighted Basket" = c(BSC = 0.10, Cabozantinib = 0.30,
                        Cabazitaxel = 0.25, Lu177_PSMA = 0.35)
)
wt <- run_weighted_analysis(psma_model, psma_model$default_params(),
                            composites, wtp = 100000)
cat("\nComposite results:\n")
print(wt$composites)
cat("\nPairwise:\n")
print(wt$pairwise[, c("Reference", "Comparator", "IncCost", "IncQALY", "ICER")])

# ===========================================================================
# Launch the app
# ===========================================================================
cat("\n=== Launching ShinyCEA ===\n")
run_cea_app(psma_model)
