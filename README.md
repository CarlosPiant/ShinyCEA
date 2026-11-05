# ShinyCEA

<!-- badges: start -->
[![R-CMD-check](https://github.com/CarlosPiant/ShinyCEA/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/CarlosPiant/ShinyCEA/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

An R package with an interactive Shiny application for **cost-effectiveness analysis (CEA)**. Designed for teaching and applied research, ShinyCEA supports multiple model structures and allows analysts to perform deterministic and probabilistic evaluations with minimal setup.

## Features

- **Plug-in model interface** — define your model with three functions (`info`, `params`, `simulate`) and the app handles everything else
- **Multiple model structures** — Markov cohort, partitioned survival, or any custom structure
- **Deterministic analysis** — results table, dominance/extended dominance frontier, tornado diagram
- **Probabilistic sensitivity analysis** — CE plane, CEAC, parameter draw distributions
- **Threshold analysis** — ICER = WTP and INB = 0 crossing detection
- **Survival and trace plots** — automatic derivation from model outputs
- **Configurable color palettes** — 4 presets (ShinyModels cream, UW, Classic, Lancet) or fully custom colors
- **Shared design with ShinyModels** — the app uses the same cream / navy / teal look as the [ShinyModels](https://github.com/CarlosPiant/ShinyModels) editor, whose "Analyze in ShinyCEA" button opens it in a new tab
- **Report generation** — Word and PDF reports with all tables and figures
- **CSV import/export** — upload parameters, download all results

## Installation

```r
# install.packages("pak")
pak::pak("CarlosPiant/ShinyCEA")
```

## Quick Start

```r
library(ShinyCEA)

# Launch with bundled example models
run_cea_app()

# Launch with a custom model
my_model <- new_cea_model(
  info = function() list(
    key = "my_model", name = "My Model",
    strategies = c("control", "treatment")
  ),
  params = function() rbind(
    cea_param("p_event", 0.1, 0.05, 0.2, distribution = "beta"),
    cea_param("rr_tx",   0.7, 0.5,  0.9, distribution = "lognormal"),
    cea_param("c_tx",  5000, 3000, 8000,  distribution = "gamma"),
    cea_param("u_well",  0.9, 0.8,  1.0,  distribution = "beta")
  ),
  simulate = function(pars, strategy) {
    p <- pars[["p_event"]]
    if (strategy == "treatment") p <- p * pars[["rr_tx"]]
    cost <- if (strategy == "treatment") pars[["c_tx"]] else 0
    qaly <- (1 - p) * pars[["u_well"]]
    list(cost = cost, qaly = qaly)
  }
)

run_cea_app(my_model)
```

## Example: Build a Sick-Sicker Markov Model and Run the App

This example builds the Sick-Sicker model from scratch: a 4-state cohort
Markov model (Healthy → Sick → Sicker → Dead) comparing standard care with
four treatments. It is a compact version of the bundled `cea_sick_sicker()`
and gives the same base-case results.

```r
library(ShinyCEA)

strategies <- c("std", "t1", "t2", "t3", "t4")

sick_sicker <- new_cea_model(
  # 1. Model identity and the strategies to compare
  info = function() list(
    key = "my_sick_sicker",
    name = "My Sick-Sicker (4-state Markov)",
    strategies = strategies
  ),

  # 2. Parameters: cea_param(name, mean, lower, upper, distribution)
  params = function() rbind(
    # Transition probabilities per 3-month cycle under standard care
    cea_param("p_HS_std",   0.04,  0.02,  0.08, distribution = "beta"),
    cea_param("p_HD_std",   0.005, 0.002, 0.01, distribution = "beta"),
    cea_param("p_S1S2_std", 0.06,  0.03,  0.12, distribution = "beta"),
    cea_param("p_S1D_std",  0.02,  0.01,  0.04, distribution = "beta"),
    cea_param("p_S2D_std",  0.08,  0.04,  0.15, distribution = "beta"),
    # Treatment effects: relative risks of progression and of death
    cea_param("rr_prog_t1",  0.85, 0.70, 0.98, distribution = "lognormal"),
    cea_param("rr_prog_t2",  0.75, 0.60, 0.92, distribution = "lognormal"),
    cea_param("rr_prog_t3",  0.90, 0.75, 1.05, distribution = "lognormal"),
    cea_param("rr_prog_t4",  0.65, 0.50, 0.85, distribution = "lognormal"),
    cea_param("rr_death_t1", 0.95, 0.80, 1.05, distribution = "lognormal"),
    cea_param("rr_death_t2", 0.90, 0.75, 1.02, distribution = "lognormal"),
    cea_param("rr_death_t3", 1.00, 0.85, 1.10, distribution = "lognormal"),
    cea_param("rr_death_t4", 0.88, 0.70, 1.00, distribution = "lognormal"),
    # Utilities and annual costs by health state
    cea_param("u_H",  0.90, 0.85, 0.95, distribution = "beta"),
    cea_param("u_S1", 0.75, 0.65, 0.85, distribution = "beta"),
    cea_param("u_S2", 0.55, 0.45, 0.70, distribution = "beta"),
    cea_param("c_H",   200,  100,  300, distribution = "gamma"),
    cea_param("c_S1",  800,  500, 1200, distribution = "gamma"),
    cea_param("c_S2", 2200, 1500, 3500, distribution = "gamma"),
    # Annual drug cost of each treatment (standard care has none)
    cea_param("drug_t1", 400, 200,  600, distribution = "gamma"),
    cea_param("drug_t2", 600, 300,  900, distribution = "gamma"),
    cea_param("drug_t3", 300, 150,  500, distribution = "gamma"),
    cea_param("drug_t4", 700, 400, 1000, distribution = "gamma"),
    # Structural settings: fixed, so excluded from the PSA
    cea_param("time_horizon", 10, 5, 15, distribution = "fixed",
              include_psa = FALSE, include_tornado = FALSE),
    cea_param("d_cost", 0.03, 0, 0.08, distribution = "fixed", include_psa = FALSE),
    cea_param("d_qaly", 0.03, 0, 0.08, distribution = "fixed", include_psa = FALSE)
  ),

  # 3. Simulation: one strategy at a time, returns its total cost and QALYs
  simulate = function(pars, strategy) {
    cycle_length <- 0.25                      # years (3-month cycles)
    n_cycles <- round(pars[["time_horizon"]] / cycle_length)

    # Treatments scale the standard-care probabilities by their relative risks
    rr_prog  <- if (strategy == "std") 1 else pars[[paste0("rr_prog_", strategy)]]
    rr_death <- if (strategy == "std") 1 else pars[[paste0("rr_death_", strategy)]]
    drug     <- if (strategy == "std") 0 else pars[[paste0("drug_", strategy)]]

    p_HS   <- pars[["p_HS_std"]] * rr_prog
    p_HD   <- pars[["p_HD_std"]]
    p_S1S2 <- pars[["p_S1S2_std"]] * rr_prog
    p_S1D  <- pars[["p_S1D_std"]] * rr_death
    p_S2D  <- pars[["p_S2D_std"]] * rr_death

    # Transition matrix (rows = from, columns = to)
    states <- c("H", "S1", "S2", "D")
    P <- rbind(
      c(1 - p_HS - p_HD, p_HS,              0,         p_HD),
      c(0,               1 - p_S1S2 - p_S1D, p_S1S2,    p_S1D),
      c(0,               0,                 1 - p_S2D, p_S2D),
      c(0,               0,                 0,         1)
    )

    # Cohort trace: everyone starts Healthy
    trace <- matrix(0, n_cycles + 1, 4, dimnames = list(NULL, states))
    trace[1, ] <- c(1, 0, 0, 0)
    for (t in seq_len(n_cycles)) trace[t + 1, ] <- trace[t, ] %*% P

    # Discounted costs and QALYs from state occupancy
    occ <- trace[-1, ]
    utilities <- c(pars[["u_H"]], pars[["u_S1"]], pars[["u_S2"]], 0)
    # Everyone alive pays the drug cost on top of the state cost
    costs     <- c(pars[["c_H"]] + drug, pars[["c_S1"]] + drug, pars[["c_S2"]] + drug, 0)
    w_cost <- cea_discount_weights(n_cycles, pars[["d_cost"]], cycle_length)
    w_qaly <- cea_discount_weights(n_cycles, pars[["d_qaly"]], cycle_length)

    list(
      cost  = sum((occ %*% costs) * w_cost) * cycle_length,
      qaly  = sum((occ %*% utilities) * w_qaly) * cycle_length,
      # Optional: enables the Traces and Survival tabs in the app
      trace = trace,
      survival = data.frame(time = seq(0, n_cycles) * cycle_length,
                            surv = 1 - trace[, "D"], curve = "OS")
    )
  }
)

# Check the model from the console
det <- run_deterministic(sick_sicker, sick_sicker$default_params(), wtp = 100000)
det$table

# Open it in the app
run_cea_app(sick_sicker)
```

In the app, click **Run Deterministic** and **Run PSA** in the sidebar, then
browse the tabs (Deterministic, PSA, CEAC, VOI, Traces, ...). To compare it
side by side with the bundled version, pass both and use the model selector:

```r
run_cea_app(sick_sicker, cea_sick_sicker())
```

## Bundled Example Models

| Model | Function | Type |
|-------|----------|------|
| Sick-Sicker (4-state) | `cea_sick_sicker()` | Markov cohort |
| PF → Progressed → Death | `cea_pf_prog_death()` | Markov cohort |
| Partitioned Survival (Weibull) | `cea_partitioned_survival()` | PartSA |

## Programmatic Use

All analysis functions work outside the Shiny app:

```r
m <- cea_sick_sicker()

# Deterministic
det <- run_deterministic(m, m$default_params(), wtp = 100000)

# PSA
psa <- run_psa(m, m$default_params(), n_sim = 1000)

# Plots
plot_frontier(det$frontier_status, palette = cea_palette("lancet"))
plot_ceac(compute_ceac(psa$results, m$info()$strategies), palette = cea_palette("lancet"))

# Report
generate_cea_report("report.docx", format = "word", model = m, det_result = det)
```

## Documentation

See `vignette("custom-models", package = "ShinyCEA")` for a detailed guide on creating custom models.
