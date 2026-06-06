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
- **Configurable color palettes** — 3 presets (UW, Classic, Lancet) or fully custom colors
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
