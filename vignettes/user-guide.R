## ----setup, include = FALSE---------------------------------------------------
# The app cannot run while the vignette is built, so its code is shown but not
# evaluated; the screenshots in figures/ were taken from the running app with
# scripts/R/make_user_guide_screenshots.R (Sick-Sicker model, default seed).
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", out.width = "100%")

## ----launch, eval = FALSE-----------------------------------------------------
# # install.packages("pak")
# pak::pak("CarlosPiant/ShinyCEA")
# 
# library(ShinyCEA)
# run_cea_app(cea_sick_sicker())

## ----echo = FALSE, fig.alt = "The app after launch: sidebar on the left, Model Info tab with the parameter dictionary on the right"----
knitr::include_graphics("figures/guide-01-start.png")

## ----echo = FALSE, fig.alt = "Parameters tab with the editable parameter table"----
knitr::include_graphics("figures/guide-02-parameters.png")

## ----echo = FALSE, fig.alt = "Deterministic tab: reference strategy, results table and frontier table"----
knitr::include_graphics("figures/guide-03-deterministic.png")

## ----echo = FALSE, out.width = "70%", fig.alt = "Tornado diagram of the incremental net benefit of std versus t4"----
knitr::include_graphics("figures/guide-04-tornado.png")

## ----echo = FALSE, fig.alt = "Traces tab: state occupancy over time for each strategy"----
knitr::include_graphics("figures/guide-09-traces.png")

## ----echo = FALSE, fig.alt = "PSA tab: cost-effectiveness plane"--------------
knitr::include_graphics("figures/guide-05-psa.png")

## ----echo = FALSE, fig.alt = "CEAC tab with the acceptability frontier superimposed"----
knitr::include_graphics("figures/guide-06-ceac.png")

## ----echo = FALSE, out.width = "75%", fig.alt = "Expected loss curves with the optimal strategy marked"----
knitr::include_graphics("figures/guide-07-expected-loss.png")

## ----echo = FALSE, out.width = "75%", fig.alt = "EVPPI by parameter"----------
knitr::include_graphics("figures/guide-08-evppi.png")

## ----echo = FALSE, fig.alt = "Scenarios tab after running: results for each scenario and strategy"----
knitr::include_graphics("figures/guide-10-scenarios.png")

## ----echo = FALSE, out.width = "75%", fig.alt = "Forest plot of the ICER of t1 versus std by subgroup"----
knitr::include_graphics("figures/guide-12-subgroups.png")

## ----echo = FALSE, fig.alt = "BIA tab: population, projection years and market shares"----
knitr::include_graphics("figures/guide-11-bia.png")

## ----echo = FALSE, fig.alt = "Study tab: form with the study details"---------
knitr::include_graphics("figures/guide-14-study.png")

## ----echo = FALSE, out.width = "70%", fig.alt = "CHEERS 2022 checklist with the status of each item"----
knitr::include_graphics("figures/guide-15-cheers.png")

## ----echo = FALSE, fig.alt = "The R code window for the CEAC tab"-------------
knitr::include_graphics("figures/guide-13-code.png")

## ----script, eval = FALSE-----------------------------------------------------
# library(ShinyCEA)
# 
# model  <- cea_sick_sicker()
# params <- sanitize_params(model$default_params())$df
# wtp    <- 100000
# 
# # Base case
# det <- run_deterministic(model, params, wtp = wtp)
# det$table
# 
# # Probabilistic analysis, CEAC and expected loss
# set.seed(20250605)
# psa  <- run_psa(model, params, n_sim = 1000)
# ceac <- compute_ceac(psa$results, det$strategies)
# plot_ceac(ceac, ceaf = compute_ceaf(psa$results, det$strategies))
# plot_expected_loss(compute_expected_loss(psa$results, det$strategies), show_optimal = TRUE)

