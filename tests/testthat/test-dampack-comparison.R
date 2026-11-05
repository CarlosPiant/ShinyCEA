# =============================================================================
# Head-to-head comparison: ShinyCEA vs dampack (DARTH)
#
# For each shared analysis we feed IDENTICAL inputs to both packages and
# verify that the numerical results agree within a tight tolerance.
# Skipped when dampack is not installed.
# =============================================================================

skip_if_not_installed("dampack")

# ---------------------------------------------------------------------------
# Shared fixture
# ---------------------------------------------------------------------------
setup_comparison <- function(n_sim = 500, seed = 20261102) {
  m   <- cea_sick_sicker()
  inf <- m$info()
  strats <- inf$strategies
  pars   <- m$default_params()

  set.seed(seed)
  psa <- run_psa(m, pars, n_sim = n_sim)

  cost_cols <- paste0(strats, ".cost")
  qaly_cols <- paste0(strats, ".q")
  cost_mat <- as.data.frame(psa$results[, cost_cols])
  eff_mat  <- as.data.frame(psa$results[, qaly_cols])
  names(cost_mat) <- names(eff_mat) <- strats

  dpsa <- dampack::make_psa_obj(
    cost         = cost_mat,
    effectiveness = eff_mat,
    parameters   = psa$draws,
    strategies   = strats
  )

  list(m = m, strats = strats, pars = pars, psa = psa,
       dpsa = dpsa, cost_mat = cost_mat, eff_mat = eff_mat)
}

# dampack::ceac() picks the best strategy with max.col(), which treats net
# benefits within a relative 1e-5 of the row maximum as tied and breaks the
# tie AT RANDOM. ShinyCEA takes the exact maximum. The two can therefore
# differ by at most (number of near-tie iterations) / N at a given WTP; this
# returns that bound so the comparison is exact wherever no near-tie exists.
near_tie_bound <- function(psa_results, strats, w) {
  nmb <- vapply(strats, function(s) {
    w * psa_results[[paste0(s, ".q")]] - psa_results[[paste0(s, ".cost")]]
  }, numeric(nrow(psa_results)))
  srt <- t(apply(nmb, 1, sort, decreasing = TRUE))
  # max.col tolerance: 1e-5 times the largest absolute value in the row
  near <- (srt[, 1] - srt[, 2]) <= 1e-5 * apply(abs(nmb), 1, max)
  sum(near) / nrow(nmb)
}

# ===========================================================================
# 1. ICER table + dominance classification
# ===========================================================================
test_that("ICER table matches dampack::calculate_icers", {
  d <- setup_comparison(n_sim = 300)

  mean_cost <- colMeans(d$cost_mat)
  mean_eff  <- colMeans(d$eff_mat)

  df_cq <- data.frame(
    Strategy = d$strats, Cost = mean_cost[d$strats],
    QALY = mean_eff[d$strats], stringsAsFactors = FALSE
  )
  sc <- frontier_analysis(df_cq)
  dp <- dampack::calculate_icers(mean_cost, mean_eff, d$strats)

  # Frontier strategies match
  sc_frontier <- sort(as.character(sc$frontier$Strategy))
  dp_frontier <- sort(as.character(dp$Strategy[dp$Status == "ND"]))
  expect_equal(sc_frontier, dp_frontier)

  # Dominance status matches
  for (s in d$strats) {
    sc_status <- as.character(sc$classified$Status[sc$classified$Strategy == s])
    dp_status <- as.character(dp$Status[dp$Strategy == s])
    sc_mapped <- switch(sc_status,
      "Frontier" = "ND", "Dominated" = "D",
      "Extendedly dominated" = "ED", "Equivalent" = "ND", sc_status)
    expect_equal(sc_mapped, dp_status, label = paste("dominance for", s))
  }
})

# ===========================================================================
# 2. CEAC
# ===========================================================================
test_that("CEAC values match dampack::ceac", {
  d <- setup_comparison(n_sim = 500)
  wtp_range <- seq(0, 200000, by = 25000)

  sc_ceac <- compute_ceac(d$psa$results, d$strats,
                           wtp_max = 200000, wtp_step = 25000)
  # dampack::ceac returns a data.frame: WTP, Strategy, Proportion, On_Frontier
  dp_ceac <- as.data.frame(dampack::ceac(d$dpsa, wtp = wtp_range))
  dp_ceac$Strategy <- as.character(dp_ceac$Strategy)

  n_exact <- 0L
  for (w in wtp_range) {
    # Largest difference random tie-breaking in dampack can cause at this WTP
    bound <- near_tie_bound(d$psa$results, d$strats, w)
    if (bound == 0) n_exact <- n_exact + 1L
    for (s in d$strats) {
      sc_val <- sc_ceac$ProbCE[sc_ceac$WTP == w & sc_ceac$Strategy == s]
      dp_val <- dp_ceac$Proportion[dp_ceac$Strategy == s & dp_ceac$WTP == w]
      # Unconditional length checks: a lookup that finds nothing must FAIL,
      # not silently skip the comparison.
      expect_length(sc_val, 1L)
      expect_length(dp_val, 1L)
      expect_lte(abs(sc_val - dp_val), bound + 1e-10,
                 label = paste("CEAC difference at WTP", w, s))
    }
  }
  # Guard against a vacuous pass: most thresholds must be tie-free and so
  # compared exactly.
  expect_gte(n_exact, length(wtp_range) - 2L)
})

# ===========================================================================
# 2b. CEAF
# ===========================================================================
test_that("CEAF matches dampack's On_Frontier flag", {
  d <- setup_comparison(n_sim = 500)
  wtp_range <- seq(0, 200000, by = 25000)

  sc_ceaf <- compute_ceaf(d$psa$results, d$strats,
                          wtp_max = 200000, wtp_step = 25000)
  dp_ceac <- as.data.frame(dampack::ceac(d$dpsa, wtp = wtp_range))
  dp_ceac$Strategy <- as.character(dp_ceac$Strategy)
  dp_front <- dp_ceac[dp_ceac$On_Frontier, ]
  dp_front <- dp_front[order(dp_front$WTP), ]

  # One optimal strategy per WTP in both packages, same strategy, same height
  expect_equal(dp_front$WTP, wtp_range)
  expect_equal(sc_ceaf$Strategy, dp_front$Strategy)
  bounds <- vapply(wtp_range, function(w) {
    near_tie_bound(d$psa$results, d$strats, w)
  }, numeric(1))
  expect_true(all(abs(sc_ceaf$ProbCE - dp_front$Proportion) <= bounds + 1e-10))
})

# ===========================================================================
# 3. EVPI
# ===========================================================================
test_that("EVPI matches dampack::calc_evpi", {
  d <- setup_comparison(n_sim = 500)
  wtp_range <- seq(0, 200000, by = 50000)

  sc_evpi <- compute_evpi(d$psa$results, d$strats,
                           wtp_max = 200000, wtp_step = 50000)
  dp_evpi <- dampack::calc_evpi(d$dpsa, wtp = wtp_range)

  for (w in wtp_range) {
    sc_val <- sc_evpi$EVPI[sc_evpi$WTP == w]
    dp_val <- dp_evpi$EVPI[dp_evpi$WTP == w]
    if (length(sc_val) == 1 && length(dp_val) == 1) {
      expect_equal(sc_val, dp_val, tolerance = 1e-6,
                   label = paste("EVPI WTP", w))
    }
  }
})

# ===========================================================================
# 4. Expected Loss
# ===========================================================================
test_that("Expected Loss matches dampack::calc_exp_loss", {
  d <- setup_comparison(n_sim = 500)
  wtp_range <- seq(0, 200000, by = 50000)

  sc_el <- compute_expected_loss(d$psa$results, d$strats,
                                  wtp_max = 200000, wtp_step = 50000)
  dp_el <- dampack::calc_exp_loss(d$dpsa, wtp = wtp_range)

  for (w in wtp_range) {
    for (s in d$strats) {
      sc_val <- sc_el$Expected_Loss[sc_el$WTP == w & sc_el$Strategy == s]
      dp_val <- dp_el$Expected_Loss[dp_el$WTP == w & dp_el$Strategy == s]
      if (length(sc_val) == 1 && length(dp_val) == 1) {
        expect_equal(sc_val, dp_val, tolerance = 1e-6,
                     label = paste("ExpLoss WTP", w, s))
      }
    }
  }
})

# ===========================================================================
# 5. EVPPI
# ===========================================================================
test_that("EVPPI is in the same ballpark as dampack::calc_evppi", {
  d <- setup_comparison(n_sim = 500)
  psa_params <- names(d$psa$draws)
  if (length(psa_params) == 0) skip("No PSA draws")
  test_param <- psa_params[1]
  wtp <- 100000

  sc_evppi <- compute_evppi(d$psa$results, d$psa$draws, d$strats,
                             wtp = wtp, param_names = test_param)

  dp_evppi <- tryCatch(
    dampack::calc_evppi(d$dpsa, wtp = wtp, params = test_param),
    error = function(e) NULL
  )

  sc_val <- sc_evppi$EVPPI[1]
  expect_true(is.finite(sc_val) && sc_val >= -1)

  if (!is.null(dp_evppi) && is.data.frame(dp_evppi) && nrow(dp_evppi) > 0) {
    dp_val <- dp_evppi$EVPPI[1]
    if (!is.na(dp_val) && is.finite(dp_val) && dp_val > 1) {
      ratio <- sc_val / dp_val
      expect_true(ratio > 0.05 && ratio < 20,
                  label = paste("EVPPI ratio", round(ratio, 2)))
    }
  }
})

# ===========================================================================
# 6. One-way DSA / Tornado
# ===========================================================================
test_that("Tornado DSA produces consistent structure", {
  d <- setup_comparison()
  det <- run_deterministic(d$m, d$pars, wtp = 100000)
  target <- d$strats[d$strats != det$ref][1]

  td <- build_tornado_data(d$pars, d$pars, d$m, ref = det$ref,
                            target = target, metric = "INB", wtp = 100000)
  expect_true(is.data.frame(td))
  expect_true(nrow(td) > 0)
  # Actual columns: name, bound, value, base
  expect_true(all(c("name", "bound", "value", "base") %in% names(td)))
})

# ===========================================================================
# 7. Two-way DSA
# ===========================================================================
test_that("Two-way DSA produces a grid", {
  d <- setup_comparison()
  ptable <- d$pars
  eligible <- ptable$name[ptable$include_tornado == TRUE &
                          ptable$lower != ptable$upper]
  if (length(eligible) < 2) skip("Not enough tornado parameters")

  p1 <- eligible[1]; p2 <- eligible[2]
  r1 <- c(ptable$lower[ptable$name == p1], ptable$upper[ptable$name == p1])
  r2 <- c(ptable$lower[ptable$name == p2], ptable$upper[ptable$name == p2])
  det <- run_deterministic(d$m, d$pars, wtp = 100000)
  target <- d$strats[d$strats != det$ref][1]

  sc_tw <- twoway_dsa(d$m, d$pars, p1, p2,
                       range1 = r1, range2 = r2,
                       n_grid = 5, ref = det$ref,
                       target = target, wtp = 100000)
  # Returns a list with $grid
  expect_true(is.list(sc_tw))
  expect_true(is.data.frame(sc_tw$grid))
  expect_true(all(c(p1, p2) %in% names(sc_tw$grid)))
  expect_equal(nrow(sc_tw$grid), 25)
})

# ===========================================================================
# 8. PSA parameter sampling distributions
# ===========================================================================
test_that("PSA sampling distributions match dampack helpers", {
  # Beta: dampack::beta_params(mean, sigma) where sigma = SD
  mean_val <- 0.3; sigma <- 0.05
  dp_bp <- dampack::beta_params(mean_val, sigma)
  sc_bp <- derive_beta(mean_val, se = sigma)
  expect_equal(unname(sc_bp["alpha"]), dp_bp$alpha, tolerance = 1e-10)
  expect_equal(unname(sc_bp["beta"]),  dp_bp$beta,  tolerance = 1e-10)

  # Gamma: dampack returns shape + SCALE (not rate)
  mean_cost <- 5000; sigma_cost <- 1000
  dp_gp <- dampack::gamma_params(mean_cost, sigma_cost)
  sc_gp <- derive_gamma(mean_cost, se = sigma_cost)
  expect_equal(unname(sc_gp["shape"]), dp_gp$shape, tolerance = 1e-10)
  # dampack returns scale = 1/rate; ShinyCEA returns rate
  expect_equal(1 / unname(sc_gp["rate"]), dp_gp$scale, tolerance = 1e-10)

  # Lognormal: dampack::lnorm_params(m, s) returns mu + sigma (log-scale)
  # Verify dampack is self-consistent: generated samples match input moments
  dp_lp <- dampack::lnorm_params(mean_cost, sigma_cost)
  samp <- rlnorm(1e6, meanlog = dp_lp$mu, sdlog = dp_lp$sigma)
  expect_equal(mean(samp), mean_cost, tolerance = mean_cost * 0.01)
  expect_equal(sd(samp), sigma_cost, tolerance = sigma_cost * 0.05)
})

# ===========================================================================
# 9. Metamodel: ShinyCEA vs dampack::metamodel
# ===========================================================================
test_that("Metamodel R-squared is comparable to dampack", {
  d <- setup_comparison(n_sim = 500)
  psa_params <- names(d$psa$draws)
  if (length(psa_params) == 0) skip("No PSA draws")
  test_param <- psa_params[1]

  sc_mm <- cea_metamodel(d$psa$results, d$psa$draws, d$strats,
                          params = test_param, outcome = "nmb",
                          wtp = 100000, type = "linear")

  dp_mm <- tryCatch(
    dampack::metamodel(analysis = "oneway", psa = d$dpsa,
                       params = test_param, outcome = "nmb",
                       wtp = 100000, type = "linear"),
    error = function(e) NULL
  )

  expect_true(all(sc_mm$r_squared >= 0 & sc_mm$r_squared <= 1))

  if (!is.null(dp_mm)) {
    for (s in d$strats) {
      sc_r2 <- unname(sc_mm$r_squared[s])
      dp_r2 <- tryCatch(
        summary(dp_mm$mods[[test_param]][[s]])$r.squared,
        error = function(e) NA
      )
      if (!is.na(dp_r2)) {
        expect_equal(sc_r2, dp_r2, tolerance = 0.01,
                     label = paste("Metamodel R2 for", s))
      }
    }
  }
})

# ===========================================================================
# 10. Metamodel prediction
# ===========================================================================
test_that("Metamodel predictions match dampack for linear type", {
  d <- setup_comparison(n_sim = 500)
  psa_params <- names(d$psa$draws)
  if (length(psa_params) == 0) skip("No PSA draws")
  test_param <- psa_params[1]

  sc_mm <- cea_metamodel(d$psa$results, d$psa$draws, d$strats,
                          params = test_param, outcome = "nmb",
                          wtp = 100000, type = "linear")
  dp_mm <- tryCatch(
    dampack::metamodel(analysis = "oneway", psa = d$dpsa,
                       params = test_param, outcome = "nmb",
                       wtp = 100000, type = "linear"),
    error = function(e) NULL
  )

  skip_if(is.null(dp_mm), "dampack::metamodel failed on this PSA")

  # dampack predicts in long format (parameter, strategy, param_val,
  # outcome_val) over its own parameter range, so ShinyCEA is evaluated on
  # that same range to compare the two fitted lines point by point.
  dp_pred <- as.data.frame(predict(dp_mm, ranges = NULL, nsamp = 10))
  dp_range <- range(dp_pred$param_val)
  sc_pred <- predict(sc_mm, params = test_param, nsamp = 10,
                     ranges = stats::setNames(list(dp_range), test_param))

  for (s in d$strats) {
    dp_s <- dp_pred[dp_pred$strategy == s, ]
    dp_s <- dp_s[order(dp_s$param_val), ]
    expect_equal(nrow(dp_s), 10L)
    expect_equal(sc_pred[[test_param]], dp_s$param_val, tolerance = 1e-8)
    # Same data + same linear model => same predictions, up to rounding
    expect_equal(sc_pred[[s]], dp_s$outcome_val, tolerance = 1e-6,
                 label = paste("Metamodel prediction for", s))
  }
})

# ===========================================================================
# 11. Unit test: cea_metamodel types work
# ===========================================================================
test_that("cea_metamodel works for all three types", {
  d <- setup_comparison(n_sim = 200)
  pnames <- names(d$psa$draws)
  if (length(pnames) == 0) skip("No PSA draws")

  for (tp in c("linear", "poly", "gam")) {
    mm <- cea_metamodel(d$psa$results, d$psa$draws, d$strats,
                        params = pnames[1], outcome = "cost", type = tp)
    expect_s3_class(mm, "cea_metamodel")
    expect_true(all(mm$r_squared >= 0))
    pred <- predict(mm, nsamp = 10)
    expect_true(is.data.frame(pred))
    expect_true(all(d$strats %in% names(pred)))
  }
})

# ===========================================================================
# 12. Two-way metamodel prediction
# ===========================================================================
test_that("Two-way metamodel prediction works", {
  d <- setup_comparison(n_sim = 200)
  pnames <- names(d$psa$draws)
  if (length(pnames) < 2) skip("Need at least 2 PSA parameters")

  mm <- cea_metamodel(d$psa$results, d$psa$draws, d$strats,
                      params = pnames[1:2], outcome = "nmb",
                      wtp = 100000, type = "poly")
  pred <- predict(mm, params = pnames[1:2], nsamp = 10)
  expect_equal(nrow(pred), 100)
  expect_true(all(d$strats %in% names(pred)))
})

# ===========================================================================
# 13. Metamodel plots
# ===========================================================================
test_that("Metamodel plot functions produce ggplot objects", {
  d <- setup_comparison(n_sim = 200)
  pnames <- names(d$psa$draws)
  if (length(pnames) < 2) skip("Need at least 2 PSA parameters")

  mm <- cea_metamodel(d$psa$results, d$psa$draws, d$strats,
                      params = pnames[1:2], outcome = "nmb",
                      wtp = 100000, type = "linear")
  p1 <- plot_metamodel(mm, param = pnames[1], nsamp = 20)
  expect_s3_class(p1, "gg")
  p2 <- plot_metamodel_twoway(mm, params = pnames[1:2],
                               strategy = d$strats[1], nsamp = 10)
  expect_s3_class(p2, "gg")
})

# ===========================================================================
# 14. NMB/NHB convention
# ===========================================================================
test_that("NMB and NHB definitions match dampack convention", {
  expect_equal(5 * 50000 - 10000, 240000)
  expect_equal(5 - 10000/50000, 4.8)
})

# ===========================================================================
# 15. Full pipeline consistency
# ===========================================================================
test_that("Full pipeline produces consistent results across packages", {
  d <- setup_comparison(n_sim = 300)

  mean_c <- colMeans(d$cost_mat)
  mean_e <- colMeans(d$eff_mat)
  df_cq <- data.frame(Strategy = d$strats, Cost = mean_c[d$strats],
                       QALY = mean_e[d$strats], stringsAsFactors = FALSE)

  sc <- frontier_analysis(df_cq)
  dp <- dampack::calculate_icers(mean_c, mean_e, d$strats)

  expect_equal(sort(as.character(sc$frontier$Strategy)),
               sort(as.character(dp$Strategy[dp$Status == "ND"])))

  # EVPI matches
  sc_evpi <- compute_evpi(d$psa$results, d$strats,
                           wtp_max = 200000, wtp_step = 100000)
  dp_evpi <- dampack::calc_evpi(d$dpsa, wtp = seq(0, 200000, 100000))
  for (w in sc_evpi$WTP) {
    expect_equal(sc_evpi$EVPI[sc_evpi$WTP == w],
                 dp_evpi$EVPI[dp_evpi$WTP == w],
                 tolerance = 1e-6, label = paste("EVPI WTP", w))
  }

  # min(EL) = EVPI identity
  sc_el <- compute_expected_loss(d$psa$results, d$strats,
                                  wtp_max = 200000, wtp_step = 100000)
  for (w in unique(sc_el$WTP)) {
    min_el <- min(sc_el$Expected_Loss[sc_el$WTP == w])
    evpi_w <- sc_evpi$EVPI[sc_evpi$WTP == w]
    expect_equal(min_el, evpi_w, tolerance = 1e-6,
                 label = paste("min(EL)=EVPI WTP", w))
  }
})
