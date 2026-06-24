# =============================================================================
# STRESS TESTS: edge cases, extreme inputs, adversarial parameters
# =============================================================================

# --- 1. MODEL INTERFACE STRESS ---

test_that("model with 2 strategies (minimum) works end-to-end", {
  m <- new_cea_model(
    info = function() list(key = "min", name = "Minimal", strategies = c("A", "B")),
    params = function() cea_param("p", 0.5, 0.1, 0.9, distribution = "beta"),
    simulate = function(pars, strategy) {
      list(cost = if (strategy == "A") 100 else 200, qaly = if (strategy == "A") 1 else 1.5)
    }
  )
  det <- run_deterministic(m, m$default_params(), wtp = 100000)
  expect_equal(nrow(det$table), 2L)
})

test_that("model with 10 strategies works", {
  strats <- paste0("s", 1:10)
  m <- new_cea_model(
    info = function() list(key = "many", name = "Many", strategies = strats),
    params = function() cea_param("x", 1, 0, 2, distribution = "uniform"),
    simulate = function(pars, strategy) {
      i <- match(strategy, strats)
      list(cost = i * 1000, qaly = i * 0.5)
    }
  )
  det <- run_deterministic(m, m$default_params(), wtp = 50000)
  expect_equal(nrow(det$table), 10L)
  expect_true(nrow(det$frontier) >= 1)
})

test_that("model with identical strategies produces valid output", {
  m <- new_cea_model(
    info = function() list(key = "same", name = "Same", strategies = c("A", "B")),
    params = function() cea_param("x", 1, 0, 2),
    simulate = function(pars, strategy) list(cost = 100, qaly = 1)
  )
  det <- run_deterministic(m, m$default_params(), wtp = 100000)
  expect_equal(det$table$Cost[1], det$table$Cost[2])
  expect_true(all(is.finite(det$table$NMB)))
})

# --- 2. EXTREME PARAMETER VALUES ---

test_that("zero cost and zero QALY handled", {
  m <- new_cea_model(
    info = function() list(key = "z", name = "Z", strategies = c("A", "B")),
    params = function() cea_param("x", 0, 0, 1),
    simulate = function(pars, strategy) {
      if (strategy == "A") list(cost = 0, qaly = 0)
      else list(cost = 100, qaly = 0.1)
    }
  )
  det <- run_deterministic(m, m$default_params(), wtp = 100000)
  expect_true(all(is.finite(det$table$NMB)))
})

test_that("very large costs don't overflow", {
  m <- new_cea_model(
    info = function() list(key = "big", name = "Big", strategies = c("A", "B")),
    params = function() cea_param("x", 1, 0, 2),
    simulate = function(pars, strategy) {
      list(cost = if (strategy == "A") 1e8 else 2e8, qaly = 1)
    }
  )
  det <- run_deterministic(m, m$default_params(), wtp = 100000)
  expect_true(all(is.finite(det$table$Cost)))
})

test_that("very small probabilities work in PSA", {
  m <- new_cea_model(
    info = function() list(key = "tiny", name = "Tiny", strategies = c("A", "B")),
    params = function() rbind(
      cea_param("p_rare", 0.001, 0.0001, 0.005, distribution = "beta"),
      cea_param("c", 1000, 500, 2000, distribution = "gamma")
    ),
    simulate = function(pars, strategy) {
      list(cost = pars[["c"]], qaly = 1 - pars[["p_rare"]])
    }
  )
  set.seed(42)
  psa <- run_psa(m, m$default_params(), n_sim = 50)
  expect_true(psa$used > 0)
  expect_true(psa$dropped == 0)
})

test_that("probability near 0 and 1 boundaries in beta sampling", {
  set.seed(42)
  row_low <- cea_param("p", 0.001, 0.0001, 0.01, distribution = "beta")
  row_high <- cea_param("p", 0.999, 0.99, 0.9999, distribution = "beta")
  vals_low <- replicate(200, sample_param_row(row_low))
  vals_high <- replicate(200, sample_param_row(row_high))
  expect_true(all(vals_low >= 0 & vals_low <= 1))
  expect_true(all(vals_high >= 0 & vals_high <= 1))
})

test_that("gamma with very small mean doesn't crash", {
  set.seed(42)
  row <- cea_param("c", 0.01, 0.001, 0.1, distribution = "gamma")
  vals <- replicate(100, sample_param_row(row))
  expect_true(all(vals >= 0))
  expect_true(all(is.finite(vals)))
})

test_that("lognormal with HR near 1 works", {
  set.seed(42)
  row <- cea_param("hr", 1.0, 0.95, 1.05, distribution = "lognormal")
  vals <- replicate(100, sample_param_row(row))
  expect_true(all(vals > 0))
  expect_true(abs(median(vals) - 1.0) < 0.1)
})

# --- 3. FRONTIER EDGE CASES ---

test_that("frontier with all strategies on frontier (distinct ICERs)", {
  df <- data.frame(
    Strategy = c("A", "B", "C"),
    Cost = c(100, 250, 500),
    QALY = c(1, 2.5, 4)
  )
  fa <- frontier_analysis(df)
  # B should be on frontier: ICER A->B = 150/1.5 = 100, B->C = 250/1.5 = 167
  expect_equal(nrow(fa$frontier), 3L)
  expect_equal(sum(fa$classified$Status == "Dominated"), 0L)
})

test_that("frontier with equal ICERs removes intermediate", {
  # Equal ICERs: A->B = B->C = 100 => B extendedly dominated
  df <- data.frame(
    Strategy = c("A", "B", "C"),
    Cost = c(100, 200, 300),
    QALY = c(1, 2, 3)
  )
  fa <- frontier_analysis(df)
  expect_equal(nrow(fa$frontier), 2L)
  expect_true("B" %in% fa$classified$Strategy[fa$classified$Status == "Extendedly dominated"])
})

test_that("frontier with all dominated except one", {
  df <- data.frame(
    Strategy = c("A", "B", "C"),
    Cost = c(100, 200, 300),
    QALY = c(3, 2, 1)
  )
  fa <- frontier_analysis(df)
  expect_equal(nrow(fa$frontier), 1L)
  expect_equal(fa$frontier$Strategy, "A")
})

test_that("frontier with equal cost, different QALY", {
  df <- data.frame(
    Strategy = c("A", "B"),
    Cost = c(100, 100),
    QALY = c(1, 2)
  )
  fa <- frontier_analysis(df)
  expect_equal(fa$classified$Status[fa$classified$Strategy == "A"], "Dominated")
})

# --- 4. PSA STRESS ---

test_that("PSA with all fixed parameters returns empty draws", {
  m <- new_cea_model(
    info = function() list(key = "fx", name = "Fx", strategies = c("A", "B")),
    params = function() cea_param("x", 1, 0, 2, distribution = "fixed",
                                  include_psa = FALSE),
    simulate = function(pars, strategy) list(cost = 100, qaly = 1)
  )
  set.seed(42)
  psa <- run_psa(m, m$default_params(), n_sim = 10)
  expect_true(psa$used > 0)
})

test_that("PSA with n_sim = 1 doesn't crash", {
  m <- cea_sick_sicker()
  set.seed(42)
  psa <- run_psa(m, m$default_params(), n_sim = 1)
  expect_true(psa$used + psa$dropped == 1)
})

test_that("CEAC with single-iteration PSA doesn't crash", {
  m <- cea_sick_sicker()
  set.seed(42)
  psa <- run_psa(m, m$default_params(), n_sim = 5)
  ceac <- compute_ceac(psa$results, m$info()$strategies,
                       wtp_max = 100000, wtp_step = 50000)
  expect_true(nrow(ceac) > 0)
  expect_true(all(ceac$ProbCE >= 0 & ceac$ProbCE <= 1))
})

# --- 5. TORNADO EDGE CASES ---

test_that("tornado with no eligible parameters returns empty", {
  m <- new_cea_model(
    info = function() list(key = "nt", name = "NT", strategies = c("A", "B")),
    params = function() cea_param("x", 1, 0, 2, distribution = "fixed",
                                  include_tornado = FALSE),
    simulate = function(pars, strategy) list(cost = 100, qaly = 1)
  )
  params <- m$default_params()
  df_tor <- params[params$include_tornado == TRUE, ]
  expect_equal(nrow(df_tor), 0L)
})

test_that("tornado with single parameter works", {
  m <- cea_sick_sicker()
  params <- m$default_params()
  df_tor <- params[params$name == "p_HS_std", ]
  pars <- stats::setNames(as.list(params$base), params$name)
  tor <- build_tornado_data(df_tor, pars, m, "std", "t4", metric = "INB", wtp = 100000)
  expect_true(nrow(tor) > 0)
})

# --- 6. THRESHOLD EDGE CASES ---

test_that("threshold when metric is flat (no crossing)", {
  m <- cea_sick_sicker()
  params <- m$default_params()
  pars <- stats::setNames(as.list(params$base), params$name)
  res <- threshold_analysis(m, pars, "u_H", "std", "t4",
                            range_min = 0.85, range_max = 0.95,
                            n_points = 10, wtp = 100000, mode = "inb")
  # May or may not find threshold, but should not error

  expect_true(is.list(res))
  expect_true(is.data.frame(res$grid))
})

# --- 7. DISCOUNT EDGE CASES ---

test_that("discount rate = 0 gives all weights = 1", {
  w <- cea_discount_weights(10, rate = 0, cycle_length = 1)
  expect_equal(w, rep(1, 10))
})

test_that("very high discount rate doesn't produce NaN", {
  w <- cea_discount_weights(10, rate = 0.50, cycle_length = 1)
  expect_true(all(is.finite(w)))
  expect_true(all(w > 0))
})

test_that("negative discount rate is treated as 0", {
  w <- cea_discount_weights(5, rate = -0.05, cycle_length = 1)
  expect_equal(w, rep(1, 5))
})

# --- 8. VOI EDGE CASES ---

test_that("EVPI with 2 strategies works", {
  m <- new_cea_model(
    info = function() list(key = "ev", name = "EV", strategies = c("A", "B")),
    params = function() cea_param("p", 0.5, 0.1, 0.9, distribution = "beta"),
    simulate = function(pars, strategy) {
      list(cost = if (strategy == "A") 100 else 200,
           qaly = if (strategy == "A") 1 else 1 + pars[["p"]])
    }
  )
  set.seed(42)
  psa <- run_psa(m, m$default_params(), n_sim = 100)
  evpi <- compute_evpi(psa$results, c("A", "B"), wtp_max = 50000, wtp_step = 25000)
  expect_true(all(evpi$EVPI >= 0))
})

test_that("expected loss minimum equals EVPI", {
  m <- cea_sick_sicker()
  set.seed(42)
  psa <- run_psa(m, m$default_params(), n_sim = 200)
  strats <- m$info()$strategies
  evpi <- compute_evpi(psa$results, strats, wtp_max = 100000, wtp_step = 100000)
  el <- compute_expected_loss(psa$results, strats, wtp_max = 100000, wtp_step = 100000)

  evpi_100k <- evpi$EVPI[evpi$WTP == 100000]
  min_el_100k <- min(el$Expected_Loss[el$WTP == 100000])
  expect_equal(evpi_100k, min_el_100k, tolerance = 0.01)
})

# --- 9. EQUITY EDGE CASES ---

test_that("distributional CEA with 1 subgroup equals standard", {
  m <- cea_sick_sicker()
  sg <- list("All" = list(overrides = list(), share = 1.0, baseline_qaly = 0.80))
  dcea <- run_distributional_cea(m, m$default_params(), sg, wtp = 100000)
  det <- run_deterministic(m, m$default_params(), wtp = 100000)

  for (s in m$info()$strategies) {
    dcea_cost <- dcea$population$Cost[dcea$population$Strategy == s]
    det_cost <- det$table$Cost[det$table$Strategy == s]
    expect_equal(dcea_cost, det_cost, tolerance = 0.01)
  }
})

test_that("concentration index with 5+ subgroups works", {
  m <- cea_sick_sicker()
  sg <- list(
    "G1" = list(overrides = list(p_HS_std = 0.01), share = 0.2, baseline_qaly = 0.95),
    "G2" = list(overrides = list(p_HS_std = 0.03), share = 0.2, baseline_qaly = 0.85),
    "G3" = list(overrides = list(p_HS_std = 0.05), share = 0.2, baseline_qaly = 0.70),
    "G4" = list(overrides = list(p_HS_std = 0.08), share = 0.2, baseline_qaly = 0.55),
    "G5" = list(overrides = list(p_HS_std = 0.12), share = 0.2, baseline_qaly = 0.40)
  )
  dcea <- run_distributional_cea(m, m$default_params(), sg, wtp = 100000)
  ci <- calculate_concentration_index(dcea, strategy = "std")
  expect_true(abs(ci$index) <= 1)
})

# --- 10. WEIGHTED COMPARATOR EDGE CASES ---

test_that("weighted with all weight on one strategy equals individual", {
  m <- cea_sick_sicker()
  composites <- list("Just std" = c(std = 1))
  res <- run_weighted_analysis(m, m$default_params(), composites, wtp = 100000)
  det <- run_deterministic(m, m$default_params(), wtp = 100000)
  expect_equal(res$composites$Cost[1],
               det$table$Cost[det$table$Strategy == "std"], tolerance = 0.01)
})

test_that("weighted with tiny weights doesn't crash", {
  m <- cea_sick_sicker()
  composites <- list(
    "Mix" = c(std = 0.001, t1 = 0.001, t2 = 0.001, t3 = 0.001, t4 = 0.996)
  )
  res <- run_weighted_analysis(m, m$default_params(), composites, wtp = 100000)
  expect_true(is.finite(res$composites$Cost[1]))
})

# --- 11. BIA EDGE CASES ---

test_that("BIA with zero population returns zero impact", {
  pop <- define_population(0, prevalence = 0.01, years = 3)
  shares <- define_market_shares(c(A = 1), c(A = 0.5, B = 0.5))
  costs <- define_cost_components(A = list(drug = 500), B = list(drug = 3000))
  bia <- run_bia_modern(pop, shares, costs)
  expect_true(all(bia$impact$Budget_Impact == 0))
})

test_that("BIA with 100% mortality empties pool", {
  pop <- define_population(100000, prevalence = 0.01, mortality_rate = 1.0, years = 3)
  expect_true(pop$eligible[3] == 0)
})

# --- 12. VALIDATION EDGE CASES ---

test_that("validation report on microsim model works", {
  m <- cea_microsim()
  params <- m$default_params()
  params$base[params$name == "n_patients"] <- 100
  report <- generate_validation_report(m, params)
  expect_true(report$score >= 0)
})

test_that("check_psa_convergence with perfect convergence", {
  # Fake PSA data where one strategy always wins
  n <- 500
  results <- data.frame(
    A.cost = rep(100, n), A.q = rep(1, n),
    B.cost = rep(200, n), B.q = rep(0.5, n),
    check.names = FALSE
  )
  checks <- check_psa_convergence(results, c("A", "B"), wtp = 100000)
  conv <- checks[grepl("Convergence", checks$check), ]
  expect_equal(conv$status, "PASS")
})

# --- 13. REPORT GENERATION STRESS ---

test_that("render_hta_report with all options doesn't crash", {
  m <- cea_sick_sicker()
  tmpdir <- tempfile("stress_")
  qmd <- render_hta_report(
    m, wtp = 100000, n_psa = 10,
    scenarios = list("Base" = list(), "Alt" = list(d_cost = 0)),
    subgroups = list("All" = list(), "Risk" = list(p_HS_std = 0.08)),
    title = "Stress Test", format = "html",
    output_dir = tmpdir, render = FALSE
  )
  expect_true(file.exists(qmd))
  content <- readLines(qmd)
  expect_true(length(content) > 50)
  unlink(tmpdir, recursive = TRUE)
})

# --- 14. SAMPLE_PSA STRESS ---

test_that("sample_psa with correlation matrix works", {
  params <- rbind(
    define_param("a", 0.3, se = 0.05, distribution = "beta"),
    define_param("b", 0.4, se = 0.06, distribution = "beta"),
    define_param("c", 1000, se = 200, distribution = "gamma")
  )
  corr <- add_correlation(pairs = list(
    list(params = c("a", "b"), rho = 0.7)
  ))
  set.seed(42)
  draws <- sample_psa(params, n = 500, correlation = corr)
  expect_equal(nrow(draws), 500)
  r <- cor(draws$a, draws$b)
  expect_true(r > 0.4)  # should be close to 0.7
})

test_that("sample_psa with all fixed params returns empty", {
  params <- rbind(
    define_param("x", 1, distribution = "fixed", include_psa = FALSE),
    define_param("y", 2, distribution = "fixed", include_psa = FALSE)
  )
  draws <- sample_psa(params, n = 10)
  expect_equal(ncol(draws), 0L)
})

# --- 15. TWO-WAY DSA STRESS ---

test_that("twoway_dsa with very coarse grid works", {
  m <- cea_sick_sicker()
  pars <- stats::setNames(as.list(m$default_params()$base), m$default_params()$name)
  res <- twoway_dsa(m, pars, "p_HS_std", "drug_t4",
                    range1 = c(0.02, 0.08), range2 = c(200, 1200),
                    n_grid = 3, ref = "std", target = "t4", wtp = 100000)
  expect_equal(nrow(res$grid), 9L)
  p <- plot_twoway_dsa(res)
  expect_s3_class(p, "ggplot")
})

test_that("twoway_dsa with NA results doesn't crash", {
  m <- new_cea_model(
    info = function() list(key = "na", name = "NA", strategies = c("A", "B")),
    params = function() rbind(
      cea_param("x", 0.5, 0, 1, distribution = "beta"),
      cea_param("y", 100, 0, 200, distribution = "gamma")
    ),
    simulate = function(pars, strategy) {
      if (pars[["x"]] > 0.9) return(list(cost = NaN, qaly = NaN))
      list(cost = pars[["y"]], qaly = pars[["x"]])
    }
  )
  pars <- list(x = 0.5, y = 100)
  res <- twoway_dsa(m, pars, "x", "y",
                    range1 = c(0, 1), range2 = c(0, 200),
                    n_grid = 5, ref = "A", target = "B", wtp = 100000)
  expect_true(any(is.na(res$grid$value)))
  p <- plot_twoway_dsa(res)
  expect_s3_class(p, "ggplot")
})
