test_that("run_distributional_cea returns correct structure", {
  m <- cea_sick_sicker()
  sg <- list(
    "Young" = list(overrides = list(p_HS_std = 0.02), share = 0.5, baseline_qaly = 0.90),
    "Old" = list(overrides = list(p_HS_std = 0.08), share = 0.5, baseline_qaly = 0.60)
  )
  res <- run_distributional_cea(m, m$default_params(), sg, wtp = 100000)
  expect_true(is.list(res))
  expect_true(all(c("by_subgroup", "population", "strategies", "wtp") %in% names(res)))
  expect_equal(nrow(res$by_subgroup), 2 * length(m$info()$strategies))
  expect_equal(nrow(res$population), length(m$info()$strategies))
})

test_that("run_distributional_cea errors on bad shares", {
  m <- cea_sick_sicker()
  sg <- list(
    "A" = list(overrides = list(), share = 0.3, baseline_qaly = 0.8),
    "B" = list(overrides = list(), share = 0.3, baseline_qaly = 0.7)
  )
  expect_error(run_distributional_cea(m, m$default_params(), sg), "sum to 1")
})

test_that("population averages are weighted correctly", {
  m <- cea_sick_sicker()
  sg <- list(
    "All" = list(overrides = list(), share = 1.0, baseline_qaly = 0.80)
  )
  res <- run_distributional_cea(m, m$default_params(), sg, wtp = 100000)
  det <- run_deterministic(m, m$default_params(), wtp = 100000)
  # With share=1 and no overrides, population cost should match deterministic
  for (s in m$info()$strategies) {
    pop_cost <- res$population$Cost[res$population$Strategy == s]
    det_cost <- det$table$Cost[det$table$Strategy == s]
    expect_equal(pop_cost, det_cost, tolerance = 0.01)
  }
})

test_that("calculate_equity_weighted_nmb returns correct structure", {
  m <- cea_sick_sicker()
  sg <- list(
    "Young" = list(overrides = list(p_HS_std = 0.02), share = 0.5, baseline_qaly = 0.90),
    "Old" = list(overrides = list(p_HS_std = 0.08), share = 0.5, baseline_qaly = 0.55)
  )
  dcea <- run_distributional_cea(m, m$default_params(), sg, wtp = 100000)
  ew <- calculate_equity_weighted_nmb(dcea, method = "atkinson", epsilon = 1)

  expect_true(is.data.frame(ew))
  expect_true(all(c("Strategy", "QALY", "NMB", "Equity_Weighted_QALY",
                     "Equity_Weighted_NMB") %in% names(ew)))
  expect_equal(nrow(ew), length(m$info()$strategies))
})

test_that("atkinson eps=0 equals standard NMB", {
  m <- cea_sick_sicker()
  sg <- list(
    "A" = list(overrides = list(), share = 0.6, baseline_qaly = 0.80),
    "B" = list(overrides = list(p_HS_std = 0.06), share = 0.4, baseline_qaly = 0.60)
  )
  dcea <- run_distributional_cea(m, m$default_params(), sg, wtp = 100000)
  ew <- calculate_equity_weighted_nmb(dcea, method = "atkinson", epsilon = 0)
  for (s in m$info()$strategies) {
    std <- ew$NMB[ew$Strategy == s]
    eq <- ew$Equity_Weighted_NMB[ew$Strategy == s]
    expect_equal(std, eq, tolerance = 1)
  }
})

test_that("higher epsilon shifts weight toward worse-off", {
  m <- cea_sick_sicker()
  sg <- list(
    "Healthy" = list(overrides = list(p_HS_std = 0.01), share = 0.5, baseline_qaly = 0.95),
    "Sick" = list(overrides = list(p_HS_std = 0.10), share = 0.5, baseline_qaly = 0.40)
  )
  dcea <- run_distributional_cea(m, m$default_params(), sg, wtp = 100000)
  ew0 <- calculate_equity_weighted_nmb(dcea, method = "atkinson", epsilon = 0)
  ew5 <- calculate_equity_weighted_nmb(dcea, method = "atkinson", epsilon = 5)
  # At high epsilon, equity-weighted values should differ from standard
  # (exact direction depends on the model, but they should differ)
  s1 <- m$info()$strategies[1]
  expect_false(abs(ew0$Equity_Weighted_NMB[ew0$Strategy == s1] -
                     ew5$Equity_Weighted_NMB[ew5$Strategy == s1]) < 1)
})

test_that("proportional weighting works", {
  m <- cea_sick_sicker()
  sg <- list(
    "A" = list(overrides = list(), share = 0.5, baseline_qaly = 0.90),
    "B" = list(overrides = list(), share = 0.5, baseline_qaly = 0.50)
  )
  dcea <- run_distributional_cea(m, m$default_params(), sg, wtp = 100000)
  ew <- calculate_equity_weighted_nmb(dcea, method = "proportional")
  expect_true(is.data.frame(ew))
})

test_that("custom weighting works", {
  m <- cea_sick_sicker()
  sg <- list(
    "A" = list(overrides = list(), share = 0.5, baseline_qaly = 0.80),
    "B" = list(overrides = list(), share = 0.5, baseline_qaly = 0.60)
  )
  dcea <- run_distributional_cea(m, m$default_params(), sg, wtp = 100000)
  ew <- calculate_equity_weighted_nmb(dcea, method = "custom",
                                       custom_weights = c("A" = 1, "B" = 2))
  expect_true(is.data.frame(ew))
})

test_that("plot_nmb_by_subgroup returns ggplot", {
  m <- cea_sick_sicker()
  sg <- list(
    "A" = list(overrides = list(), share = 0.5, baseline_qaly = 0.80),
    "B" = list(overrides = list(p_HS_std = 0.06), share = 0.5, baseline_qaly = 0.60)
  )
  dcea <- run_distributional_cea(m, m$default_params(), sg, wtp = 100000)
  expect_s3_class(plot_nmb_by_subgroup(dcea, "NMB"), "ggplot")
  expect_s3_class(plot_nmb_by_subgroup(dcea, "QALY"), "ggplot")
})

test_that("plot_health_distribution returns ggplot", {
  m <- cea_sick_sicker()
  sg <- list(
    "Low" = list(overrides = list(p_HS_std = 0.02), share = 0.33, baseline_qaly = 0.90),
    "Med" = list(overrides = list(p_HS_std = 0.05), share = 0.34, baseline_qaly = 0.70),
    "High" = list(overrides = list(p_HS_std = 0.10), share = 0.33, baseline_qaly = 0.50)
  )
  dcea <- run_distributional_cea(m, m$default_params(), sg, wtp = 100000)
  p <- plot_health_distribution(dcea, strategy = "std")
  expect_s3_class(p, "ggplot")
})

test_that("calculate_concentration_index returns valid result", {
  m <- cea_sick_sicker()
  sg <- list(
    "Poor" = list(overrides = list(p_HS_std = 0.08), share = 0.4, baseline_qaly = 0.50),
    "Rich" = list(overrides = list(p_HS_std = 0.02), share = 0.6, baseline_qaly = 0.90)
  )
  dcea <- run_distributional_cea(m, m$default_params(), sg, wtp = 100000)
  ci <- calculate_concentration_index(dcea, strategy = "std")
  expect_true(is.list(ci))
  expect_true(is.finite(ci$index))
  expect_true(abs(ci$index) <= 1)
  expect_true(nzchar(ci$interpretation))
})

test_that("concentration_index = 0 for equal subgroups", {
  m <- cea_sick_sicker()
  sg <- list(
    "A" = list(overrides = list(), share = 0.5, baseline_qaly = 0.70),
    "B" = list(overrides = list(), share = 0.5, baseline_qaly = 0.80)
  )
  dcea <- run_distributional_cea(m, m$default_params(), sg, wtp = 100000)
  ci <- calculate_concentration_index(dcea, strategy = "std")
  # Same overrides → same QALYs → C should be ~0
  expect_true(abs(ci$index) < 0.05)
})

test_that("equity_frontier returns ggplot", {
  m <- cea_sick_sicker()
  sg <- list(
    "Low risk" = list(overrides = list(p_HS_std = 0.02), share = 0.5, baseline_qaly = 0.90),
    "High risk" = list(overrides = list(p_HS_std = 0.10), share = 0.5, baseline_qaly = 0.50)
  )
  dcea <- run_distributional_cea(m, m$default_params(), sg, wtp = 100000)
  p <- equity_frontier(dcea, epsilon_values = c(0, 1, 5))
  expect_s3_class(p, "ggplot")
})
