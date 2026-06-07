test_that("run_weighted_analysis returns correct structure", {
  m <- cea_sick_sicker()
  composites <- list(
    "Current SoC" = c(std = 0.6, t1 = 0.2, t2 = 0.1, t3 = 0.1),
    "New drug" = c(t4 = 1)
  )
  res <- run_weighted_analysis(m, m$default_params(), composites, wtp = 100000)

  expect_true(is.list(res))
  expect_true(all(c("composites", "pairwise", "details") %in% names(res)))
  expect_equal(nrow(res$composites), 2L)
  expect_true(all(c("Composite", "Cost", "QALY", "NMB") %in% names(res$composites)))
})

test_that("run_weighted_analysis computes weighted averages correctly", {
  m <- cea_sick_sicker()
  # Single strategy with weight 1 should equal the individual run
  composites <- list("Just std" = c(std = 1))
  res <- run_weighted_analysis(m, m$default_params(), composites, wtp = 100000)
  det <- run_deterministic(m, m$default_params(), wtp = 100000)

  std_cost <- det$table$Cost[det$table$Strategy == "std"]
  expect_equal(res$composites$Cost[1], std_cost, tolerance = 0.01)
})

test_that("pairwise comparisons are computed", {
  m <- cea_sick_sicker()
  composites <- list(
    "Mix A" = c(std = 0.5, t1 = 0.5),
    "Mix B" = c(t2 = 0.5, t3 = 0.5),
    "Pure t4" = c(t4 = 1)
  )
  res <- run_weighted_analysis(m, m$default_params(), composites, wtp = 100000)

  # 3 composites -> 6 pairwise comparisons (3*2)
  expect_equal(nrow(res$pairwise), 6L)
  expect_true(all(c("Reference", "Comparator", "IncCost", "IncQALY",
                     "ICER", "INB") %in% names(res$pairwise)))
})

test_that("details show per-strategy contributions", {
  m <- cea_sick_sicker()
  composites <- list("Mix" = c(std = 0.6, t1 = 0.4))
  res <- run_weighted_analysis(m, m$default_params(), composites, wtp = 100000)

  expect_true(all(c("Composite", "Strategy", "Weight",
                     "Cost_Weighted", "QALY_Weighted") %in% names(res$details)))
  expect_equal(nrow(res$details), 2L)
  expect_equal(sum(res$details$Weight), 1)
})

test_that("weights are auto-normalized", {
  m <- cea_sick_sicker()
  # Weights don't sum to 1 but are close
  composites <- list("Mix" = c(std = 0.6, t1 = 0.3, t2 = 0.1))
  res <- run_weighted_analysis(m, m$default_params(), composites)
  expect_equal(sum(res$details$Weight), 1, tolerance = 0.001)
})

test_that("run_weighted_analysis errors on non-named list", {
  m <- cea_sick_sicker()
  expect_error(run_weighted_analysis(m, m$default_params(), list(c(std = 1))),
               "named list")
})

test_that("run_weighted_analysis errors on unknown strategy", {
  m <- cea_sick_sicker()
  expect_error(
    run_weighted_analysis(m, m$default_params(),
                          list("Bad" = c(nonexistent = 1))),
    "unknown strategies"
  )
})

test_that("plot_weighted_comparison returns ggplot", {
  detail_df <- data.frame(
    Composite = rep(c("A", "B"), each = 2),
    Strategy = rep(c("s1", "s2"), 2),
    Weight = c(0.6, 0.4, 0.3, 0.7),
    Cost_Weighted = c(600, 400, 300, 700),
    QALY_Weighted = c(0.6, 0.4, 0.3, 0.7),
    stringsAsFactors = FALSE
  )
  expect_s3_class(plot_weighted_comparison(detail_df, "Cost"), "ggplot")
  expect_s3_class(plot_weighted_comparison(detail_df, "QALY"), "ggplot")
})

test_that("plot_weighted_pairwise returns ggplot", {
  pw_df <- data.frame(
    Reference = "A", Comparator = "B",
    IncCost = 1000, IncQALY = 0.5,
    ICER = 2000, INB = 48000,
    stringsAsFactors = FALSE
  )
  expect_s3_class(plot_weighted_pairwise(pw_df, "ICER"), "ggplot")
  expect_s3_class(plot_weighted_pairwise(pw_df, "INB"), "ggplot")
})

test_that("end-to-end weighted analysis with partitioned survival", {
  m <- cea_partitioned_survival()
  composites <- list(
    "Current care" = c(std = 0.7, t1 = 0.3),
    "New protocol" = c(t4 = 0.5, t5 = 0.5)
  )
  res <- run_weighted_analysis(m, m$default_params(), composites, wtp = 100000)

  expect_equal(nrow(res$composites), 2L)
  expect_true(all(is.finite(res$composites$Cost)))
  expect_true(all(is.finite(res$composites$QALY)))
  expect_equal(nrow(res$pairwise), 2L)
})
