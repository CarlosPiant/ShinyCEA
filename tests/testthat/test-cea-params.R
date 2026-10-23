test_that("sanitize_params returns empty df for NULL input", {
  res <- sanitize_params(NULL)
  expect_equal(nrow(res$df), 0L)
  expect_true(length(res$notes) > 0)
})

test_that("sanitize_params adds missing columns", {
  df <- data.frame(name = "x", mean = 1, stringsAsFactors = FALSE)
  res <- sanitize_params(df)
  expect_true(all(c("name", "distribution", "mean", "lower", "upper",
                     "base", "include_psa", "include_tornado") %in% names(res$df)))
})

test_that("sanitize_params swaps reversed bounds", {
  df <- data.frame(name = "x", mean = 5, lower = 10, upper = 1,
                   base = 5, distribution = "normal",
                   include_psa = TRUE, include_tornado = TRUE,
                   stringsAsFactors = FALSE)
  res <- sanitize_params(df)
  expect_true(res$df$lower[1] < res$df$upper[1])
  expect_true(any(grepl("Swapped", res$notes)))
})

test_that("sanitize_params fills missing base from mean", {
  df <- data.frame(name = "x", mean = 7, lower = 1, upper = 10,
                   base = NA, distribution = "normal",
                   include_psa = TRUE, include_tornado = TRUE,
                   stringsAsFactors = FALSE)
  res <- sanitize_params(df)
  expect_equal(res$df$base[1], 7)
})

test_that("sanitize_params clamps base to bounds", {
  df <- data.frame(name = "x", mean = 0.5, lower = 0.2, upper = 0.8,
                   base = 0.05, distribution = "beta",
                   include_psa = TRUE, include_tornado = TRUE,
                   stringsAsFactors = FALSE)
  res <- sanitize_params(df)
  expect_equal(res$df$base[1], 0.2)
  expect_true(any(grepl("Clamped", res$notes)))
})

test_that("sanitize_params drops empty-name rows", {
  df <- data.frame(name = c("x", "", "y"), mean = c(1, 2, 3),
                   lower = 0, upper = 5, base = c(1, 2, 3),
                   distribution = "fixed",
                   include_psa = TRUE, include_tornado = TRUE,
                   stringsAsFactors = FALSE)
  res <- sanitize_params(df)
  expect_equal(nrow(res$df), 2L)
  expect_true(any(grepl("Dropped", res$notes)))
})

test_that("sanitize_params de-duplicates names", {
  df <- data.frame(name = c("x", "x"), mean = c(1, 2),
                   lower = 0, upper = 5, base = c(1, 2),
                   distribution = "fixed",
                   include_psa = TRUE, include_tornado = TRUE,
                   stringsAsFactors = FALSE)
  res <- sanitize_params(df)
  expect_false(any(duplicated(res$df$name)))
})

test_that("sanitize_params normalizes distribution names", {
  df <- data.frame(name = "x", mean = 1, lower = 0, upper = 2,
                   base = 1, distribution = "None",
                   include_psa = TRUE, include_tornado = TRUE,
                   stringsAsFactors = FALSE)
  res <- sanitize_params(df)
  expect_equal(res$df$distribution[1], "fixed")
})

test_that("sample_param_row returns base for fixed distribution", {
  row <- cea_param("x", 5, 1, 10, base = 5, distribution = "fixed")
  val <- sample_param_row(row)
  expect_equal(val, 5)
})

test_that("sample_param_row draws from beta distribution in [0,1]", {
  set.seed(42)
  row <- cea_param("p_test", 0.5, 0.2, 0.8, distribution = "beta")
  vals <- replicate(100, sample_param_row(row))
  expect_true(all(vals >= 0 & vals <= 1))
  expect_true(mean(vals) > 0.2 && mean(vals) < 0.8)
})

test_that("sample_param_row draws from gamma distribution > 0", {
  set.seed(42)
  row <- cea_param("c_drug", 500, 200, 800, distribution = "gamma")
  vals <- replicate(100, sample_param_row(row))
  expect_true(all(vals >= 0))
})

test_that("sample_param_row draws from lognormal > 0", {
  set.seed(42)
  row <- cea_param("rr", 0.8, 0.5, 1.2, distribution = "lognormal")
  vals <- replicate(100, sample_param_row(row))
  expect_true(all(vals >= 0))
})

test_that("sample_param_row draws from normal distribution", {
  set.seed(42)
  row <- cea_param("x", 0, -2, 2, distribution = "normal")
  vals <- replicate(200, sample_param_row(row))
  expect_true(abs(mean(vals)) < 0.5)
})

test_that("sample_param_row draws from uniform within bounds", {
  set.seed(42)
  row <- cea_param("x", 5, 2, 8, distribution = "uniform")
  vals <- replicate(100, sample_param_row(row))
  expect_true(all(vals >= 2 & vals <= 8))
})

test_that("sample_param_row clamps probabilities to [0,1]", {
  set.seed(42)
  row <- cea_param("p_event", 0.95, 0.85, 0.99, distribution = "beta")
  vals <- replicate(100, sample_param_row(row))
  expect_true(all(vals >= 0 & vals <= 1))
})

test_that("sample_param_row does not clamp by parameter name (review A3)", {
  set.seed(42)
  # A disutility named like a utility and a negative log hazard ratio named
  # with "drug" keep their sign and centre.
  dis <- cea_param("u_decrement", -0.10, -0.15, -0.05, distribution = "normal")
  vals <- replicate(500, sample_param_row(dis))
  expect_true(all(vals < 0))
  expect_equal(mean(vals), -0.10, tolerance = 0.02)
  lhr <- cea_param("log_hr_drug", -0.3, -0.5, -0.1, distribution = "normal")
  vals <- replicate(500, sample_param_row(lhr))
  expect_equal(mean(vals), -0.3, tolerance = 0.02)
  # A ratio above 1 named p_ is not forced into [0, 1].
  rr <- cea_param("p_rate_ratio", 1.4, 1.1, 1.8, distribution = "lognormal")
  vals <- replicate(500, sample_param_row(rr))
  expect_true(mean(vals > 1) > 0.9)
})

test_that("equal bounds mean no uncertainty (review A4)", {
  row <- cea_param("x", 0.3, 0.3, 0.3, distribution = "beta")
  expect_true(all(replicate(20, sample_param_row(row)) == 0.3))
  row <- cea_param("y", 100, 100, 100, distribution = "normal", base = 120)
  expect_true(all(replicate(20, sample_param_row(row)) == 120))
})

test_that("lognormal draws reproduce the stated point estimate and interval (review A5)", {
  set.seed(1)
  row <- cea_param("hr", 0.5, 0.2, 1.25, distribution = "lognormal")
  vals <- replicate(20000, sample_param_row(row))
  expect_equal(unname(stats::median(vals)), 0.5, tolerance = 0.03)
  expect_equal(unname(stats::quantile(vals, c(0.025, 0.975))), c(0.2, 1.25), tolerance = 0.06)
})

test_that("text numbers and flags are parsed, unreadable ones reported (review D3)", {
  df <- data.frame(name = c("p_a", "p_b", "c_x", "bad"),
                   distribution = c("beta", "beta", "gamma", "beta"),
                   mean = c("0,05", "5%", "1,000.5", "abc"),
                   lower = c("0,01", "1%", "900", "0"), upper = c("0,1", "9%", "1,100", "1"),
                   base = c("0,05", "5%", "1,000.5", "0.5"),
                   include_psa = c("0", "no", "1", "maybe"), include_tornado = c("yes", "n", "TRUE", "F"),
                   stringsAsFactors = FALSE)
  res <- sanitize_params(df)
  expect_equal(res$df$mean[1:3], c(0.05, 0.05, 1000.5))
  expect_true(is.na(res$df$mean[4]))
  expect_equal(res$df$include_psa, c(FALSE, FALSE, TRUE, FALSE))
  expect_equal(res$df$include_tornado, c(TRUE, FALSE, TRUE, FALSE))
  expect_true(any(grepl("could not read 'abc'", res$notes)))
  expect_true(any(grepl("maybe", res$notes)))
})

test_that("sample_param_row handles triangular distribution", {
  set.seed(42)
  row <- cea_param("x", 5, 2, 8, distribution = "triangular")
  vals <- replicate(100, sample_param_row(row))
  expect_true(all(vals >= 2 & vals <= 8))
})

test_that("sample_param_row handles PERT distribution", {
  set.seed(42)
  row <- cea_param("x", 5, 2, 8, distribution = "pert")
  vals <- replicate(100, sample_param_row(row))
  expect_true(all(is.finite(vals)))
})

test_that("sample_param_row falls back to base for unknown dist", {
  row <- cea_param("x", 5, 2, 8, base = 5, distribution = "weirdone")
  val <- sample_param_row(row)
  expect_equal(val, 5)
})
