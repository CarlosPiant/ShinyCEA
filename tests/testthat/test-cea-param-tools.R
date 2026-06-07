# --- define_param ---

test_that("define_param creates row with all columns", {
  row <- define_param("p_death", 0.05, se = 0.01, distribution = "beta",
                      group = "Probs", source = "Smith 2023")
  expect_equal(nrow(row), 1L)
  expect_true(all(c("name", "mean", "lower", "upper", "se", "base",
                     "distribution", "shape1", "shape2",
                     "group", "source") %in% names(row)))
  expect_equal(row$name, "p_death")
  expect_equal(row$group, "Probs")
})

test_that("define_param derives bounds from SE", {
  row <- define_param("x", 0.5, se = 0.1, distribution = "normal")
  expect_equal(row$lower, 0.5 - 1.96 * 0.1, tolerance = 0.001)
  expect_equal(row$upper, 0.5 + 1.96 * 0.1, tolerance = 0.001)
})

test_that("define_param derives SE from bounds", {
  row <- define_param("x", 100, lower = 50, upper = 150, distribution = "gamma")
  expected_se <- (150 - 50) / (2 * 1.96)
  expect_equal(row$se, expected_se, tolerance = 0.01)
})

test_that("define_param computes beta shapes", {
  row <- define_param("p", 0.3, lower = 0.2, upper = 0.4, distribution = "beta")
  expect_true(is.finite(row$shape1) && row$shape1 > 0)
  expect_true(is.finite(row$shape2) && row$shape2 > 0)
})

test_that("define_param computes gamma shapes", {
  row <- define_param("c", 5000, se = 1000, distribution = "gamma")
  expect_true(is.finite(row$shape1) && row$shape1 > 0)
  expect_true(is.finite(row$shape2) && row$shape2 > 0)
})

test_that("define_param computes lognormal shapes", {
  row <- define_param("hr", 0.72, lower = 0.58, upper = 0.88,
                      distribution = "lognormal")
  expect_true(is.finite(row$shape1))  # meanlog
  expect_true(is.finite(row$shape2) && row$shape2 > 0)  # sdlog
})

# --- derive_beta ---

test_that("derive_beta returns valid alpha/beta", {
  ab <- derive_beta(0.3, lower = 0.2, upper = 0.4)
  expect_true(ab[["alpha"]] > 0 && ab[["beta"]] > 0)
  # Mean of Beta(a,b) = a/(a+b) should be close to 0.3
  expect_equal(unname(ab[["alpha"]] / (ab[["alpha"]] + ab[["beta"]])), 0.3, tolerance = 0.01)
})

test_that("derive_beta from SE", {
  ab <- derive_beta(0.5, se = 0.05)
  expect_true(ab["alpha"] > 0 && ab["beta"] > 0)
})

test_that("derive_beta clamps extreme means", {
  ab <- derive_beta(0.001)
  expect_true(all(is.finite(ab)))
})

# --- derive_gamma ---

test_that("derive_gamma returns valid shape/rate", {
  gp <- derive_gamma(5000, se = 1000)
  expect_true(gp[["shape"]] > 0 && gp[["rate"]] > 0)
  # Mean of Gamma(shape, rate) = shape / rate
  expect_equal(unname(gp[["shape"]] / gp[["rate"]]), 5000, tolerance = 1)
})

test_that("derive_gamma from CI", {
  gp <- derive_gamma(1000, lower = 600, upper = 1400)
  expect_true(all(is.finite(gp)))
})

# --- derive_lognormal ---

test_that("derive_lognormal from HR and CI", {
  lp <- derive_lognormal(0.72, lower = 0.58, upper = 0.88)
  expect_equal(unname(lp[["meanlog"]]), log(0.72), tolerance = 0.001)
  expect_true(lp[["sdlog"]] > 0)
})

test_that("derive_lognormal from SE on log scale", {
  lp <- derive_lognormal(0.60, se = 0.15)
  expect_equal(unname(lp[["sdlog"]]), 0.15)
})

# --- validate_param_table ---

test_that("validate_param_table catches beta domain violation", {
  params <- rbind(
    define_param("p_bad", 1.5, se = 0.1, distribution = "beta"),
    define_param("p_good", 0.3, se = 0.05, distribution = "beta")
  )
  diag <- validate_param_table(params)
  fails <- diag[diag$status == "FAIL", ]
  expect_true(any(grepl("p_bad", fails$parameter)))
})

test_that("validate_param_table catches gamma negative mean", {
  params <- define_param("c_bad", -500, se = 100, distribution = "gamma")
  diag <- validate_param_table(params)
  expect_true(any(diag$status == "FAIL"))
})

test_that("validate_param_table passes clean params", {
  params <- rbind(
    define_param("p", 0.3, se = 0.05, distribution = "beta"),
    define_param("c", 5000, se = 1000, distribution = "gamma"),
    define_param("hr", 0.72, lower = 0.58, upper = 0.88, distribution = "lognormal")
  )
  diag <- validate_param_table(params)
  expect_true(all(diag$status %in% c("PASS", "WARN")))
  expect_equal(sum(diag$status == "FAIL"), 0L)
})

test_that("validate_param_table warns on PSA without bounds", {
  params <- define_param("x", 100, distribution = "gamma",
                         include_psa = TRUE)
  # Remove bounds and SE to trigger PSA readiness warning
  params$lower <- NA_real_; params$upper <- NA_real_; params$se <- NA_real_
  diag <- validate_param_table(params)
  psa_rows <- diag[grepl("PSA", diag$check), ]
  expect_true(nrow(psa_rows) > 0 && any(psa_rows$status == "WARN"))
})

# --- sample_psa ---

test_that("sample_psa returns correct dimensions", {
  params <- rbind(
    define_param("p", 0.3, se = 0.05, distribution = "beta"),
    define_param("c", 5000, se = 1000, distribution = "gamma"),
    define_param("fixed_param", 10, distribution = "fixed", include_psa = FALSE)
  )
  set.seed(42)
  draws <- sample_psa(params, n = 100)
  expect_equal(nrow(draws), 100)
  expect_equal(ncol(draws), 2L)  # only PSA-eligible params
  expect_true("p" %in% names(draws))
  expect_true("c" %in% names(draws))
})

test_that("sample_psa beta draws are in [0,1]", {
  params <- define_param("p", 0.3, se = 0.05, distribution = "beta")
  set.seed(42)
  draws <- sample_psa(params, n = 500)
  expect_true(all(draws$p >= 0 & draws$p <= 1))
})

test_that("sample_psa gamma draws are positive", {
  params <- define_param("c", 5000, se = 1000, distribution = "gamma")
  set.seed(42)
  draws <- sample_psa(params, n = 500)
  expect_true(all(draws$c > 0))
})

test_that("sample_psa lognormal draws are positive", {
  params <- define_param("hr", 0.72, lower = 0.58, upper = 0.88,
                         distribution = "lognormal")
  set.seed(42)
  draws <- sample_psa(params, n = 500)
  expect_true(all(draws$hr > 0))
  expect_true(abs(mean(draws$hr) - 0.72) < 0.1)
})

test_that("sample_psa with correlation produces correlated draws", {
  params <- rbind(
    define_param("a", 0.5, se = 0.1, distribution = "normal"),
    define_param("b", 0.5, se = 0.1, distribution = "normal")
  )
  corr <- add_correlation(pairs = list(
    list(params = c("a", "b"), rho = 0.8)
  ))
  set.seed(42)
  draws <- sample_psa(params, n = 1000, correlation = corr)
  r <- cor(draws$a, draws$b)
  expect_true(r > 0.5)  # should be close to 0.8
})

# --- add_correlation ---

test_that("add_correlation creates valid matrix", {
  corr <- add_correlation(pairs = list(
    list(params = c("a", "b"), rho = 0.5),
    list(params = c("b", "c"), rho = -0.3)
  ))
  expect_true(is.matrix(corr$matrix))
  expect_equal(nrow(corr$matrix), 3L)
  expect_equal(corr$matrix["a", "b"], 0.5)
  expect_equal(corr$matrix["b", "a"], 0.5)
  expect_equal(corr$matrix["b", "c"], -0.3)
  # Diagonal = 1
  expect_true(all(diag(corr$matrix) == 1))
})

test_that("add_correlation warns on non-PD matrix", {
  expect_warning(
    add_correlation(pairs = list(
      list(params = c("a", "b"), rho = 0.9),
      list(params = c("a", "c"), rho = 0.9),
      list(params = c("b", "c"), rho = -0.9)
    )),
    "not positive"
  )
})

# --- read_param_csv ---

test_that("read_param_csv reads a valid CSV", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  df <- data.frame(
    name = c("p_death", "c_drug"),
    mean = c(0.05, 5000),
    se = c(0.01, 1000),
    distribution = c("beta", "gamma"),
    stringsAsFactors = FALSE
  )
  utils::write.csv(df, tmp, row.names = FALSE)

  result <- read_param_csv(tmp)
  expect_equal(nrow(result), 2L)
  expect_true("shape1" %in% names(result))
  expect_true(is.finite(result$lower[1]))  # derived from SE
})

test_that("read_param_csv errors on missing name column", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  utils::write.csv(data.frame(value = 1:3), tmp, row.names = FALSE)
  expect_error(read_param_csv(tmp), "name")
})
