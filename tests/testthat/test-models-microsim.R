test_that("cea_microsim creates valid model", {
  m <- cea_microsim()
  expect_s3_class(m, "cea_model")
  expect_equal(m$info()$key, "microsim_chronic")
  expect_equal(length(m$info()$strategies), 2L)
})

test_that("microsim simulates both strategies", {
  m <- cea_microsim()
  params <- m$default_params()
  params$base[params$name == "n_patients"] <- 200
  pars <- stats::setNames(as.list(params$base), params$name)

  set.seed(42)
  for (s in m$info()$strategies) {
    res <- m$simulate(pars, s)
    expect_true(is.finite(res$cost), info = paste("cost finite for", s))
    expect_true(is.finite(res$qaly), info = paste("qaly finite for", s))
    expect_true(res$cost > 0, info = paste("cost positive for", s))
    expect_true(res$qaly > 0, info = paste("qaly positive for", s))
  }
})

test_that("microsim returns trace", {
  m <- cea_microsim()
  params <- m$default_params()
  params$base[params$name == "n_patients"] <- 100
  pars <- stats::setNames(as.list(params$base), params$name)

  set.seed(42)
  res <- m$simulate(pars, "standard")
  expect_true(is.matrix(res$trace))
  expect_equal(ncol(res$trace), 4L)
  expect_equal(colnames(res$trace), c("Stable", "Progressed", "Severe", "Dead"))

  # Rows should sum to ~1
  rs <- rowSums(res$trace)
  expect_true(all(abs(rs - 1) < 0.01))
})

test_that("microsim returns survival", {
  m <- cea_microsim()
  params <- m$default_params()
  params$base[params$name == "n_patients"] <- 100
  pars <- stats::setNames(as.list(params$base), params$name)

  set.seed(42)
  res <- m$simulate(pars, "standard")
  expect_true(is.data.frame(res$survival))
  expect_true(all(c("time", "surv", "curve") %in% names(res$survival)))
  expect_true(all(res$survival$surv >= 0 & res$survival$surv <= 1))
})

test_that("new_treatment produces better outcomes than standard", {
  m <- cea_microsim()
  params <- m$default_params()
  params$base[params$name == "n_patients"] <- 500
  pars <- stats::setNames(as.list(params$base), params$name)

  set.seed(42)
  res_std <- m$simulate(pars, "standard")
  res_new <- m$simulate(pars, "new_treatment")

  # New treatment should have more QALYs (better efficacy)
  expect_true(res_new$qaly > res_std$qaly)
  # New treatment should cost more (higher drug cost)
  expect_true(res_new$cost > res_std$cost)
})

test_that("microsim works through run_deterministic", {
  m <- cea_microsim()
  params <- m$default_params()
  params$base[params$name == "n_patients"] <- 200

  set.seed(42)
  det <- run_deterministic(m, params, wtp = 50000)
  expect_equal(nrow(det$table), 2L)
  expect_true(det$ref %in% m$info()$strategies)
})

test_that("microsim works through run_psa", {
  m <- cea_microsim()
  params <- m$default_params()
  params$base[params$name == "n_patients"] <- 100

  set.seed(42)
  psa <- run_psa(m, params, n_sim = 10)
  expect_true(psa$used > 0)
  expect_equal(psa$dropped, 0L)
})

test_that("microsim death is monotone non-decreasing", {
  m <- cea_microsim()
  params <- m$default_params()
  params$base[params$name == "n_patients"] <- 300
  pars <- stats::setNames(as.list(params$base), params$name)

  set.seed(42)
  res <- m$simulate(pars, "standard")
  d <- res$trace[, "Dead"]
  expect_true(all(diff(d) >= -0.01))
})

test_that("microsim has predefined scenarios", {
  m <- cea_microsim()
  expect_true(!is.null(m$scenarios))
  expect_true(length(m$scenarios) >= 3)
})
