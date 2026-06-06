test_that("cea_sick_sicker creates valid model", {
  m <- cea_sick_sicker()
  expect_s3_class(m, "cea_model")
  expect_equal(m$info()$key, "sick_sicker")
  expect_equal(length(m$info()$strategies), 5L)
})

test_that("sick_sicker default params have correct shape", {
  m <- cea_sick_sicker()
  df <- m$default_params()
  expect_true(nrow(df) > 0)
  expect_true(all(c("name", "mean", "lower", "upper", "base", "distribution",
                     "include_psa", "include_tornado") %in% names(df)))
  expect_false(any(duplicated(df$name)))
})

test_that("sick_sicker simulates all strategies", {
  m <- cea_sick_sicker()
  pars <- stats::setNames(as.list(m$default_params()$base), m$default_params()$name)
  for (s in m$info()$strategies) {
    res <- m$simulate(pars, s)
    expect_true(is.finite(res$cost), info = paste("cost finite for", s))
    expect_true(is.finite(res$qaly), info = paste("qaly finite for", s))
    expect_true(is.matrix(res$trace), info = paste("trace is matrix for", s))
    expect_true(is.data.frame(res$survival), info = paste("survival is df for", s))
  }
})

test_that("sick_sicker trace rows sum to ~1", {
  m <- cea_sick_sicker()
  pars <- stats::setNames(as.list(m$default_params()$base), m$default_params()$name)
  res <- m$simulate(pars, "std")
  rs <- rowSums(res$trace)
  expect_true(all(abs(rs - 1) < 1e-10))
})

test_that("sick_sicker death state is monotone non-decreasing", {
  m <- cea_sick_sicker()
  pars <- stats::setNames(as.list(m$default_params()$base), m$default_params()$name)
  res <- m$simulate(pars, "std")
  d <- res$trace[, "D"]
  expect_true(all(diff(d) >= -1e-10))
})

test_that("sick_sicker treatments modify costs vs standard", {
  m <- cea_sick_sicker()
  pars <- stats::setNames(as.list(m$default_params()$base), m$default_params()$name)
  res_std <- m$simulate(pars, "std")
  res_t1 <- m$simulate(pars, "t1")
  expect_true(res_t1$cost > res_std$cost)
})

test_that("sick_sicker survival is in [0,1]", {
  m <- cea_sick_sicker()
  pars <- stats::setNames(as.list(m$default_params()$base), m$default_params()$name)
  res <- m$simulate(pars, "std")
  expect_true(all(res$survival$surv >= 0 & res$survival$surv <= 1))
})

test_that("cea_partitioned_survival creates valid model", {
  m <- cea_partitioned_survival()
  expect_s3_class(m, "cea_model")
  expect_equal(m$info()$key, "ps_weibull")
  expect_equal(length(m$info()$strategies), 6L)
})

test_that("partitioned_survival simulates all strategies", {
  m <- cea_partitioned_survival()
  pars <- stats::setNames(as.list(m$default_params()$base), m$default_params()$name)
  for (s in m$info()$strategies) {
    res <- m$simulate(pars, s)
    expect_true(is.finite(res$cost), info = paste("cost finite for", s))
    expect_true(is.finite(res$qaly), info = paste("qaly finite for", s))
    expect_null(res$trace, info = paste("trace should be NULL for PartSA", s))
    expect_true(is.data.frame(res$survival), info = paste("survival is df for", s))
  }
})

test_that("partitioned_survival returns OS and PFS curves", {
  m <- cea_partitioned_survival()
  pars <- stats::setNames(as.list(m$default_params()$base), m$default_params()$name)
  res <- m$simulate(pars, "std")
  curves <- unique(res$survival$curve)
  expect_true("OS" %in% curves)
  expect_true("PFS" %in% curves)
})

test_that("partitioned_survival OS >= PFS", {
  m <- cea_partitioned_survival()
  pars <- stats::setNames(as.list(m$default_params()$base), m$default_params()$name)
  res <- m$simulate(pars, "std")
  os <- res$survival[res$survival$curve == "OS", ]
  pfs <- res$survival[res$survival$curve == "PFS", ]
  os <- os[order(os$time), ]
  pfs <- pfs[order(pfs$time), ]
  expect_true(all(os$surv >= pfs$surv - 1e-10))
})

test_that("partitioned_survival HR < 1 improves survival", {
  m <- cea_partitioned_survival()
  pars <- stats::setNames(as.list(m$default_params()$base), m$default_params()$name)
  res_std <- m$simulate(pars, "std")
  res_t5 <- m$simulate(pars, "t5")
  expect_true(res_t5$qaly > res_std$qaly)
})

# --- PF -> Progressed -> Death model ---

test_that("cea_pf_prog_death creates valid model", {
  m <- cea_pf_prog_death()
  expect_s3_class(m, "cea_model")
  expect_equal(m$info()$key, "pf_prog_death")
  expect_equal(length(m$info()$strategies), 6L)
})

test_that("pf_prog_death default params have correct shape", {
  m <- cea_pf_prog_death()
  df <- m$default_params()
  expect_true(nrow(df) > 0)
  expect_true(all(c("name", "mean", "lower", "upper", "base", "distribution",
                     "include_psa", "include_tornado") %in% names(df)))
  expect_false(any(duplicated(df$name)))
})

test_that("pf_prog_death simulates all strategies", {
  m <- cea_pf_prog_death()
  pars <- stats::setNames(as.list(m$default_params()$base), m$default_params()$name)
  for (s in m$info()$strategies) {
    res <- m$simulate(pars, s)
    expect_true(is.finite(res$cost), info = paste("cost finite for", s))
    expect_true(is.finite(res$qaly), info = paste("qaly finite for", s))
    expect_true(is.matrix(res$trace), info = paste("trace is matrix for", s))
    expect_true(is.data.frame(res$survival), info = paste("survival is df for", s))
  }
})

test_that("pf_prog_death trace has 3 states", {
  m <- cea_pf_prog_death()
  pars <- stats::setNames(as.list(m$default_params()$base), m$default_params()$name)
  res <- m$simulate(pars, "std")
  expect_equal(ncol(res$trace), 3L)
  expect_equal(colnames(res$trace), c("PF", "Prog", "Death"))
})

test_that("pf_prog_death trace rows sum to ~1", {
  m <- cea_pf_prog_death()
  pars <- stats::setNames(as.list(m$default_params()$base), m$default_params()$name)
  res <- m$simulate(pars, "std")
  rs <- rowSums(res$trace)
  expect_true(all(abs(rs - 1) < 1e-10))
})

test_that("pf_prog_death death state is monotone non-decreasing", {
  m <- cea_pf_prog_death()
  pars <- stats::setNames(as.list(m$default_params()$base), m$default_params()$name)
  res <- m$simulate(pars, "std")
  d <- res$trace[, "Death"]
  expect_true(all(diff(d) >= -1e-10))
})

test_that("pf_prog_death PF state is monotone non-increasing", {
  m <- cea_pf_prog_death()
  pars <- stats::setNames(as.list(m$default_params()$base), m$default_params()$name)
  res <- m$simulate(pars, "std")
  pf <- res$trace[, "PF"]
  expect_true(all(diff(pf) <= 1e-10))
})

test_that("pf_prog_death returns OS and PFS survival curves", {
  m <- cea_pf_prog_death()
  pars <- stats::setNames(as.list(m$default_params()$base), m$default_params()$name)
  res <- m$simulate(pars, "std")
  curves <- unique(res$survival$curve)
  expect_true("OS" %in% curves)
  expect_true("PFS" %in% curves)
})

test_that("pf_prog_death OS >= PFS at all time points", {
  m <- cea_pf_prog_death()
  pars <- stats::setNames(as.list(m$default_params()$base), m$default_params()$name)
  res <- m$simulate(pars, "std")
  os <- res$survival[res$survival$curve == "OS", ]
  pfs <- res$survival[res$survival$curve == "PFS", ]
  os <- os[order(os$time), ]
  pfs <- pfs[order(pfs$time), ]
  expect_true(all(os$surv >= pfs$surv - 1e-10))
})

test_that("pf_prog_death treatments with HR < 1 improve QALYs", {
  m <- cea_pf_prog_death()
  pars <- stats::setNames(as.list(m$default_params()$base), m$default_params()$name)
  res_std <- m$simulate(pars, "std")
  res_t5 <- m$simulate(pars, "t5")
  expect_true(res_t5$qaly > res_std$qaly)
})

test_that("pf_prog_death survival in [0,1]", {
  m <- cea_pf_prog_death()
  pars <- stats::setNames(as.list(m$default_params()$base), m$default_params()$name)
  res <- m$simulate(pars, "std")
  expect_true(all(res$survival$surv >= 0 & res$survival$surv <= 1))
})

test_that("pf_prog_death works through full pipeline", {
  m <- cea_pf_prog_death()
  det <- run_deterministic(m, m$default_params(), wtp = 100000)
  expect_equal(nrow(det$table), 6L)
  expect_true(det$ref %in% m$info()$strategies)

  set.seed(42)
  psa <- run_psa(m, m$default_params(), n_sim = 20)
  expect_true(psa$used > 0)
  expect_equal(psa$dropped, 0L)

  health <- validate_params_and_model(m, m$default_params())
  expect_true(all(health$status == "PASS"))
})

# --- End-to-end custom model ---

test_that("end-to-end: custom model through full pipeline", {
  m <- new_cea_model(
    info = function() list(key = "coin", name = "Coin Flip", strategies = c("heads", "tails")),
    params = function() rbind(
      cea_param("p_win", 0.5, 0.3, 0.7, distribution = "beta"),
      cea_param("prize", 1000, 500, 2000, distribution = "gamma")
    ),
    simulate = function(pars, strategy) {
      p <- if (strategy == "heads") pars[["p_win"]] else 1 - pars[["p_win"]]
      list(cost = pars[["prize"]] * (1 - p), qaly = p)
    }
  )

  det <- run_deterministic(m, m$default_params(), wtp = 10000)
  expect_equal(nrow(det$table), 2L)

  set.seed(99)
  psa <- run_psa(m, m$default_params(), n_sim = 30)
  expect_true(psa$used > 0)

  ceac <- compute_ceac(psa$results, m$info()$strategies, wtp_max = 20000, wtp_step = 5000)
  expect_true(nrow(ceac) > 0)

  health <- validate_params_and_model(m, m$default_params())
  expect_true(all(health$status == "PASS"))
})
