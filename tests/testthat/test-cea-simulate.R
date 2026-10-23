test_that("safe_simulate returns NULL on model error", {
  bad_model <- structure(
    list(simulate = function(pars, strategy) stop("boom")),
    class = "cea_model"
  )
  expect_null(safe_simulate(bad_model, list(), "A"))
})

test_that("safe_simulate returns NULL for non-finite cost", {
  m <- structure(
    list(simulate = function(pars, strategy) list(cost = Inf, qaly = 1)),
    class = "cea_model"
  )
  expect_null(safe_simulate(m, list(), "A"))
})

test_that("safe_simulate preserves trace and survival", {
  tr <- matrix(c(1, 0, 0.5, 0.5), nrow = 2, byrow = TRUE)
  sv <- data.frame(time = c(0, 1), surv = c(1, 0.5))
  m <- structure(
    list(simulate = function(pars, strategy) {
      list(cost = 100, qaly = 1, trace = tr, survival = sv)
    }),
    class = "cea_model"
  )
  res <- safe_simulate(m, list(), "A")
  expect_equal(res$cost, 100)
  expect_equal(res$qaly, 1)
  expect_true(is.matrix(res$trace))
  expect_true(is.data.frame(res$survival))
})

test_that("run_deterministic produces correct structure", {
  m <- cea_sick_sicker()
  det <- run_deterministic(m, m$default_params(), wtp = 100000)

  expect_true(is.list(det))
  expect_true(all(c("res", "table", "ref", "pars", "frontier",
                     "icers", "frontier_status", "strategies") %in% names(det)))
  expect_equal(nrow(det$table), length(m$info()$strategies))
  expect_true(det$ref %in% m$info()$strategies)
  expect_true(all(c("Strategy", "Cost", "QALY", "NMB",
                     "IncCost_vsRef", "IncQALY_vsRef", "ICER_vsRef") %in% names(det$table)))
})

test_that("run_deterministic respects pinned reference", {
  m <- cea_sick_sicker()
  det <- run_deterministic(m, m$default_params(), wtp = 100000, ref_strategy = "std")
  expect_equal(det$ref, "std")
  expect_equal(det$table$IncCost_vsRef[det$table$Strategy == "std"], 0)
})

test_that("run_deterministic errors on simulation failure", {
  bad <- new_cea_model(
    info = function() list(key = "b", name = "B", strategies = c("A", "B")),
    params = function() cea_param("p", 1, 0, 2),
    # Fails only away from the defaults, so the model passes validation
    # (which now simulates every strategy) and the run itself must fail.
    simulate = function(pars, strategy) {
      if (strategy == "B" && pars$p > 1) stop("fail")
      list(cost = 10, qaly = 1)
    }
  )
  pd <- bad$default_params(); pd$base <- 2
  expect_error(run_deterministic(bad, pd), "Simulation failed")
})

test_that("run_psa returns correct structure", {
  m <- cea_sick_sicker()
  set.seed(123)
  psa <- run_psa(m, m$default_params(), n_sim = 20)

  expect_true(is.list(psa))
  expect_true(all(c("results", "draws", "dropped", "used") %in% names(psa)))
  expect_true(is.data.frame(psa$results))
  expect_true(psa$used > 0)
  expect_equal(psa$used + psa$dropped, 20L)

  strategies <- m$info()$strategies
  expected_cols <- as.vector(rbind(paste0(strategies, ".cost"), paste0(strategies, ".q")))
  expect_true(all(expected_cols %in% names(psa$results)))
})

test_that("run_psa draws are recorded for include_psa params", {
  m <- cea_sick_sicker()
  set.seed(123)
  psa <- run_psa(m, m$default_params(), n_sim = 10)
  expect_true(is.data.frame(psa$draws))
  expect_true(ncol(psa$draws) > 0)
  expect_equal(nrow(psa$draws), psa$used)
})

test_that("run_psa progress callback is called", {
  m <- cea_sick_sicker()
  calls <- 0L
  set.seed(123)
  run_psa(m, m$default_params(), n_sim = 10,
          progress_callback = function(i, n) calls <<- calls + 1L)
  expect_true(calls > 0)
})

test_that("compute_ceac returns correct format", {
  m <- cea_sick_sicker()
  set.seed(123)
  psa <- run_psa(m, m$default_params(), n_sim = 50)
  ceac <- compute_ceac(psa$results, m$info()$strategies, wtp_max = 200000, wtp_step = 50000)

  expect_true(is.data.frame(ceac))
  expect_true(all(c("WTP", "Strategy", "ProbCE") %in% names(ceac)))
  expect_true(all(ceac$ProbCE >= 0 & ceac$ProbCE <= 1))

  wtp_vals <- unique(ceac$WTP)
  for (w in wtp_vals) {
    sub <- ceac[ceac$WTP == w, ]
    expect_equal(sum(sub$ProbCE), 1, tolerance = 1e-10)
  }
})
