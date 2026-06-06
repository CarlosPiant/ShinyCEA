test_that("cea_discount_weights returns correct length", {
  w <- cea_discount_weights(10, rate = 0.03, cycle_length = 1)
  expect_length(w, 10)
})

test_that("cea_discount_weights returns all 1s when rate is 0", {
  w <- cea_discount_weights(5, rate = 0, cycle_length = 1)
  expect_equal(w, rep(1, 5))
})

test_that("cea_discount_weights is monotone decreasing", {
  w <- cea_discount_weights(20, rate = 0.05, cycle_length = 1)
  expect_true(all(diff(w) < 0))
})

test_that("cea_discount_weights first value depends on first_cycle", {
  w0 <- cea_discount_weights(5, rate = 0.03, cycle_length = 1, first_cycle = 0L)
  w1 <- cea_discount_weights(5, rate = 0.03, cycle_length = 1, first_cycle = 1L)
  expect_equal(w0[1], 1)
  expect_true(w1[1] < 1)
})

test_that("cea_discount_weights handles monthly cycles", {
  w <- cea_discount_weights(120, rate = 0.03, cycle_length = 1/12)
  expect_length(w, 120)
  expect_true(w[1] > w[120])
  expect_true(all(w > 0 & w <= 1))
})

test_that("cea_discounted_sum applies discount correctly", {
  vals <- rep(100, 10)
  undiscounted <- sum(vals)
  discounted <- cea_discounted_sum(vals, rate = 0.05, cycle_length = 1)
  expect_true(discounted < undiscounted)
  expect_true(discounted > 0)
})

test_that("cea_discounted_sum with rate 0 equals plain sum", {
  vals <- c(100, 200, 300)
  expect_equal(cea_discounted_sum(vals, rate = 0), sum(vals))
})

test_that("bundled models apply discounting", {
  m <- cea_sick_sicker()
  pars_no_disc <- m$default_params()
  pars_no_disc$base[pars_no_disc$name == "d_cost"] <- 0
  pars_no_disc$base[pars_no_disc$name == "d_qaly"] <- 0

  pars_disc <- m$default_params()

  det_no <- run_deterministic(m, pars_no_disc, wtp = 100000)
  det_yes <- run_deterministic(m, pars_disc, wtp = 100000)

  cost_no <- det_no$table$Cost[det_no$table$Strategy == "std"]
  cost_yes <- det_yes$table$Cost[det_yes$table$Strategy == "std"]
  expect_true(cost_yes < cost_no)

  qaly_no <- det_no$table$QALY[det_no$table$Strategy == "std"]
  qaly_yes <- det_yes$table$QALY[det_yes$table$Strategy == "std"]
  expect_true(qaly_yes < qaly_no)
})

test_that("partitioned survival model applies discounting", {
  m <- cea_partitioned_survival()
  pars_no <- m$default_params()
  pars_no$base[pars_no$name == "d_cost"] <- 0
  pars_no$base[pars_no$name == "d_qaly"] <- 0

  pars_yes <- m$default_params()

  p0 <- stats::setNames(as.list(pars_no$base), pars_no$name)
  p1 <- stats::setNames(as.list(pars_yes$base), pars_yes$name)

  r0 <- m$simulate(p0, "std")
  r1 <- m$simulate(p1, "std")

  expect_true(r1$cost < r0$cost)
  expect_true(r1$qaly < r0$qaly)
})
