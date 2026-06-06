test_that("cea_table computes correct NMB", {
  res <- list(
    A = list(cost = 1000, qaly = 2),
    B = list(cost = 5000, qaly = 3)
  )
  tab <- cea_table(res, wtp = 100000)
  expect_equal(nrow(tab), 2L)
  expect_equal(tab$NMB[tab$Strategy == "A"], 100000 * 2 - 1000)
  expect_equal(tab$NMB[tab$Strategy == "B"], 100000 * 3 - 5000)
})

test_that("frontier_analysis identifies dominated strategies", {
  df <- data.frame(
    Strategy = c("A", "B", "C"),
    Cost = c(100, 200, 150),
    QALY = c(1, 1.5, 0.8),
    stringsAsFactors = FALSE
  )
  fa <- frontier_analysis(df)

  expect_true("Dominated" %in% fa$classified$Status)
  expect_equal(fa$classified$Status[fa$classified$Strategy == "C"], "Dominated")
  expect_true("A" %in% fa$frontier$Strategy)
  expect_true("B" %in% fa$frontier$Strategy)
})

test_that("frontier_analysis computes sequential ICERs", {
  df <- data.frame(
    Strategy = c("A", "B"),
    Cost = c(100, 300),
    QALY = c(1, 2),
    stringsAsFactors = FALSE
  )
  fa <- frontier_analysis(df)
  expect_equal(nrow(fa$icers), 1L)
  expect_equal(fa$icers$ICER[1], 200)
})

test_that("frontier_analysis handles extended dominance", {
  df <- data.frame(
    Strategy = c("A", "B", "C"),
    Cost = c(100, 250, 500),
    QALY = c(1, 1.5, 3),
    stringsAsFactors = FALSE
  )
  fa <- frontier_analysis(df)
  expect_true(all(fa$classified$Status %in% c("Frontier", "Dominated", "Extendedly dominated")))
})

test_that("frontier_analysis handles single-strategy frontier", {
  df <- data.frame(
    Strategy = c("A", "B"),
    Cost = c(100, 200),
    QALY = c(2, 1),
    stringsAsFactors = FALSE
  )
  fa <- frontier_analysis(df)
  expect_equal(nrow(fa$frontier), 1L)
  expect_equal(fa$frontier$Strategy, "A")
  expect_equal(fa$classified$Status[fa$classified$Strategy == "B"], "Dominated")
})

test_that("threshold_analysis finds crossing", {
  m <- cea_sick_sicker()
  params <- m$default_params()
  pars <- stats::setNames(as.list(params$base), params$name)

  res <- threshold_analysis(
    m, pars, "drug_t1", "std", "t1",
    range_min = 0, range_max = 5000, n_points = 20,
    wtp = 100000, mode = "inb"
  )

  expect_true(is.list(res))
  expect_true(all(c("grid", "threshold", "message") %in% names(res)))
  expect_true(is.data.frame(res$grid))
  expect_equal(nrow(res$grid), 20L)
})

test_that("threshold_analysis handles icer mode", {
  m <- cea_sick_sicker()
  params <- m$default_params()
  pars <- stats::setNames(as.list(params$base), params$name)

  res <- threshold_analysis(
    m, pars, "rr_prog_t1", "std", "t1",
    range_min = 0.5, range_max = 1.5, n_points = 15,
    wtp = 100000, mode = "icer"
  )
  expect_true(is.data.frame(res$grid))
})

test_that("build_tornado_data returns correct structure", {
  m <- cea_sick_sicker()
  params <- m$default_params()
  pars <- stats::setNames(as.list(params$base), params$name)
  df_tor <- params[params$include_tornado == TRUE &
                     (is.finite(params$lower) | is.finite(params$upper)), ]

  tor <- build_tornado_data(df_tor, pars, m, "std", "t1", metric = "INB", wtp = 100000)

  expect_true(is.data.frame(tor))
  expect_true(all(c("name", "bound", "value", "base") %in% names(tor)))
  expect_true(nrow(tor) > 0)
  expect_true(all(tor$bound %in% c("Lower bound", "Upper bound")))

  diag <- attr(tor, "diag")
  expect_true(is.list(diag))
  expect_true(diag$base_is_finite)
  expect_true(diag$n_eligible > 0)
})

test_that("build_tornado_data works with ICER metric", {
  m <- cea_sick_sicker()
  params <- m$default_params()
  pars <- stats::setNames(as.list(params$base), params$name)
  df_tor <- params[params$include_tornado == TRUE &
                     (is.finite(params$lower) | is.finite(params$upper)), ]

  tor <- build_tornado_data(df_tor, pars, m, "std", "t1", metric = "ICER", wtp = 100000)
  expect_true(nrow(tor) > 0)
})

test_that("validate_params_and_model returns health checks", {
  m <- cea_sick_sicker()
  df <- m$default_params()
  health <- validate_params_and_model(m, df)

  expect_true(is.data.frame(health))
  expect_true(all(c("check", "status") %in% names(health)))
  expect_true(all(health$status %in% c("PASS", "WARN", "FAIL")))
  expect_true(sum(health$status == "PASS") > 0)
})

test_that("validate_params_and_model catches empty df", {
  m <- cea_sick_sicker()
  health <- validate_params_and_model(m, data.frame())
  expect_true(any(health$status == "FAIL"))
})
