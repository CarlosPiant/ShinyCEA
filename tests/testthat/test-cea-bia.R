test_that("run_bia returns correct structure", {
  m <- cea_sick_sicker()
  strats <- m$info()$strategies
  cur <- stats::setNames(rep(1 / length(strats), length(strats)), strats)
  new_mix <- cur; new_mix["std"] <- 0; new_mix["t4"] <- new_mix["t4"] + cur["std"]

  res <- run_bia(m, m$default_params(), population = 10000,
                 current_mix = cur, new_mix = new_mix, years = 3)

  expect_true(is.list(res))
  expect_true(all(c("annual", "impact", "total_impact") %in% names(res)))
  expect_true(is.data.frame(res$annual))
  expect_true(is.data.frame(res$impact))
  expect_equal(nrow(res$impact), 3L)
  expect_true(is.numeric(res$total_impact))
})

test_that("run_bia annual has both scenarios", {
  m <- cea_sick_sicker()
  strats <- m$info()$strategies
  cur <- stats::setNames(rep(1 / length(strats), length(strats)), strats)
  new_mix <- cur

  res <- run_bia(m, m$default_params(), population = 5000,
                 current_mix = cur, new_mix = new_mix, years = 2)

  expect_true(all(c("Current", "New") %in% res$annual$Scenario))
})

test_that("run_bia same mix gives zero impact", {
  m <- cea_sick_sicker()
  strats <- m$info()$strategies
  mix <- stats::setNames(rep(1 / length(strats), length(strats)), strats)

  res <- run_bia(m, m$default_params(), population = 10000,
                 current_mix = mix, new_mix = mix, years = 3)

  expect_true(all(abs(res$impact$Budget_Impact) < 0.01))
  expect_true(abs(res$total_impact) < 0.01)
})

test_that("run_bia population growth increases patients", {
  m <- cea_sick_sicker()
  strats <- m$info()$strategies
  mix <- stats::setNames(rep(1 / length(strats), length(strats)), strats)

  res <- run_bia(m, m$default_params(), population = 10000,
                 current_mix = mix, new_mix = mix, years = 3,
                 annual_growth = 0.05)

  yr1_patients <- sum(res$annual$Patients[res$annual$Year == 1 &
                                            res$annual$Scenario == "Current"])
  yr3_patients <- sum(res$annual$Patients[res$annual$Year == 3 &
                                            res$annual$Scenario == "Current"])
  expect_true(yr3_patients > yr1_patients)
})

test_that("linear_uptake creates correct transition", {
  from <- c(A = 1, B = 0)
  to <- c(A = 0, B = 1)
  uptake <- linear_uptake(from, to, years = 3)

  expect_length(uptake, 3L)
  expect_equal(uptake[[1]][["A"]], 1)
  expect_equal(uptake[[3]][["B"]], 1)
  expect_true(abs(uptake[[2]][["A"]] - 0.5) < 0.01)
})

test_that("run_bia with linear uptake list works", {
  m <- cea_sick_sicker()
  strats <- m$info()$strategies
  cur <- stats::setNames(rep(1 / length(strats), length(strats)), strats)
  target <- cur; target["std"] <- 0; target["t4"] <- target["t4"] + cur["std"]
  new_mix <- linear_uptake(cur, target, years = 3)

  res <- run_bia(m, m$default_params(), population = 10000,
                 current_mix = cur, new_mix = new_mix, years = 3)

  expect_equal(nrow(res$impact), 3L)
})

test_that("run_bia errors on bad mix", {
  m <- cea_sick_sicker()
  expect_error(
    run_bia(m, m$default_params(), population = 100,
            current_mix = c(A = 1), new_mix = c(A = 1), years = 1),
    "unknown strategies"
  )
})

test_that("plot_bia returns ggplot", {
  impact_df <- data.frame(Year = 1:3, Cost_Current = c(1e6, 1e6, 1e6),
                          Cost_New = c(1.2e6, 1.3e6, 1.4e6),
                          Budget_Impact = c(2e5, 3e5, 4e5))
  p <- plot_bia(impact_df)
  expect_s3_class(p, "ggplot")
})

test_that("plot_bia_waterfall returns ggplot", {
  impact_df <- data.frame(Year = 1:3, Cost_Current = c(1e6, 1e6, 1e6),
                          Cost_New = c(1.2e6, 0.9e6, 1.1e6),
                          Budget_Impact = c(2e5, -1e5, 1e5))
  p <- plot_bia_waterfall(impact_df)
  expect_s3_class(p, "ggplot")
})
