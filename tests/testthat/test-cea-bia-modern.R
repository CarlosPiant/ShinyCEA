# --- define_population ---

test_that("define_population returns correct structure", {
  pop <- define_population(1000000, prevalence = 0.01, years = 5)
  expect_s3_class(pop, "bia_population")
  expect_length(pop$eligible, 5L)
  expect_true(all(pop$eligible > 0))
})

test_that("define_population applies growth", {
  pop <- define_population(100000, prevalence = 0.1, annual_growth = 0.05, years = 3)
  expect_true(pop$eligible[3] > pop$eligible[1])
})

test_that("define_population applies mortality", {
  pop <- define_population(100000, prevalence = 0.1, mortality_rate = 0.1, years = 3)
  expect_true(pop$eligible[3] < pop$eligible[1])
})

test_that("define_population adds incidence", {
  pop_no <- define_population(100000, prevalence = 0.01, incident_annual = 0, years = 3)
  pop_yes <- define_population(100000, prevalence = 0.01, incident_annual = 500, years = 3)
  expect_true(pop_yes$eligible[3] > pop_no$eligible[3])
})

# --- define_market_shares ---

test_that("define_market_shares validates sums", {
  expect_error(define_market_shares(c(A = 0.5), c(A = 1)), "sum to 1")
  shares <- define_market_shares(c(A = 0.6, B = 0.4), c(A = 0.3, B = 0.3, C = 0.4))
  expect_s3_class(shares, "bia_shares")
})

# --- define_uptake_curve ---

test_that("linear uptake transitions correctly", {
  shares <- define_market_shares(c(A = 1), c(A = 0.4, B = 0.6))
  uptake <- define_uptake_curve(shares, years = 3, shape = "linear")
  expect_length(uptake, 3L)
  expect_equal(uptake[[1]][["A"]], 1, tolerance = 0.01)
  expect_equal(uptake[[3]][["B"]], 0.6, tolerance = 0.01)
})

test_that("s_curve uptake works", {
  shares <- define_market_shares(c(A = 1), c(A = 0.3, B = 0.7))
  uptake <- define_uptake_curve(shares, years = 5, shape = "s_curve")
  expect_length(uptake, 5L)
  # Midpoint should be ~50% of target
  expect_true(all(vapply(uptake, sum, numeric(1)) - 1 < 0.01))
})

test_that("immediate uptake reaches target in year 1", {
  shares <- define_market_shares(c(A = 1), c(A = 0.2, B = 0.8))
  uptake <- define_uptake_curve(shares, years = 3, shape = "immediate")
  expect_equal(uptake[[1]][["B"]], 0.8, tolerance = 0.01)
})

# --- define_cost_components ---

test_that("define_cost_components computes totals", {
  costs <- define_cost_components(
    std = list(drug = 500, admin = 200),
    new_drug = list(drug = 5000, admin = 300, monitoring = 333)
  )
  expect_s3_class(costs, "bia_costs")
  expect_equal(costs$totals[["std"]], 700)
  expect_equal(costs$totals[["new_drug"]], 5633)
})

# --- run_bia_modern ---

test_that("run_bia_modern with cost components works", {
  pop <- define_population(100000, prevalence = 0.01, years = 3)
  shares <- define_market_shares(c(std = 1), c(std = 0.5, new = 0.5))
  costs <- define_cost_components(
    std = list(drug = 500, admin = 200),
    new = list(drug = 3000, admin = 300)
  )

  res <- run_bia_modern(pop, shares, costs)
  expect_true(is.list(res))
  expect_true(all(c("annual", "impact", "by_component", "total_impact") %in% names(res)))
  expect_equal(nrow(res$impact), 3L)
  expect_true(res$total_impact > 0)  # new drug costs more
})

test_that("run_bia_modern with CEA model works", {
  m <- cea_sick_sicker()
  pop <- define_population(50000, prevalence = 0.02, years = 2)
  shares <- define_market_shares(
    c(std = 0.6, t1 = 0.4),
    c(std = 0.3, t1 = 0.3, t4 = 0.4)
  )
  res <- run_bia_modern(pop, shares, model = m, params_df = m$default_params())
  expect_equal(nrow(res$impact), 2L)
})

test_that("run_bia_modern same shares gives zero impact", {
  pop <- define_population(100000, prevalence = 0.01, years = 2)
  shares <- define_market_shares(c(A = 0.5, B = 0.5), c(A = 0.5, B = 0.5))
  costs <- define_cost_components(A = list(drug = 100), B = list(drug = 200))
  uptake <- define_uptake_curve(shares, years = 2, shape = "immediate")
  res <- run_bia_modern(pop, shares, costs, uptake)
  expect_true(all(abs(res$impact$Budget_Impact) < 1))
})

# --- scenario_bia ---

test_that("scenario_bia compares scenarios", {
  pop <- define_population(100000, prevalence = 0.01, years = 3)
  shares <- define_market_shares(c(A = 1), c(A = 0.5, B = 0.5))
  costs <- define_cost_components(A = list(drug = 500), B = list(drug = 3000))

  scenarios <- list(
    "Linear uptake" = list(
      population = pop, shares = shares, costs = costs,
      uptake = define_uptake_curve(shares, 3, "linear")
    ),
    "Immediate uptake" = list(
      population = pop, shares = shares, costs = costs,
      uptake = define_uptake_curve(shares, 3, "immediate")
    )
  )
  res <- scenario_bia(scenarios)
  expect_true(is.data.frame(res))
  expect_true("Cumulative_Impact" %in% names(res))
  expect_equal(length(unique(res$Scenario)), 2L)
})

# --- plot_budget_impact ---

test_that("plot_budget_impact returns ggplot", {
  pop <- define_population(100000, prevalence = 0.01, years = 3)
  shares <- define_market_shares(c(A = 1), c(A = 0.5, B = 0.5))
  costs <- define_cost_components(A = list(drug = 500), B = list(drug = 3000))
  res <- run_bia_modern(pop, shares, costs)
  expect_s3_class(plot_budget_impact(res), "ggplot")
  expect_s3_class(plot_budget_impact(res, show_components = TRUE), "ggplot")
})

# --- table_annual_budget ---

test_that("table_annual_budget returns formatted table", {
  pop <- define_population(100000, prevalence = 0.01, years = 3)
  shares <- define_market_shares(c(A = 1), c(A = 0.5, B = 0.5))
  costs <- define_cost_components(A = list(drug = 500), B = list(drug = 3000))
  res <- run_bia_modern(pop, shares, costs)
  tab <- table_annual_budget(res)
  expect_true(is.data.frame(tab))
  expect_equal(nrow(tab), 3L)
  expect_true(all(c("Year", "Eligible_Patients", "Cost_Current",
                     "Cost_New", "Annual_Impact", "Cumulative_Impact") %in% names(tab)))
})
