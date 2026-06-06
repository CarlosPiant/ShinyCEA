test_that("run_scenarios returns correct structure", {
  m <- cea_sick_sicker()
  scenarios <- list(
    "Base case" = list(),
    "No discount" = list(d_cost = 0, d_qaly = 0)
  )
  res <- run_scenarios(m, m$default_params(), scenarios, wtp = 100000)

  expect_true(is.data.frame(res))
  expect_true(all(c("Scenario", "Strategy", "Cost", "QALY", "NMB",
                     "ICER_vsRef", "Reference") %in% names(res)))
  expect_equal(length(unique(res$Scenario)), 2L)
  expect_equal(nrow(res), 2L * length(m$info()$strategies))
})

test_that("run_scenarios applies overrides correctly", {
  m <- cea_sick_sicker()
  scenarios <- list(
    "Base" = list(),
    "Expensive t1" = list(drug_t1 = 10000)
  )
  res <- run_scenarios(m, m$default_params(), scenarios, wtp = 100000)

  cost_base <- res$Cost[res$Scenario == "Base" & res$Strategy == "t1"]
  cost_exp <- res$Cost[res$Scenario == "Expensive t1" & res$Strategy == "t1"]
  expect_true(cost_exp > cost_base)
})

test_that("run_scenarios respects pinned reference", {
  m <- cea_sick_sicker()
  scenarios <- list("Test" = list())
  res <- run_scenarios(m, m$default_params(), scenarios, wtp = 100000, ref_strategy = "std")
  expect_true(all(res$Reference == "std"))
})

test_that("run_scenarios errors on non-named list", {
  m <- cea_sick_sicker()
  expect_error(run_scenarios(m, m$default_params(), list(list()), wtp = 100000),
               "named list")
})

test_that("summarize_scenarios picks highest NMB", {
  m <- cea_sick_sicker()
  scenarios <- list(
    "Base" = list(),
    "No discount" = list(d_cost = 0, d_qaly = 0)
  )
  res <- run_scenarios(m, m$default_params(), scenarios, wtp = 100000)
  summ <- summarize_scenarios(res)

  expect_true(is.data.frame(summ))
  expect_equal(nrow(summ), 2L)
  expect_true(all(c("Scenario", "Optimal_Strategy", "Cost", "QALY", "NMB") %in% names(summ)))

  for (sc in summ$Scenario) {
    sub <- res[res$Scenario == sc, ]
    expect_equal(summ$Optimal_Strategy[summ$Scenario == sc],
                 sub$Strategy[which.max(sub$NMB)])
  }
})

test_that("run_scenarios handles multiple overrides per scenario", {
  m <- cea_sick_sicker()
  scenarios <- list(
    "Pessimistic" = list(p_HS_std = 0.08, p_S1S2_std = 0.12, u_S1 = 0.60)
  )
  res <- run_scenarios(m, m$default_params(), scenarios, wtp = 100000)
  expect_equal(nrow(res), length(m$info()$strategies))
  expect_true(all(is.finite(res$Cost)))
})

test_that("run_scenarios works with partitioned survival model", {
  m <- cea_partitioned_survival()
  scenarios <- list(
    "Base" = list(),
    "Longer horizon" = list(time_horizon = 15)
  )
  res <- run_scenarios(m, m$default_params(), scenarios, wtp = 100000)
  expect_equal(length(unique(res$Scenario)), 2L)
})

# --- Forest plot ---

test_that("plot_scenario_forest returns ggplot for ICER", {
  m <- cea_sick_sicker()
  scenarios <- list(
    "Base" = list(),
    "No discount" = list(d_cost = 0, d_qaly = 0),
    "High cost" = list(drug_t1 = 2000)
  )
  res <- run_scenarios(m, m$default_params(), scenarios, wtp = 100000,
                       ref_strategy = "std")
  p <- plot_scenario_forest(res, target = "t1", metric = "ICER", wtp = 100000)
  expect_s3_class(p, "ggplot")
})

test_that("plot_scenario_forest returns ggplot for NMB", {
  m <- cea_sick_sicker()
  scenarios <- list("Base" = list(), "Alt" = list(u_H = 0.80))
  res <- run_scenarios(m, m$default_params(), scenarios, wtp = 100000)
  p <- plot_scenario_forest(res, target = "t4", metric = "NMB", wtp = 100000)
  expect_s3_class(p, "ggplot")
})

test_that("plot_scenario_forest handles missing strategy gracefully", {
  m <- cea_sick_sicker()
  scenarios <- list("Base" = list())
  res <- run_scenarios(m, m$default_params(), scenarios, wtp = 100000)
  p <- plot_scenario_forest(res, target = "nonexistent", metric = "ICER")
  expect_s3_class(p, "ggplot")
})

test_that("plot_scenario_forest accepts palette", {
  m <- cea_sick_sicker()
  scenarios <- list("Base" = list(), "Alt" = list(d_cost = 0))
  res <- run_scenarios(m, m$default_params(), scenarios, wtp = 100000,
                       ref_strategy = "std")
  p <- plot_scenario_forest(res, target = "t1", metric = "ICER",
                            wtp = 100000, palette = cea_palette("lancet"))
  expect_s3_class(p, "ggplot")
})

test_that("plot_scenario_forest colors by cost-effectiveness", {
  m <- cea_sick_sicker()
  scenarios <- list(
    "Cheap" = list(drug_t1 = 10),
    "Expensive" = list(drug_t1 = 50000)
  )
  res <- run_scenarios(m, m$default_params(), scenarios, wtp = 100000,
                       ref_strategy = "std")
  p <- plot_scenario_forest(res, target = "t1", metric = "ICER", wtp = 100000)
  expect_s3_class(p, "ggplot")
})
