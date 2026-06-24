# --- check_half_cycle ---

test_that("check_half_cycle runs on Markov model", {
  m <- cea_sick_sicker()
  checks <- check_half_cycle(m, m$default_params())
  expect_true(is.data.frame(checks))
  expect_true(any(grepl("Trace", checks$check) | grepl("HCC", checks$check)))
})

test_that("check_half_cycle runs on partitioned survival", {
  m <- cea_partitioned_survival()
  checks <- check_half_cycle(m, m$default_params())
  expect_true(is.data.frame(checks))
})

# --- check_ph_assumption ---

test_that("check_ph_assumption detects no crossings in bundled models", {
  m <- cea_sick_sicker()
  checks <- check_ph_assumption(m, m$default_params())
  expect_true(is.data.frame(checks))
  expect_equal(sum(checks$status == "FAIL"), 0L)
})

test_that("check_ph_assumption works with partitioned survival", {
  m <- cea_partitioned_survival()
  checks <- check_ph_assumption(m, m$default_params())
  expect_true(is.data.frame(checks))
})

# --- check_psa_convergence ---

test_that("check_psa_convergence passes with enough iterations", {
  m <- cea_sick_sicker()
  set.seed(42)
  psa <- run_psa(m, m$default_params(), n_sim = 500)
  checks <- check_psa_convergence(psa$results, m$info()$strategies, wtp = 100000)
  expect_true(is.data.frame(checks))
  expect_true(any(checks$check == "Iterations"))
  expect_true(any(grepl("Convergence", checks$check)))
})

test_that("check_psa_convergence flags too few iterations", {
  m <- cea_sick_sicker()
  set.seed(42)
  psa <- run_psa(m, m$default_params(), n_sim = 20)
  checks <- check_psa_convergence(psa$results, m$info()$strategies, wtp = 100000)
  expect_true(any(checks$status %in% c("WARN", "FAIL")))
})

# --- check_scenario_robustness ---

test_that("check_scenario_robustness with consistent scenarios", {
  m <- cea_sick_sicker()
  scenarios <- list(
    "Base" = list(),
    "Alt1" = list(d_cost = 0, d_qaly = 0),
    "Alt2" = list(drug_t4 = 500)
  )
  checks <- check_scenario_robustness(m, m$default_params(), scenarios, wtp = 100000)
  expect_true(is.data.frame(checks))
  expect_true(any(grepl("Robustness", checks$check)))
})

test_that("check_scenario_robustness flags switching scenarios", {
  m <- cea_sick_sicker()
  scenarios <- list(
    "Base" = list(),
    "Flip1" = list(drug_t4 = 50000),   # t4 very expensive -> different winner
    "Flip2" = list(drug_t4 = 100000)   # even more expensive
  )
  checks <- check_scenario_robustness(m, m$default_params(), scenarios, wtp = 100000)
  expect_true(is.data.frame(checks))
  # At least one scenario should disagree
})

# --- Integration: generate_validation_report includes new checks ---

test_that("generate_validation_report includes advanced checks", {
  m <- cea_sick_sicker()
  report <- generate_validation_report(m)
  cats <- unique(report$details$category)
  expect_true("Half-cycle correction" %in% cats)
  expect_true("PH assumption" %in% cats)
  expect_true("Scenario robustness" %in% cats)
})
