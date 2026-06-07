# --- check_transition_matrix ---

test_that("check_transition_matrix passes valid trace", {
  m <- cea_sick_sicker()
  pars <- stats::setNames(as.list(m$default_params()$base), m$default_params()$name)
  res <- m$simulate(pars, "std")
  checks <- check_transition_matrix(res$trace)
  expect_true(all(checks$status %in% c("PASS", "WARN")))
  expect_equal(sum(checks$status == "FAIL"), 0L)
})

test_that("check_transition_matrix handles NULL trace", {
  checks <- check_transition_matrix(NULL)
  expect_true(any(checks$status == "WARN"))
})

test_that("check_transition_matrix catches negative entries", {
  bad_trace <- matrix(c(1, 0, 0, -0.1, 0.5, 0.6), nrow = 2, byrow = TRUE)
  colnames(bad_trace) <- c("A", "B", "C")
  checks <- check_transition_matrix(bad_trace)
  expect_true(any(checks$status == "FAIL"))
})

# --- check_probabilities_sum ---

test_that("check_probabilities_sum passes valid params", {
  m <- cea_sick_sicker()
  checks <- check_probabilities_sum(m$default_params())
  expect_equal(sum(checks$status == "FAIL"), 0L)
})

test_that("check_probabilities_sum catches out-of-range", {
  params <- rbind(
    cea_param("p_bad", 1.5, 1.0, 2.0, distribution = "beta"),
    cea_param("p_good", 0.3, 0.1, 0.5, distribution = "beta")
  )
  checks <- check_probabilities_sum(params)
  expect_true(any(checks$status == "FAIL"))
})

test_that("check_probabilities_sum checks custom groups", {
  params <- rbind(
    cea_param("p_A_B", 0.6, 0.4, 0.8, distribution = "beta"),
    cea_param("p_A_C", 0.5, 0.3, 0.7, distribution = "beta")
  )
  checks <- check_probabilities_sum(params,
    prob_groups = list("from_A" = c("p_A_B", "p_A_C")))
  expect_true(any(checks$status == "FAIL"))  # sum = 1.1 > 1
})

# --- check_discounting ---

test_that("check_discounting passes for bundled model", {
  m <- cea_sick_sicker()
  checks <- check_discounting(m, m$default_params())
  expect_equal(sum(checks$status == "FAIL"), 0L)
})

# --- check_psa_inputs ---

test_that("check_psa_inputs passes valid params", {
  m <- cea_sick_sicker()
  checks <- check_psa_inputs(m$default_params())
  expect_equal(sum(checks$status == "FAIL"), 0L)
})

test_that("check_psa_inputs catches bad beta domain", {
  params <- cea_param("p_bad", 1.5, 1.0, 2.0, distribution = "beta")
  checks <- check_psa_inputs(params)
  expect_true(any(checks$status == "FAIL"))
})

# --- check_dominance_logic ---

test_that("check_dominance_logic passes for bundled model", {
  m <- cea_sick_sicker()
  det <- run_deterministic(m, m$default_params(), wtp = 100000)
  checks <- check_dominance_logic(det)
  expect_equal(sum(checks$status == "FAIL"), 0L)
})

# --- validate_model_outputs ---

test_that("validate_model_outputs passes for sick-sicker", {
  m <- cea_sick_sicker()
  checks <- validate_model_outputs(m, m$default_params())
  expect_equal(sum(checks$status == "FAIL"), 0L)
})

test_that("validate_model_outputs passes for partitioned survival", {
  m <- cea_partitioned_survival()
  checks <- validate_model_outputs(m, m$default_params())
  expect_equal(sum(checks$status == "FAIL"), 0L)
})

# --- generate_validation_report ---

test_that("generate_validation_report returns structured result", {
  m <- cea_sick_sicker()
  report <- generate_validation_report(m)
  expect_true(is.list(report))
  expect_true(all(c("summary", "details", "score", "model_name") %in% names(report)))
  expect_true(report$score >= 0 && report$score <= 100)
  expect_true(is.data.frame(report$details))
})

test_that("generate_validation_report exports markdown", {
  m <- cea_sick_sicker()
  tmp <- tempfile(fileext = ".md")
  on.exit(unlink(tmp))
  report <- generate_validation_report(m, output_file = tmp)
  expect_true(file.exists(tmp))
  expect_true(file.size(tmp) > 100)
  content <- readLines(tmp)
  expect_true(any(grepl("Validation Report", content)))
})

test_that("generate_validation_report scores high for clean model", {
  m <- cea_sick_sicker()
  report <- generate_validation_report(m)
  expect_true(report$score >= 80)
})
