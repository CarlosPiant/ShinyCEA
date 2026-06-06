test_that("build_psa_summary returns correct structure", {
  m <- cea_sick_sicker()
  set.seed(123)
  psa <- run_psa(m, m$default_params(), n_sim = 30)
  summ <- build_psa_summary(psa$results, m$info()$strategies, wtp = 100000)

  expect_true(is.data.frame(summ))
  expect_equal(nrow(summ), length(m$info()$strategies))
  expect_true(all(c("Strategy", "Cost", "QALY", "NMB") %in% names(summ)))
  expect_true(all(m$info()$strategies %in% summ$Strategy))
})

test_that("build_psa_summary formats with CIs", {
  m <- cea_sick_sicker()
  set.seed(123)
  psa <- run_psa(m, m$default_params(), n_sim = 50)
  summ <- build_psa_summary(psa$results, m$info()$strategies)

  expect_true(all(grepl("\\(", summ$Cost)))
  expect_true(all(grepl("\\(", summ$QALY)))
  expect_true(all(grepl("\\(", summ$NMB)))
})

test_that("generate_cea_report creates Word file", {
  skip_if_not_installed("rmarkdown")

  m <- cea_sick_sicker()
  det <- run_deterministic(m, m$default_params(), wtp = 100000)

  outfile <- tempfile(fileext = ".docx")
  on.exit(unlink(outfile), add = TRUE)

  result <- generate_cea_report(
    output_file = outfile,
    format = "word",
    model = m,
    det_result = det,
    wtp = 100000
  )

  expect_true(file.exists(outfile))
  expect_true(file.size(outfile) > 0)
})

test_that("generate_cea_report includes plots when provided", {
  skip_if_not_installed("rmarkdown")

  m <- cea_sick_sicker()
  det <- run_deterministic(m, m$default_params(), wtp = 100000)

  frontier_plot <- plot_frontier(det$frontier_status)

  outfile <- tempfile(fileext = ".docx")
  on.exit(unlink(outfile), add = TRUE)

  result <- generate_cea_report(
    output_file = outfile,
    format = "word",
    model = m,
    det_result = det,
    wtp = 100000,
    plots = list(frontier = frontier_plot)
  )

  expect_true(file.exists(outfile))
  expect_true(file.size(outfile) > 1000)
})

test_that("generate_cea_report includes PSA summary", {
  skip_if_not_installed("rmarkdown")

  m <- cea_sick_sicker()
  det <- run_deterministic(m, m$default_params(), wtp = 100000)
  set.seed(42)
  psa <- run_psa(m, m$default_params(), n_sim = 30)
  psa_summ <- build_psa_summary(psa$results, m$info()$strategies, wtp = 100000)

  outfile <- tempfile(fileext = ".docx")
  on.exit(unlink(outfile), add = TRUE)

  generate_cea_report(
    output_file = outfile,
    format = "word",
    model = m,
    det_result = det,
    psa_result = psa,
    wtp = 100000,
    psa_summary_df = psa_summ
  )

  expect_true(file.exists(outfile))
  expect_true(file.size(outfile) > 1000)
})

test_that("generate_cea_report works with det-only (no PSA)", {
  skip_if_not_installed("rmarkdown")

  m <- cea_pf_prog_death()
  det <- run_deterministic(m, m$default_params(), wtp = 50000)

  outfile <- tempfile(fileext = ".docx")
  on.exit(unlink(outfile), add = TRUE)

  generate_cea_report(
    output_file = outfile,
    format = "word",
    model = m,
    det_result = det,
    wtp = 50000
  )

  expect_true(file.exists(outfile))
})

test_that("generate_cea_report errors without rmarkdown", {
  skip("Cannot unload rmarkdown in test environment")
})
