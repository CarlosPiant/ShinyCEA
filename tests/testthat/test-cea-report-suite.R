# --- Table formatters ---

test_that("table_base_case returns formatted table", {
  m <- cea_sick_sicker()
  det <- run_deterministic(m, m$default_params(), wtp = 100000)
  tab <- table_base_case(det)
  expect_true(is.data.frame(tab))
  expect_true(all(c("Strategy", "Cost", "QALY", "NMB") %in% names(tab)))
  expect_true(all(grepl("\\$", tab$Cost)))
})

test_that("table_incremental returns formatted table", {
  m <- cea_sick_sicker()
  det <- run_deterministic(m, m$default_params(), wtp = 100000)
  tab <- table_incremental(det)
  expect_true(is.data.frame(tab))
  expect_true(all(c("Strategy", "Reference", "Inc_Cost", "ICER") %in% names(tab)))
})

# --- Collectors ---

test_that("collect_cea_results returns structured output", {
  m <- cea_sick_sicker()
  cea <- collect_cea_results(m, m$default_params(), wtp = 100000)
  expect_true(is.list(cea))
  expect_true(all(c("det", "plots", "tables") %in% names(cea)))
  expect_s3_class(cea$plots$frontier, "ggplot")
  expect_true(is.data.frame(cea$tables$base_case))
})

test_that("collect_psa_results returns structured output", {
  m <- cea_sick_sicker()
  det <- run_deterministic(m, m$default_params(), wtp = 100000)
  set.seed(42)
  psa <- collect_psa_results(m, m$default_params(), det, wtp = 100000, n_psa = 50)
  expect_true(is.list(psa))
  expect_s3_class(psa$plots$ce_plane, "ggplot")
  expect_s3_class(psa$plots$ceac, "ggplot")
  expect_true(is.data.frame(psa$tables$psa_summary))
})

test_that("collect_bia_results returns structured output", {
  m <- cea_sick_sicker()
  strats <- m$info()$strategies
  n <- length(strats)
  cur <- stats::setNames(rep(1/n, n), strats)
  new_mix <- cur; new_mix[1] <- 0; new_mix[n] <- new_mix[n] + cur[1]

  bia <- collect_bia_results(m, m$default_params(),
                             population = 10000,
                             current_mix = cur, new_mix = new_mix, years = 3)
  expect_true(is.list(bia))
  expect_s3_class(bia$plots$budget_impact, "ggplot")
})

# --- Quarto section generators ---

test_that("create_cea_report generates markdown lines", {
  m <- cea_sick_sicker()
  cea <- collect_cea_results(m, m$default_params(), wtp = 100000)
  tmpdir <- tempfile("report_")
  lines <- create_cea_report(cea, output_dir = tmpdir)
  expect_true(length(lines) > 10)
  expect_true(any(grepl("Base-Case", lines)))
  unlink(tmpdir, recursive = TRUE)
})

test_that("create_psa_appendix generates markdown lines", {
  m <- cea_sick_sicker()
  det <- run_deterministic(m, m$default_params(), wtp = 100000)
  set.seed(42)
  psa <- collect_psa_results(m, m$default_params(), det, wtp = 100000, n_psa = 30)
  tmpdir <- tempfile("report_")
  lines <- create_psa_appendix(psa, output_dir = tmpdir)
  expect_true(length(lines) > 5)
  expect_true(any(grepl("PSA", lines)))
  unlink(tmpdir, recursive = TRUE)
})

# --- Master orchestrator ---

test_that("render_hta_report generates .qmd file", {
  m <- cea_sick_sicker()
  tmpdir <- tempfile("report_")

  qmd_path <- render_hta_report(
    m, wtp = 100000, n_psa = 20,
    title = "Test Report", author = "Test",
    output_dir = tmpdir, render = FALSE
  )

  expect_true(file.exists(qmd_path))
  content <- readLines(qmd_path)
  expect_true(any(grepl("title:", content)))
  expect_true(any(grepl("Base-Case", content)))
  expect_true(any(grepl("PSA", content)))
  expect_true(any(grepl("Validation", content)))
  unlink(tmpdir, recursive = TRUE)
})

test_that("render_hta_report includes scenarios when provided", {
  m <- cea_sick_sicker()
  tmpdir <- tempfile("report_")

  qmd_path <- render_hta_report(
    m, wtp = 100000, n_psa = 0,
    scenarios = list("Base" = list(), "No disc" = list(d_cost = 0, d_qaly = 0)),
    output_dir = tmpdir, render = FALSE
  )

  content <- readLines(qmd_path)
  expect_true(any(grepl("Scenario", content)))
  unlink(tmpdir, recursive = TRUE)
})

test_that("render_hta_report supports revealjs format", {
  m <- cea_sick_sicker()
  tmpdir <- tempfile("report_")

  qmd_path <- render_hta_report(
    m, wtp = 100000, n_psa = 0,
    format = "revealjs",
    output_dir = tmpdir, render = FALSE
  )

  content <- readLines(qmd_path)
  expect_true(any(grepl("revealjs", content)))
  unlink(tmpdir, recursive = TRUE)
})
