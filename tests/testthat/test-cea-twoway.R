test_that("twoway_dsa returns correct structure", {
  m <- cea_sick_sicker()
  pars <- stats::setNames(as.list(m$default_params()$base), m$default_params()$name)

  res <- twoway_dsa(m, pars,
                    param1 = "p_HS_std", param2 = "drug_t4",
                    range1 = c(0.02, 0.08), range2 = c(200, 1200),
                    n_grid = 5, ref = "std", target = "t4",
                    wtp = 100000, metric = "INB")

  expect_true(is.list(res))
  expect_true(all(c("grid", "param1", "param2", "ref", "target", "metric") %in% names(res)))
  expect_equal(nrow(res$grid), 25L)  # 5x5
  expect_equal(names(res$grid), c("p_HS_std", "drug_t4", "value"))
})

test_that("twoway_dsa works with ICER metric", {
  m <- cea_sick_sicker()
  pars <- stats::setNames(as.list(m$default_params()$base), m$default_params()$name)

  res <- twoway_dsa(m, pars,
                    param1 = "u_H", param2 = "u_S1",
                    range1 = c(0.80, 0.95), range2 = c(0.60, 0.85),
                    n_grid = 4, ref = "std", target = "t1",
                    wtp = 100000, metric = "ICER")

  expect_equal(nrow(res$grid), 16L)
  expect_equal(res$metric, "ICER")
})

test_that("twoway_dsa handles simulation failures gracefully", {
  m <- cea_sick_sicker()
  pars <- stats::setNames(as.list(m$default_params()$base), m$default_params()$name)

  # Extreme ranges that might cause issues
  res <- twoway_dsa(m, pars,
                    param1 = "p_HS_std", param2 = "p_S2D_std",
                    range1 = c(0, 0.5), range2 = c(0, 0.5),
                    n_grid = 4, ref = "std", target = "t2",
                    wtp = 100000, metric = "INB")

  expect_equal(nrow(res$grid), 16L)
  # Some values may be NA but shouldn't error
})

test_that("plot_twoway_dsa returns ggplot for INB", {
  m <- cea_sick_sicker()
  pars <- stats::setNames(as.list(m$default_params()$base), m$default_params()$name)

  res <- twoway_dsa(m, pars,
                    param1 = "p_HS_std", param2 = "drug_t4",
                    range1 = c(0.02, 0.08), range2 = c(200, 1200),
                    n_grid = 5, ref = "std", target = "t4",
                    wtp = 100000, metric = "INB")

  p <- plot_twoway_dsa(res)
  expect_s3_class(p, "ggplot")
})

test_that("plot_twoway_dsa returns ggplot for ICER", {
  m <- cea_sick_sicker()
  pars <- stats::setNames(as.list(m$default_params()$base), m$default_params()$name)

  res <- twoway_dsa(m, pars,
                    param1 = "c_S1", param2 = "c_S2",
                    range1 = c(500, 1200), range2 = c(1500, 3500),
                    n_grid = 4, ref = "std", target = "t1",
                    wtp = 100000, metric = "ICER")

  p <- plot_twoway_dsa(res)
  expect_s3_class(p, "ggplot")
})

test_that("plot_twoway_dsa accepts palette", {
  m <- cea_sick_sicker()
  pars <- stats::setNames(as.list(m$default_params()$base), m$default_params()$name)

  res <- twoway_dsa(m, pars,
                    param1 = "u_H", param2 = "u_S1",
                    range1 = c(0.80, 0.95), range2 = c(0.60, 0.85),
                    n_grid = 4, ref = "std", target = "t4",
                    wtp = 100000, metric = "INB")

  p <- plot_twoway_dsa(res, palette = cea_palette("lancet"))
  expect_s3_class(p, "ggplot")
})

test_that("plot_twoway_dsa can hide contour", {
  m <- cea_sick_sicker()
  pars <- stats::setNames(as.list(m$default_params()$base), m$default_params()$name)

  res <- twoway_dsa(m, pars,
                    param1 = "p_HS_std", param2 = "drug_t4",
                    range1 = c(0.02, 0.08), range2 = c(200, 1200),
                    n_grid = 4, ref = "std", target = "t4",
                    wtp = 100000, metric = "INB")

  p <- plot_twoway_dsa(res, show_contour = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("twoway_dsa works with partitioned survival model", {
  m <- cea_partitioned_survival()
  pars <- stats::setNames(as.list(m$default_params()$base), m$default_params()$name)

  res <- twoway_dsa(m, pars,
                    param1 = "hr_pfs_t1", param2 = "hr_os_t1",
                    range1 = c(0.5, 1.0), range2 = c(0.7, 1.1),
                    n_grid = 4, ref = "std", target = "t1",
                    wtp = 100000, metric = "INB")

  expect_equal(nrow(res$grid), 16L)
  p <- plot_twoway_dsa(res)
  expect_s3_class(p, "ggplot")
})
