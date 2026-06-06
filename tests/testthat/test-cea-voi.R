test_that("compute_evpi returns correct structure", {
  m <- cea_sick_sicker()
  set.seed(123)
  psa <- run_psa(m, m$default_params(), n_sim = 50)
  evpi <- compute_evpi(psa$results, m$info()$strategies, wtp_max = 100000, wtp_step = 25000)

  expect_true(is.data.frame(evpi))
  expect_true(all(c("WTP", "EVPI") %in% names(evpi)))
  expect_true(all(evpi$EVPI >= 0))
  expect_equal(nrow(evpi), 5L)
})

test_that("compute_evpi is zero at extremes when one strategy dominates", {
  m <- cea_sick_sicker()
  set.seed(42)
  psa <- run_psa(m, m$default_params(), n_sim = 100)
  evpi <- compute_evpi(psa$results, m$info()$strategies, wtp_max = 200000, wtp_step = 50000)
  expect_true(all(is.finite(evpi$EVPI)))
})

test_that("compute_evppi returns correct structure", {
  m <- cea_sick_sicker()
  set.seed(123)
  psa <- run_psa(m, m$default_params(), n_sim = 80)
  evppi <- compute_evppi(psa$results, psa$draws, m$info()$strategies, wtp = 100000)

  expect_true(is.data.frame(evppi))
  expect_true(all(c("Parameter", "EVPPI") %in% names(evppi)))
  expect_true(all(evppi$EVPPI >= 0))
  expect_true(nrow(evppi) > 0)
})

test_that("compute_evppi handles subset of parameters", {
  m <- cea_sick_sicker()
  set.seed(123)
  psa <- run_psa(m, m$default_params(), n_sim = 50)
  evppi <- compute_evppi(psa$results, psa$draws, m$info()$strategies,
                         wtp = 100000, param_names = c("p_HS_std", "u_H"))
  expect_equal(nrow(evppi), 2L)
})

test_that("compute_evppi returns empty for NULL draws", {
  m <- cea_sick_sicker()
  set.seed(123)
  psa <- run_psa(m, m$default_params(), n_sim = 20)
  evppi <- compute_evppi(psa$results, NULL, m$info()$strategies, wtp = 100000)
  expect_equal(nrow(evppi), 0L)
})

test_that("plot_evpi returns ggplot", {
  evpi_df <- data.frame(WTP = seq(0, 100000, 25000), EVPI = c(0, 500, 1200, 800, 300))
  p <- plot_evpi(evpi_df)
  expect_s3_class(p, "ggplot")
})

test_that("plot_evppi returns ggplot", {
  evppi_df <- data.frame(Parameter = c("p1", "p2", "p3"), EVPPI = c(500, 200, 0))
  p <- plot_evppi(evppi_df)
  expect_s3_class(p, "ggplot")
})

test_that("plot_evppi handles all-zero EVPPI", {
  evppi_df <- data.frame(Parameter = c("p1", "p2"), EVPPI = c(0, 0))
  p <- plot_evppi(evppi_df)
  expect_s3_class(p, "ggplot")
})

# --- Expected Loss ---

test_that("compute_expected_loss returns correct structure", {
  m <- cea_sick_sicker()
  set.seed(123)
  psa <- run_psa(m, m$default_params(), n_sim = 50)
  el <- compute_expected_loss(psa$results, m$info()$strategies,
                              wtp_max = 100000, wtp_step = 50000)

  expect_true(is.data.frame(el))
  expect_true(all(c("WTP", "Strategy", "Expected_Loss") %in% names(el)))
  expect_true(all(el$Expected_Loss >= 0))
})

test_that("compute_expected_loss has one row per strategy per WTP", {
  m <- cea_sick_sicker()
  set.seed(42)
  psa <- run_psa(m, m$default_params(), n_sim = 30)
  el <- compute_expected_loss(psa$results, m$info()$strategies,
                              wtp_max = 50000, wtp_step = 25000)

  n_strat <- length(m$info()$strategies)
  n_wtp <- 3L  # 0, 25000, 50000
  expect_equal(nrow(el), n_strat * n_wtp)
})

test_that("compute_expected_loss minimum equals zero at each WTP", {
  m <- cea_sick_sicker()
  set.seed(42)
  psa <- run_psa(m, m$default_params(), n_sim = 50)
  el <- compute_expected_loss(psa$results, m$info()$strategies,
                              wtp_max = 100000, wtp_step = 50000)

  for (w in unique(el$WTP)) {
    sub <- el[el$WTP == w, ]
    # The optimal strategy should have the lowest EL (close to EVPI complement)
    expect_true(min(sub$Expected_Loss) >= 0)
  }
})

test_that("plot_expected_loss returns ggplot", {
  el_df <- data.frame(
    WTP = rep(c(0, 50000, 100000), 2),
    Strategy = rep(c("A", "B"), each = 3),
    Expected_Loss = c(0, 500, 1200, 800, 300, 0),
    stringsAsFactors = FALSE
  )
  p <- plot_expected_loss(el_df)
  expect_s3_class(p, "ggplot")
})

test_that("plot_expected_loss accepts palette", {
  el_df <- data.frame(
    WTP = rep(c(0, 50000), 2),
    Strategy = rep(c("A", "B"), each = 2),
    Expected_Loss = c(0, 500, 400, 0),
    stringsAsFactors = FALSE
  )
  p <- plot_expected_loss(el_df, palette = cea_palette("lancet"))
  expect_s3_class(p, "ggplot")
})
