test_that("run_subgroups returns correct structure", {
  m <- cea_sick_sicker()
  subgroups <- list(
    "Overall" = list(),
    "High risk" = list(p_HS_std = 0.08)
  )
  res <- run_subgroups(m, m$default_params(), subgroups,
                       target = "t1", ref = "std", wtp = 100000)

  expect_true(is.data.frame(res))
  expect_true(all(c("Subgroup", "Target", "Reference", "IncCost",
                     "IncQALY", "ICER", "INB") %in% names(res)))
  expect_equal(nrow(res), 2L)
})

test_that("run_subgroups applies overrides correctly", {
  m <- cea_sick_sicker()
  subgroups <- list(
    "Low prog" = list(p_HS_std = 0.01),
    "High prog" = list(p_HS_std = 0.10)
  )
  res <- run_subgroups(m, m$default_params(), subgroups,
                       target = "t1", ref = "std", wtp = 100000)

  # Higher progression -> larger QALY gain from treatment -> different ICER
  expect_true(res$IncQALY[res$Subgroup == "High prog"] !=
                res$IncQALY[res$Subgroup == "Low prog"])
})

test_that("run_subgroups errors on bad target", {
  m <- cea_sick_sicker()
  expect_error(
    run_subgroups(m, m$default_params(), list("X" = list()),
                  target = "nonexistent", ref = "std"),
    "not in model strategies"
  )
})

test_that("run_subgroups errors on non-named list", {
  m <- cea_sick_sicker()
  expect_error(
    run_subgroups(m, m$default_params(), list(list()),
                  target = "t1", ref = "std"),
    "named list"
  )
})

test_that("run_subgroups works with multiple overrides", {
  m <- cea_sick_sicker()
  subgroups <- list(
    "Elderly" = list(u_H = 0.75, u_S1 = 0.50, p_HD_std = 0.015)
  )
  res <- run_subgroups(m, m$default_params(), subgroups,
                       target = "t4", ref = "std", wtp = 100000)
  expect_equal(nrow(res), 1L)
  expect_true(is.finite(res$ICER))
})

test_that("run_subgroups works with partitioned survival model", {
  m <- cea_partitioned_survival()
  subgroups <- list(
    "Overall" = list(),
    "Aggressive" = list(os_scale_std = 2.0)
  )
  res <- run_subgroups(m, m$default_params(), subgroups,
                       target = "t1", ref = "std", wtp = 100000)
  expect_equal(nrow(res), 2L)
})

test_that("plot_subgroup_forest returns ggplot for ICER", {
  sg_df <- data.frame(
    Subgroup = c("Overall", "High", "Low"),
    Target = "t1", Reference = "std",
    Cost_Ref = 100, Cost_Tgt = 200,
    QALY_Ref = 1, QALY_Tgt = 1.5,
    IncCost = 100, IncQALY = 0.5,
    ICER = c(200, 300, 100),
    INB = c(400, 200, 600),
    stringsAsFactors = FALSE
  )
  p <- plot_subgroup_forest(sg_df, metric = "ICER", wtp = 250)
  expect_s3_class(p, "ggplot")
})

test_that("plot_subgroup_forest returns ggplot for INB", {
  sg_df <- data.frame(
    Subgroup = c("A", "B"),
    Target = "t1", Reference = "std",
    Cost_Ref = 100, Cost_Tgt = 200,
    QALY_Ref = 1, QALY_Tgt = 1.5,
    IncCost = 100, IncQALY = 0.5,
    ICER = c(200, 300),
    INB = c(400, -100),
    stringsAsFactors = FALSE
  )
  p <- plot_subgroup_forest(sg_df, metric = "INB", wtp = 500)
  expect_s3_class(p, "ggplot")
})

test_that("plot_subgroup_forest handles NA values", {
  sg_df <- data.frame(
    Subgroup = c("A", "B"),
    Target = "t1", Reference = "std",
    Cost_Ref = c(100, NA), Cost_Tgt = c(200, NA),
    QALY_Ref = c(1, NA), QALY_Tgt = c(1.5, NA),
    IncCost = c(100, NA), IncQALY = c(0.5, NA),
    ICER = c(200, NA), INB = c(400, NA),
    stringsAsFactors = FALSE
  )
  p <- plot_subgroup_forest(sg_df, metric = "ICER", wtp = 250)
  expect_s3_class(p, "ggplot")
})

test_that("plot_subgroup_forest colors by CE status", {
  sg_df <- data.frame(
    Subgroup = c("Cheap", "Expensive"),
    Target = "t1", Reference = "std",
    Cost_Ref = 100, Cost_Tgt = 200,
    QALY_Ref = 1, QALY_Tgt = 1.5,
    IncCost = 100, IncQALY = 0.5,
    ICER = c(50, 500),
    INB = c(1000, -200),
    stringsAsFactors = FALSE
  )
  p <- plot_subgroup_forest(sg_df, metric = "ICER", wtp = 200)
  expect_s3_class(p, "ggplot")
})

test_that("end-to-end subgroup pipeline", {
  m <- cea_sick_sicker()
  subgroups <- list(
    "Overall" = list(),
    "High risk" = list(p_HS_std = 0.08, p_S1S2_std = 0.10),
    "Low risk" = list(p_HS_std = 0.02, p_S1S2_std = 0.03)
  )
  res <- run_subgroups(m, m$default_params(), subgroups,
                       target = "t4", ref = "std", wtp = 100000)

  expect_equal(nrow(res), 3L)
  expect_true(all(is.finite(res$ICER)))

  p <- plot_subgroup_forest(res, metric = "ICER", wtp = 100000,
                            palette = cea_palette("lancet"))
  expect_s3_class(p, "ggplot")
})
