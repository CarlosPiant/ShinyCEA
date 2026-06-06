test_that("plot_tornado returns ggplot", {
  tor_df <- data.frame(
    name = c("p1", "p1", "p2", "p2"),
    bound = c("Lower bound", "Upper bound", "Lower bound", "Upper bound"),
    value = c(900, 1100, 950, 1050),
    base = 1000,
    stringsAsFactors = FALSE
  )
  p <- plot_tornado(tor_df, base_val = 1000, metric_label = "INB")
  expect_s3_class(p, "ggplot")
})

test_that("plot_tornado returns NULL for empty data", {
  expect_null(plot_tornado(data.frame(), base_val = 0, metric_label = "INB"))
  expect_null(plot_tornado(NULL, base_val = 0, metric_label = "INB"))
})

test_that("plot_ce_plane returns ggplot", {
  inc_df <- data.frame(
    strategy = rep(c("A", "B"), each = 10),
    inc_qaly = rnorm(20, 0.5, 0.1),
    inc_cost = rnorm(20, 1000, 200),
    stringsAsFactors = FALSE
  )
  p <- plot_ce_plane(inc_df, wtp = 50000)
  expect_s3_class(p, "ggplot")
})

test_that("plot_ce_plane handles non-finite values", {
  inc_df <- data.frame(
    strategy = c("A", "A", "A"),
    inc_qaly = c(0.5, NA, Inf),
    inc_cost = c(1000, 2000, 3000),
    stringsAsFactors = FALSE
  )
  p <- plot_ce_plane(inc_df, wtp = 50000)
  expect_s3_class(p, "ggplot")
})

test_that("plot_ceac returns ggplot", {
  ceac_long <- data.frame(
    WTP = rep(seq(0, 100000, by = 50000), 2),
    Strategy = rep(c("A", "B"), each = 3),
    ProbCE = c(0.3, 0.4, 0.6, 0.7, 0.6, 0.4),
    stringsAsFactors = FALSE
  )
  p <- plot_ceac(ceac_long)
  expect_s3_class(p, "ggplot")
})

test_that("plot_frontier returns ggplot", {
  classified_df <- data.frame(
    Strategy = c("A", "B", "C"),
    Cost = c(100, 300, 200),
    QALY = c(1, 3, 1.5),
    Status = c("Frontier", "Frontier", "Dominated"),
    stringsAsFactors = FALSE
  )
  p <- plot_frontier(classified_df)
  expect_s3_class(p, "ggplot")
})

test_that("plot_threshold returns ggplot", {
  grid_df <- data.frame(
    param_value = seq(0, 1, by = 0.1),
    delta_cost = seq(1000, 0, length.out = 11),
    delta_qaly = seq(0, 0.5, length.out = 11),
    stringsAsFactors = FALSE
  )
  p <- plot_threshold(grid_df, threshold = 0.5, param_name = "p",
                      wtp = 50000, mode = "inb")
  expect_s3_class(p, "ggplot")
})

test_that("plot_threshold handles NA threshold", {
  grid_df <- data.frame(
    param_value = 1:5,
    delta_cost = rep(100, 5),
    delta_qaly = rep(0.1, 5),
    stringsAsFactors = FALSE
  )
  p <- plot_threshold(grid_df, threshold = NA, param_name = "p",
                      wtp = 50000, mode = "icer")
  expect_s3_class(p, "ggplot")
})

test_that("plot_survival returns ggplot", {
  surv_df <- data.frame(
    time = rep(0:10, 2),
    surv = c(seq(1, 0.5, length.out = 11), seq(1, 0.3, length.out = 11)),
    strategy = rep(c("A", "B"), each = 11),
    curve = "OS",
    stringsAsFactors = FALSE
  )
  p <- plot_survival(surv_df, curve_name = "OS")
  expect_s3_class(p, "ggplot")
})

test_that("plot_survival supports faceting", {
  surv_df <- data.frame(
    time = rep(0:5, 2),
    surv = rep(seq(1, 0.5, length.out = 6), 2),
    strategy = rep(c("A", "B"), each = 6),
    curve = "OS",
    stringsAsFactors = FALSE
  )
  p <- plot_survival(surv_df, curve_name = "OS", facet = TRUE)
  expect_s3_class(p, "ggplot")
})

test_that("plot_trace returns ggplot for all states", {
  trace_df <- data.frame(
    strategy = rep("A", 20),
    time = rep(0:9, 2),
    state = rep(c("Alive", "Dead"), each = 10),
    prob = c(seq(1, 0.5, length.out = 10), seq(0, 0.5, length.out = 10)),
    stringsAsFactors = FALSE
  )
  p <- plot_trace(trace_df)
  expect_s3_class(p, "ggplot")
})

test_that("plot_trace filters by state", {
  trace_df <- data.frame(
    strategy = rep(c("A", "B"), each = 10),
    time = rep(0:4, 4),
    state = rep(c("Alive", "Dead"), each = 5, times = 2),
    prob = runif(20),
    stringsAsFactors = FALSE
  )
  p <- plot_trace(trace_df, state_filter = "Alive")
  expect_s3_class(p, "ggplot")
})
