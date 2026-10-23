test_that("cea_palette returns all four presets", {
  for (nm in c("cream", "uw", "classic", "lancet")) {
    pal <- cea_palette(nm)
    expect_true(is.list(pal))
    expect_true(all(c("primary", "secondary", "accent", "frontier",
                       "dominated", "ext_dominated", "strategies") %in% names(pal)))
    expect_true(length(pal$strategies) >= 2)
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", pal$strategies)))
  }
})

test_that("cea_palette rejects invalid name", {
  expect_error(cea_palette("invalid"))
})

test_that("cea_palette_custom builds valid palette", {
  pal <- cea_palette_custom(c("#111111", "#222222", "#333333"))
  expect_equal(pal$primary, "#111111")
  expect_equal(pal$secondary, "#222222")
  expect_equal(length(pal$strategies), 3)
})

test_that("cea_palette_custom uses custom frontier colors", {
  pal <- cea_palette_custom(c("#AA0000", "#00AA00"),
                            frontier = "#0000FF",
                            dominated = "#FF0000")
  expect_equal(pal$frontier, "#0000FF")
  expect_equal(pal$dominated, "#FF0000")
})

test_that("cea_palette_custom errors with < 2 colors", {
  expect_error(cea_palette_custom("#111111"), "at least 2")
})

test_that("scale_color_cea returns a ggplot scale", {
  pal <- cea_palette("classic")
  sc <- scale_color_cea(pal)
  expect_true(inherits(sc, "Scale"))
})

test_that("scale_fill_cea returns a ggplot scale", {
  pal <- cea_palette("lancet")
  sc <- scale_fill_cea(pal)
  expect_true(inherits(sc, "Scale"))
})

test_that("plot functions accept palette parameter", {
  pal <- cea_palette("lancet")

  tor_df <- data.frame(
    name = c("p1", "p1"), bound = c("Lower bound", "Upper bound"),
    value = c(900, 1100), base = 1000, stringsAsFactors = FALSE
  )
  expect_s3_class(plot_tornado(tor_df, 1000, "INB", palette = pal), "ggplot")

  inc_df <- data.frame(
    strategy = "A", inc_qaly = 0.5, inc_cost = 1000, stringsAsFactors = FALSE
  )
  expect_s3_class(plot_ce_plane(inc_df, 50000, palette = pal), "ggplot")

  ceac_df <- data.frame(
    WTP = c(0, 50000), Strategy = c("A", "A"), ProbCE = c(0.3, 0.7),
    stringsAsFactors = FALSE
  )
  expect_s3_class(plot_ceac(ceac_df, palette = pal), "ggplot")

  cl_df <- data.frame(
    Strategy = c("A", "B"), Cost = c(100, 200), QALY = c(1, 2),
    Status = c("Frontier", "Frontier"), stringsAsFactors = FALSE
  )
  expect_s3_class(plot_frontier(cl_df, palette = pal), "ggplot")
})

test_that("all three palettes produce distinct strategy colors", {
  pals <- lapply(c("uw", "classic", "lancet"), cea_palette)
  s1 <- pals[[1]]$strategies[1]
  s2 <- pals[[2]]$strategies[1]
  s3 <- pals[[3]]$strategies[1]
  expect_false(s1 == s2)
  expect_false(s2 == s3)
})

test_that("cream is the default palette and matches the design tokens", {
  pal <- cea_palette()
  expect_equal(pal$primary, "#18213d")      # navy
  expect_equal(pal$secondary, "#1aa3a1")    # teal
  expect_equal(pal$dominated, "#c62828")    # error red
  expect_equal(length(pal$strategies), 8)
})

test_that("theme_cea returns a ggplot2 theme with a transparent background", {
  th <- theme_cea()
  expect_s3_class(th, "theme")
  expect_equal(th$plot.background$fill, "transparent")
  expect_equal(th$legend.position, "bottom")
  expect_equal(theme_cea(legend = "none")$legend.position, "none")
  # The theme renders without error.
  p <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) +
    ggplot2::geom_point() + th
  expect_silent(ggplot2::ggplot_build(p))
})
