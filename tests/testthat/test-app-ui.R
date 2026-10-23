# Tests for the app UI: it builds, loads the shared ShinyModels design
# (shinycea.css, Barlow fonts, cream bslib theme) and no longer uses the
# former Bootswatch "cosmo" theme.

test_that("cea_app_ui builds and links the shared stylesheet", {
  # tags$head() content is rendered into the document head, so look there.
  r <- htmltools::renderTags(cea_app_ui())
  expect_true(grepl("shinycea/shinycea.css", r$head, fixed = TRUE))
  expect_true(grepl("fonts.googleapis.com", r$head, fixed = TRUE))
  expect_true(grepl('class="cea-topbar"', r$html, fixed = TRUE))
  expect_true(grepl('cea-sidebar', r$html, fixed = TRUE))
  expect_false(grepl("cosmo", paste(r$head, r$html), fixed = TRUE))
  # The stylesheet is shipped with the package and carries the design tokens.
  css <- system.file("www", "shinycea.css", package = "ShinyCEA")
  expect_true(nzchar(css))
  expect_true(any(grepl("--navy: #18213d", readLines(css), fixed = TRUE)))
})

test_that("cea_app_ui shows the model selector only with several models", {
  one <- as.character(cea_app_ui(c("A" = "a")))
  two <- as.character(cea_app_ui(c("A" = "a", "B" = "b")))
  expect_false(grepl('id="model_key"', one, fixed = TRUE))
  expect_true(grepl('id="model_key"', two, fixed = TRUE))
})

test_that("the cream preset is the default in the Settings tab", {
  html <- as.character(cea_app_ui())
  expect_true(grepl('value="cream" checked', html, fixed = TRUE))
})
