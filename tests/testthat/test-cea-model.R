test_that("new_cea_model creates valid model from functions", {
  m <- new_cea_model(
    info = function() list(key = "test", name = "Test", strategies = c("A", "B")),
    params = function() cea_param("p", 0.5, 0.1, 0.9, distribution = "beta"),
    simulate = function(pars, strategy) list(cost = 100, qaly = 1)
  )
  expect_s3_class(m, "cea_model")
  expect_equal(m$info()$key, "test")
  expect_equal(m$info()$strategies, c("A", "B"))
})

test_that("validate_cea_model catches missing info fields", {
  bad <- structure(
    list(
      info = function() list(key = "x"),
      default_params = function() cea_param("p", 1, 0, 2),
      simulate = function(pars, strategy) list(cost = 0, qaly = 0)
    ),
    class = "cea_model"
  )
  expect_error(validate_cea_model(bad), "missing.*strategies")
})

test_that("validate_cea_model catches < 2 strategies", {
  expect_error(
    new_cea_model(
      info = function() list(key = "x", name = "X", strategies = "only_one"),
      params = function() cea_param("p", 1, 0, 2),
      simulate = function(pars, strategy) list(cost = 0, qaly = 0)
    ),
    ">= 2 strategies"
  )
})

test_that("validate_cea_model catches missing param columns", {
  expect_error(
    new_cea_model(
      info = function() list(key = "x", name = "X", strategies = c("A", "B")),
      params = function() data.frame(name = "p", value = 1),
      simulate = function(pars, strategy) list(cost = 0, qaly = 0)
    ),
    "missing columns"
  )
})

test_that("validate_cea_model catches non-finite simulate output", {
  expect_error(
    new_cea_model(
      info = function() list(key = "x", name = "X", strategies = c("A", "B")),
      params = function() cea_param("p", 1, 0, 2),
      simulate = function(pars, strategy) list(cost = NaN, qaly = 1)
    ),
    "non-finite"
  )
})

test_that("validate_cea_model rejects non-cea_model objects", {
  expect_error(validate_cea_model(list()), "must be a 'cea_model'")
})

test_that("cea_param creates correct row", {
  row <- cea_param("myp", 0.5, 0.1, 0.9, distribution = "beta",
                   include_psa = FALSE, include_tornado = TRUE)
  expect_equal(nrow(row), 1L)
  expect_equal(row$name, "myp")
  expect_equal(row$mean, 0.5)
  expect_equal(row$base, 0.5)
  expect_equal(row$distribution, "beta")
  expect_false(row$include_psa)
  expect_true(row$include_tornado)
})

test_that("print.cea_model produces output", {
  m <- cea_sick_sicker()
  expect_output(print(m), "Sick-Sicker")
  expect_output(print(m), "Strategies:")
})

test_that("diagram and description fields are stored", {
  m <- new_cea_model(
    info = function() list(key = "t", name = "T", strategies = c("A", "B")),
    params = function() cea_param("p", 1, 0, 2),
    simulate = function(pars, strategy) list(cost = 10, qaly = 1),
    diagram = "path/to/img.png",
    description = "A test model."
  )
  expect_equal(m$diagram, "path/to/img.png")
  expect_equal(m$description, "A test model.")
})
