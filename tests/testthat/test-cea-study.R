# Tests for the study details (cea_study), the CHEERS 2022 checklist, the
# parameter documentation columns and the model diagram, and for how the app,
# the reports and the code export use them.

# Smallest valid model, to test the optional arguments of new_cea_model().
tiny_model <- function(...) {
  new_cea_model(
    info = function() list(key = "tiny", name = "Tiny", strategies = c("a", "b")),
    params = function() rbind(cea_param("c_b", 100, 50, 150, distribution = "gamma"),
                              cea_param("q_b", 0.1, 0.05, 0.2, distribution = "beta")),
    simulate = function(pars, strategy) {
      if (strategy == "b") list(cost = pars[["c_b"]], qaly = 1 + pars[["q_b"]])
      else list(cost = 0, qaly = 1)
    },
    ...
  )
}

test_that("cea_study keeps filled fields, trims text and validates input", {
  s <- cea_study(population = "  Adults  ", setting = "", perspective = NA,
                 assumptions = c("A1", "", "A2"), comparators = c(a = "Old", b = "New"))
  expect_s3_class(s, "cea_study")
  expect_named(s, c("population", "assumptions", "comparators"))
  expect_identical(s$population, "Adults")
  expect_identical(s$assumptions, c("A1", "A2"))
  expect_identical(s$comparators, c(a = "Old", b = "New"))
  expect_length(cea_study(), 0)
  expect_error(cea_study(population = c("x", "y")), "single string")
  expect_error(cea_study(comparators = c("Old", "New")), "named by strategy")
  expect_error(tiny_model(study = list(population = "x")), "cea_study")
  expect_output(print(s), "3 field\\(s\\) filled")
})

test_that("every study field has a label and the form fields match the constructor", {
  f <- ShinyCEA:::.study_fields
  expect_false(anyDuplicated(f$field) > 0)
  expect_true(all(nzchar(f$label)))
  expect_setequal(c(f$field, "comparators"), names(formals(cea_study)))
})

test_that("the checklist has the 28 items and reacts to study, sources and analyses", {
  m <- tiny_model()
  chk <- cheers_checklist(m)
  expect_identical(chk$Item, 1:28)
  expect_named(chk, c("Item", "Section", "Topic", "Status", "Where"))
  expect_setequal(chk$Item[chk$Status == "Manuscript"], c(1, 2, 25, 26))
  status <- function(x, item) x$Status[x$Item == item]
  # Nothing entered: study items are missing; parameters have values but no source
  expect_identical(status(chk, 5), "Not reported")
  expect_identical(status(chk, 22), "Partly")
  expect_identical(status(chk, 23), "Not reported")

  s <- cea_study(population = "Adults", currency = "USD", comparators = c(a = "Old"),
                 model_rationale = "Simple comparison")
  chk <- cheers_checklist(m, study = s, analyses = "deterministic")
  expect_identical(status(chk, 5), "Reported")
  expect_identical(status(chk, 15), "Partly")          # currency without price year
  expect_identical(status(chk, 7), "Partly")           # strategy b not described
  expect_identical(status(chk, 16), "Partly")          # rationale but no diagram
  expect_identical(status(chk, 23), "Reported")
  expect_identical(status(chk, 20), "Partly")          # no PSA yet
  expect_match(chk$Where[chk$Item == 7], "b")

  s2 <- cea_study(currency = "USD", price_year = "2024", comparators = c(a = "Old", b = "New"),
                  model_rationale = "Simple comparison")
  p <- m$default_params(); p$source <- c("Trial X", "Registry Y")
  chk <- cheers_checklist(tiny_model(description = "Two arms"), study = s2, params = p,
                          analyses = c("deterministic", "psa", "subgroups", "equity"))
  for (it in c(7, 15, 16, 18, 19, 20, 22, 24)) expect_identical(status(chk, it), "Reported")
  p$source[2] <- NA
  expect_match(cheers_checklist(m, params = p)$Where[22], "1 of 2")

  # The study stored in the model is the default
  expect_identical(status(cheers_checklist(tiny_model(study = s)), 5), "Reported")
})

test_that("parameter source and note travel through the analyses", {
  expect_named(cea_param("x", 1, 0, 2),
               c("name", "mean", "lower", "upper", "base", "distribution",
                 "include_psa", "include_tornado", "source", "note"))
  p <- rbind(cea_param("c_b", 100, 50, 150, distribution = "gamma", source = "Trial X",
                       note = "Inflated to 2024"),
             cea_param("q_b", 0.1, 0.05, 0.2, distribution = "beta"))
  s <- sanitize_params(p)$df
  expect_identical(s$source, c("Trial X", NA))
  expect_identical(s$note, c("Inflated to 2024", NA))
  # A table without the columns stays without them
  expect_false("source" %in% names(sanitize_params(p[, 1:8])$df))
  # An empty CSV column comes back as text, so a source can be typed into it
  f <- tempfile(fileext = ".csv")
  utils::write.csv(p, f, row.names = FALSE, na = "")
  back <- sanitize_params(utils::read.csv(f, stringsAsFactors = FALSE))$df
  expect_identical(back$source, c("Trial X", NA))
  expect_type(back$note, "character")
  # The analyses ignore the text columns
  m <- tiny_model()
  expect_identical(run_deterministic(m, s)$table, run_deterministic(m, p[, 1:8])$table)
  set.seed(1); a <- run_psa(m, s, n_sim = 20)$results
  set.seed(1); b <- run_psa(m, p[, 1:8], n_sim = 20)$results
  expect_identical(a, b)
})

test_that("a diagram can be a file, a drawing function or a ggplot", {
  png_out <- function(model) {
    f <- tempfile(fileext = ".png")
    ok <- ShinyCEA:::.cea_diagram_png(model, f)
    if (ok) file.size(f) else 0
  }
  draw <- function() { graphics::plot.new(); graphics::text(0.5, 0.5, "A -> B") }
  expect_gt(png_out(tiny_model(diagram = draw)), 0)
  gg <- ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) + ggplot2::geom_point()
  expect_gt(png_out(tiny_model(diagram = gg)), 0)

  f <- tempfile(fileext = ".png")
  grDevices::png(f); draw(); grDevices::dev.off()
  expect_gt(png_out(tiny_model(diagram = f)), 0)

  expect_identical(png_out(tiny_model()), 0)                        # no diagram
  expect_identical(png_out(tiny_model(diagram = "missing.png")), 0) # file not found
  expect_error(tiny_model(diagram = 42), "diagram")
  # A drawing function that fails leaves no file behind
  bad <- tempfile(fileext = ".png")
  expect_false(ShinyCEA:::.cea_diagram_png(tiny_model(diagram = function() stop("x")), bad))
  expect_false(file.exists(bad))
  # The preferred height scales with the width
  attr(draw, "diagram_height") <- 4
  expect_equal(ShinyCEA:::.diagram_height(draw, 8), 4)
  expect_equal(ShinyCEA:::.diagram_height(draw, 4), 2)
  expect_output(print(tiny_model(diagram = draw)), "Diagram: \\(function\\)")
})

test_that("the bundled Sick-Sicker model shows the feature out of the box", {
  m <- cea_sick_sicker()
  expect_s3_class(m$study, "cea_study")
  expect_setequal(names(m$study$comparators), m$info()$strategies)
  expect_true(is.function(m$diagram))
  f <- tempfile(fileext = ".png")
  expect_true(ShinyCEA:::.cea_diagram_png(m, f))
  # Left empty on purpose, so users see what an incomplete item looks like
  chk <- cheers_checklist(m)
  expect_identical(chk$Status[chk$Item == 15], "Partly")
})

test_that("the Word report prints study details, diagram, sources and checklist", {
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available())
  m <- cea_sick_sicker()
  p <- sanitize_params(m$default_params())$df
  p$source[p$name == "u_H"] <- "Illustrative source ZZZ"
  det <- run_deterministic(m, p)
  out <- tempfile(fileext = ".docx")
  generate_cea_report(out, format = "word", model = m, det_result = det, params_df = p,
                      checklist = cheers_checklist(m, params = p, analyses = "deterministic"))
  expect_true(file.exists(out))
  # A .docx is a zip archive: read the text and the list of embedded images
  files <- utils::unzip(out, list = TRUE)$Name
  xml <- paste(readLines(unz(out, "word/document.xml"), warn = FALSE), collapse = "")
  expect_match(xml, "Study Details", fixed = TRUE)
  expect_match(xml, "Hypothetical cohort of healthy adults", fixed = TRUE)
  expect_match(xml, "Model Diagram", fixed = TRUE)
  expect_true(any(grepl("^word/media/", files)))
  expect_match(xml, "Illustrative source ZZZ", fixed = TRUE)
  expect_match(xml, "CHEERS 2022 Checklist", fixed = TRUE)

  # A model without study or diagram, and no extras: sections are left out
  out2 <- tempfile(fileext = ".docx")
  tm <- tiny_model()
  generate_cea_report(out2, format = "word", model = tm,
                      det_result = run_deterministic(tm, tm$default_params()))
  xml2 <- paste(readLines(unz(out2, "word/document.xml"), warn = FALSE), collapse = "")
  expect_false(grepl("Study Details", xml2, fixed = TRUE))
  expect_false(grepl("CHEERS", xml2, fixed = TRUE))
})

test_that("the exported code rebuilds the study details", {
  m <- cea_sick_sicker()
  s <- cea_study(population = "Adults \"aged\" 50+", assumptions = c("One", "Two"),
                 comparators = c(std = "Usual care", `new drug` = "Drug"))
  st <- list(model_code = "cea_sick_sicker()", strategies = m$info()$strategies,
             default_params = sanitize_params(m$default_params())$df,
             params = sanitize_params(m$default_params())$df, wtp = 100000,
             study = s, analyses = c("deterministic", "psa"))
  code <- cea_analysis_code("Study", st)
  env <- new.env(parent = globalenv())
  eval(parse(text = code[code != "library(ShinyCEA)"]), envir = env)
  expect_identical(env$study, s)
  expect_identical(env$checklist,
                   cheers_checklist(m, study = s, params = st$params,
                                    analyses = c("deterministic", "psa")))
  # An edited source appears as a parameter assignment
  st$params$source[st$params$name == "c_S1"] <- "Registry"
  expect_true('params$source[params$name == "c_S1"] <- "Registry"' %in%
                cea_analysis_code("Parameters", st))
  # No study at all still gives runnable code
  st$study <- NULL
  expect_no_error(eval(parse(text = setdiff(cea_analysis_code("Study", st), "library(ShinyCEA)")),
                       envir = new.env(parent = globalenv())))
})

test_that("the app has the Study tab and the diagram placeholder", {
  html <- as.character(cea_app_ui())
  expect_true(grepl('data-value="Study"', html, fixed = TRUE))
  for (id in c("study_form", "cheers_table", "download_cheers_csv", "model_diagram_ui")) {
    expect_true(grepl(paste0('id="', id, '"'), html, fixed = TRUE), info = id)
  }
})

test_that("the server builds the study form, the checklist and the diagram", {
  shiny::testServer(cea_app_server(list(sick_sicker = cea_sick_sicker())), {
    session$setInputs(wtp = 100000, ref_mode = "auto", psa_n = 50, psa_seed = 1)
    # Before the form exists the model's own study is used
    expect_identical(current_study(), cea_sick_sicker()$study)
    expect_identical(analyses_run(), NULL)
    expect_identical(cheers_data()$Status[cheers_data()$Item == 23], "Not reported")

    session$setInputs(run_det = 1)
    expect_identical(analyses_run(), "deterministic")
    expect_identical(cheers_data()$Status[cheers_data()$Item == 23], "Reported")

    # Filling the form changes the study and the checklist
    vals <- stats::setNames(as.list(rep("", nrow(ShinyCEA:::.study_fields))),
                            paste0("study_", ShinyCEA:::.study_fields$field))
    vals$study_population <- "Edited population"
    vals$study_currency <- "USD"; vals$study_price_year <- "2024"
    vals$study_assumptions <- "First\nSecond"
    do.call(session$setInputs, vals)
    session$setInputs(study_comp_1 = "Usual care")
    expect_identical(current_study()$population, "Edited population")
    expect_identical(current_study()$assumptions, c("First", "Second"))
    expect_identical(current_study()$comparators, c(std = "Usual care"))
    expect_identical(cheers_data()$Status[cheers_data()$Item == 15], "Reported")
    expect_identical(cheers_data()$Status[cheers_data()$Item == 7], "Partly")
    expect_identical(code_state()$study, current_study())

    # The diagram block is rendered for a model that has a diagram
    expect_match(as.character(output$model_diagram_ui$html), "model_diagram_plot")
  })
})
