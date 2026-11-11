# Tests for the "R code" export of the app (R/cea-code.R) and the pieces it
# relies on: the survival / trace helpers and the capture of the model
# expression in run_cea_app().
#
# The main check is behavioural: the generated script is run in a clean
# environment and its objects are compared with direct calls.

# App state for the bundled Sick-Sicker model with the app's default inputs.
code_state <- function(...) {
  m <- cea_sick_sicker()
  st <- list(
    model_code = "cea_sick_sicker()", model_name = m$info()$name,
    strategies = m$info()$strategies,
    default_params = sanitize_params(m$default_params())$df,
    params = sanitize_params(m$default_params())$df,
    wtp = 100000, ref_mode = "auto", ref_pinned = NULL,
    psa_n = 100, psa_seed = 123, palette_choice = "cream",
    model_subgroups = m$subgroups
  )
  utils::modifyList(st, list(...))
}

# Run generated code in its own environment; plots go to a null device.
run_code <- function(lines) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  env <- new.env(parent = globalenv())
  # The package is loaded for the tests, so the script's library() call is dropped
  lines <- lines[lines != "library(ShinyCEA)"]
  eval(parse(text = lines), envir = env)
  env
}

test_that("every tab and the full script give code that parses and runs", {
  st <- code_state(
    scenarios = list("Base case" = list(), "No discounting" = list(d_cost = 0, d_qaly = 0)),
    subgroups = list("Overall" = list(), "High risk" = list(p_HS_std = 0.08)),
    composites = list("Current care" = c(std = 0.6, t1 = 0.4), "New drug" = c(t2 = 1)),
    thr = list(strategy = "t1", param = "c_S1", mode = "inb", grid_n = 10, min = 500, max = 1200),
    tw = list(param1 = "c_S1", param2 = "c_S2", target = "t1", metric = "INB", n = 5)
  )
  for (tab in c(ShinyCEA:::.code_tabs, "full")) {
    code <- cea_analysis_code(tab, st)
    expect_type(code, "character")
    expect_no_error(parse(text = code))
    expect_no_error(suppressWarnings(run_code(code)))
  }
})

test_that("the deterministic code reproduces run_deterministic()", {
  st <- code_state()
  env <- run_code(cea_analysis_code("Deterministic", st))
  m <- cea_sick_sicker()
  direct <- run_deterministic(m, sanitize_params(m$default_params())$df, wtp = 100000)
  expect_identical(env$det$table, direct$table)
  expect_identical(env$det$ref, direct$ref)
  # Code for a tab that needs no PSA does not run one
  expect_false(any(grepl("run_psa", cea_analysis_code("Deterministic", st))))
})

test_that("the PSA code reproduces a seeded run_psa() and the CEAC family", {
  st <- code_state(psa_n = 150, psa_seed = 777, ceac_show_ceaf = TRUE)
  code <- cea_analysis_code("CEAC", st)
  expect_true("set.seed(777)" %in% code)
  expect_true(any(grepl("ceaf = ceaf", code, fixed = TRUE) & !grepl("add `ceaf", code, fixed = TRUE)))
  env <- run_code(code)

  m <- cea_sick_sicker()
  p <- sanitize_params(m$default_params())$df
  set.seed(777)
  psa <- run_psa(m, p, n_sim = 150)
  strats <- m$info()$strategies
  expect_identical(env$psa$results, psa$results)
  expect_identical(env$ceac, compute_ceac(psa$results, strats, wtp_max = 200000))
  expect_identical(env$ceaf, compute_ceaf(psa$results, strats, wtp_max = 200000))
  expect_identical(env$el, compute_expected_loss(psa$results, strats, wtp_max = 200000,
                                                 wtp_step = 5000))
  # Without the checkbox the frontier is not passed to the plot
  off <- cea_analysis_code("CEAC", code_state(ceac_show_ceaf = FALSE))
  expect_true("plot_ceac(ceac, palette = pal)   # add `ceaf = ceaf` to superimpose the frontier" %in% off)
})

test_that("the number of PSA iterations is kept within the app's limits", {
  expect_true(any(grepl("n_sim = 10000)", cea_analysis_code("PSA", code_state(psa_n = 5e6)), fixed = TRUE)))
  expect_true(any(grepl("n_sim = 100)", cea_analysis_code("PSA", code_state(psa_n = NA)), fixed = TRUE)))
})

test_that("edited parameters become assignments that change the result", {
  st <- code_state()
  st$params$base[st$params$name == "c_S2"] <- 5000
  st$params$include_psa[st$params$name == "u_H"] <- FALSE
  code <- cea_analysis_code("Deterministic", st)
  expect_true('params$base[params$name == "c_S2"] <- 5000' %in% code)
  expect_true('params$include_psa[params$name == "u_H"] <- FALSE' %in% code)
  env <- run_code(code)
  expect_identical(env$params, st$params)
  m <- cea_sick_sicker()
  expect_identical(env$det$table, run_deterministic(m, st$params, wtp = 100000)$table)
  # Unedited tables give no assignment lines
  expect_false(any(grepl("^params\\$", cea_analysis_code("Deterministic", code_state()))))
})

test_that("a table with other rows than the defaults falls back to the CSV", {
  st <- code_state()
  st$params <- st$params[-1, ]
  code <- cea_analysis_code("Parameters", st)
  expect_true(any(grepl("parameters_current.csv", code, fixed = TRUE)))
  expect_false(any(grepl("model$default_params()", code, fixed = TRUE)))
})

test_that("the reference, WTP and palette follow the app settings", {
  auto <- cea_analysis_code("Deterministic", code_state(wtp = 50000))
  expect_true(any(grepl("^ref_strategy <- NULL", auto)))
  expect_true(any(grepl("^wtp <- 50000 ", auto)))          # plain notation, not 5e+04
  pinned <- cea_analysis_code("Deterministic",
                              code_state(ref_mode = "pinned", ref_pinned = "t2", palette_choice = "uw"))
  expect_true(any(grepl('^ref_strategy <- "t2"', pinned)))
  expect_true('pal <- cea_palette("uw")' %in% pinned)
  expect_identical(run_code(pinned)$det$ref, "t2")
})

test_that("scenarios, subgroups and composites are written as literals", {
  sc <- list("Base case" = list(), "No discounting" = list(d_cost = 0, d_qaly = 0))
  env <- run_code(cea_analysis_code("Scenarios", code_state(scenarios = sc)))
  expect_identical(env$scenarios, sc)
  m <- cea_sick_sicker()
  expect_identical(env$sc, run_scenarios(m, sanitize_params(m$default_params())$df, sc, wtp = 100000))

  wc <- list("Current care" = c(std = 0.6, t1 = 0.4), "New drug" = c(t2 = 1))
  expect_identical(run_code(cea_analysis_code("Weighted", code_state(composites = wc)))$composites, wc)

  sg <- list("Overall" = list(), "High risk" = list(p_HS_std = 0.08))
  expect_identical(run_code(cea_analysis_code("Subgroups", code_state(subgroups = sg)))$subgroups, sg)
})

test_that("tabs with nothing defined give comments, not broken code", {
  for (tab in c("Scenarios", "Weighted", "Subgroups", "Threshold")) {
    code <- cea_analysis_code(tab, code_state(scenarios = list(), composites = list(),
                                              subgroups = list()))
    section <- code[seq(which(grepl("^# ---- ", code))[2], length(code))]
    expect_true(all(grepl("^#", section)), info = tab)
  }
})

test_that("a model whose source is unknown gets a placeholder line", {
  code <- cea_analysis_code("Parameters", code_state(model_code = NA_character_))
  expect_true("model <- my_model" %in% code)
  expect_true(any(grepl("TODO", code, fixed = TRUE)))
})

test_that("values are written so that they round-trip", {
  expect_identical(ShinyCEA:::.code_num(100000), "100000")
  expect_identical(eval(parse(text = ShinyCEA:::.code_num(0.1 + 0.2))), 0.1 + 0.2)
  expect_identical(eval(parse(text = ShinyCEA:::.code_val(c("a\"b", "c")))), c("a\"b", "c"))
  expect_identical(eval(parse(text = ShinyCEA:::.code_val(c(std = 0.5, `new drug` = 0.5)))),
                   c(std = 0.5, `new drug` = 0.5))
})

test_that("run_cea_app() records how each model was supplied", {
  # Capture the arguments of cea_app_server() without starting an app
  seen <- NULL
  local_mocked_bindings(cea_app_server = function(models, model_code = NULL) {
    seen <<- model_code
    function(input, output, session) NULL
  })
  my_model <- cea_sick_sicker()
  run_cea_app(my_model, cea_pf_prog_death(), launch.browser = FALSE)
  expect_identical(unname(seen), c("my_model", "cea_pf_prog_death()"))
  expect_identical(names(seen), c(my_model$info()$key, cea_pf_prog_death()$info()$key))

  run_cea_app(launch.browser = FALSE)
  expect_identical(unname(seen[1]), "cea_sick_sicker()")

  model_set <- list(a = my_model)
  run_cea_app(models = model_set, launch.browser = FALSE)
  expect_identical(seen, c(a = 'model_set[["a"]]'))

  # A call built from objects has no source to show
  do.call(run_cea_app, list(my_model, launch.browser = FALSE))
  expect_true(is.na(seen[[1]]))
})

test_that("survival and trace helpers shape Markov and survival-model output", {
  m <- cea_sick_sicker()
  p <- m$default_params()
  det <- run_deterministic(m, p)
  surv <- cea_survival_data(det, p)
  expect_named(surv, c("time", "surv", "curve", "strategy"))
  expect_setequal(unique(surv$strategy), m$info()$strategies)
  expect_true(all(surv$surv >= 0 & surv$surv <= 1))

  tr <- cea_trace_data(det, p)
  expect_named(tr, c("strategy", "time", "state", "prob"))
  # State occupancy adds up to 1 at every time point of every strategy
  tot <- stats::aggregate(prob ~ strategy + time, data = tr, FUN = sum)
  expect_equal(tot$prob, rep(1, nrow(tot)), tolerance = 1e-8)

  # A partitioned-survival model has curves but no trace: states are derived
  ps <- cea_partitioned_survival()
  det_ps <- run_deterministic(ps, ps$default_params())
  tr_ps <- cea_trace_data(det_ps, ps$default_params())
  expect_setequal(unique(tr_ps$state), c("PF", "Progressed", "Death"))
  tot_ps <- stats::aggregate(prob ~ strategy + time, data = tr_ps, FUN = sum)
  expect_equal(tot_ps$prob, rep(1, nrow(tot_ps)), tolerance = 1e-8)

  # Trace-only output: overall survival comes from the death column
  expect_identical(
    ShinyCEA:::.cea_extract_surv(list(trace = cbind(A = c(1, 0.8), Dead = c(0, 0.2))), 0.5),
    data.frame(time = c(0, 0.5), surv = c(1, 0.8), curve = "OS", stringsAsFactors = FALSE))
})

test_that("the app has the code button and the PSA seed", {
  html <- as.character(cea_app_ui())
  expect_true(grepl('id="show_code"', html, fixed = TRUE))
  expect_true(grepl('id="psa_seed"', html, fixed = TRUE))
})
