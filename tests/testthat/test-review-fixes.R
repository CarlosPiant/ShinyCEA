# Tests for the fixes of the 2026-10-21 deep review (ShinyCEA side).
# Each block names the finding it guards against.

toy_model <- function() {
  # Cost = 1000 * x, QALY = y: everything observable from the parameters.
  info <- function() list(key = "toy", name = "Toy", strategies = c("A", "B"), horizon = 10)
  params <- function() rbind(
    cea_param("x", 0.5, 0.2, 0.8, distribution = "beta", include_psa = FALSE),
    cea_param("y", 2, 1, 3, distribution = "normal"),
    cea_param("time_horizon", 10, 5, 15, distribution = "fixed", include_psa = FALSE)
  )
  simulate <- function(pars, strategy) {
    k <- if (strategy == "A") 1 else 1.5
    list(cost = 1000 * pars$x * k, qaly = pars$y * k)
  }
  new_cea_model(info = info, params = params, simulate = simulate)
}

test_that("run_psa samples only rows with include_psa (review A2)", {
  m <- toy_model()
  set.seed(1)
  psa <- run_psa(m, m$default_params(), n_sim = 200)
  expect_equal(stats::sd(psa$results$A.cost), 0)          # x was not varied
  expect_gt(stats::sd(psa$results$A.q), 0)                 # y was
  expect_equal(names(psa$draws), "y")
})

test_that("sample_psa varies triangular and PERT parameters (review D2)", {
  df <- rbind(cea_param("x", 5, 2, 8, distribution = "triangular"),
              cea_param("y", 5, 2, 8, distribution = "pert"))
  set.seed(2)
  d <- sample_psa(df, n = 200)
  expect_gt(stats::sd(d$x), 0); expect_gt(stats::sd(d$y), 0)
  expect_true(all(d$x >= 2 & d$x <= 8)); expect_true(all(d$y >= 2 & d$y <= 8))
})

test_that("run_scenarios keeps one reference for every scenario (review D4)", {
  m <- cea_sick_sicker()
  res <- run_scenarios(m, m$default_params(),
                       list("Base case" = list(), "Cheap t4" = list(drug_t4 = 350),
                            "Expensive t4" = list(drug_t4 = 1400)), wtp = 100000)
  expect_equal(length(unique(res$Reference)), 1)
  # The reference row itself has no ICER; every other strategy with more QALYs does.
  others <- res[res$Strategy != res$Reference[1] & res$QALY > 0, ]
  expect_true(nrow(others) > 0)
})

test_that("run_bia charges an annual cost per patient (review D1)", {
  m <- toy_model()
  pars <- m$default_params()
  bia <- run_bia(m, pars, population = 1000, current_mix = c(A = 1, B = 0),
                 new_mix = c(A = 0, B = 1), years = 3)
  lifetime_A <- m$simulate(stats::setNames(as.list(pars$base), pars$name), "A")$cost
  expect_equal(unique(bia$annual$Per_Patient_Cost[bia$annual$Strategy == "A"]), lifetime_A / 10)
  expect_equal(bia$impact$Cost_Current[1], 1000 * lifetime_A / 10)
  expect_equal(bia$horizon_years, 10)
  # Without any horizon information the function refuses rather than guessing.
  m2 <- toy_model(); m2$info <- function() list(key = "toy", name = "Toy", strategies = c("A", "B"))
  expect_error(run_bia(m2, pars[pars$name != "time_horizon", ], 1000, c(A = 1, B = 0), c(A = 0, B = 1)),
               "horizon")
})

test_that("health check fails on parameter names the model does not know (review E7)", {
  m <- cea_sick_sicker()
  df <- m$default_params()
  ok <- validate_params_and_model(m, df)
  expect_equal(ok$status[ok$check == "Parameter names match model"], "PASS")
  df$name[df$name == "u_H"] <- "u_h"
  bad <- validate_params_and_model(m, df)
  expect_equal(bad$status[bad$check == "Parameter names match model"], "FAIL")
  expect_true(grepl("u_H", bad$details[bad$check == "Parameter names match model"]))
})

test_that("the microsimulation model is reproducible and leaves the RNG alone (review E5)", {
  m <- cea_microsim()
  pars <- stats::setNames(as.list(m$default_params()$base), m$default_params()$name)
  set.seed(99); before <- stats::runif(1)
  set.seed(99)
  a <- m$simulate(pars, "standard"); b <- m$simulate(pars, "standard")
  after <- stats::runif(1)
  expect_identical(a$cost, b$cost); expect_identical(a$qaly, b$qaly)
  expect_identical(before, after)                           # caller's stream untouched
  pars$seed <- 7
  expect_false(identical(m$simulate(pars, "standard")$cost, a$cost))
})

test_that("the app clears results on model switch and flags stale inputs (review E1, E3, E4, E8)", {
  models <- list(sick_sicker = cea_sick_sicker(), pf = cea_pf_prog_death())
  shiny::testServer(cea_app_server(models), {
    session$setInputs(model_key = "sick_sicker", wtp = 100000, ref_mode = "auto",
                      psa_n = 100, tornado_metric = "INB")
    session$setInputs(run_det = 1)
    expect_false(is.null(det_result()))
    expect_equal(det_result()$wtp, 100000)
    expect_false(stale())
    # A changed WTP marks the results stale until the next run.
    session$setInputs(wtp = 20000)
    expect_true(stale())
    session$setInputs(run_det = 2)
    expect_false(stale()); expect_equal(det_result()$wtp, 20000)
    # Editing base moves mean and the equal bounds with it (E4).
    df <- params_rv(); i <- which(df$name == "drug_t4"); col <- which(names(df) == "base")
    session$setInputs(param_table_cell_edit = data.frame(row = i, col = col, value = "1400"))
    expect_equal(params_rv()$mean[i], 1400)
    expect_true(stale())
    # The row-number column is ignored instead of crashing (E8).
    session$setInputs(param_table_cell_edit = data.frame(row = 1, col = 0, value = "x"))
    expect_equal(params_rv()$mean[i], 1400)
    # Switching model drops the results and the old model's stores (E1, E6).
    session$setInputs(model_key = "pf")
    expect_null(det_result()); expect_null(psa_results())
    expect_false(stale())
    expect_true(any(grepl("PF", params_rv()$name)))   # the pf model's own table is loaded
  })
})

# ---- MINOR findings --------------------------------------------------------------

test_that("probability-sum groups are only those the caller defines (D5)", {
  df <- rbind(cea_param("p_death_t1", 0.6, 0.5, 0.7, distribution = "beta"),
              cea_param("p_death_t2", 0.5, 0.4, 0.6, distribution = "beta"))
  res <- check_probabilities_sum(df)
  expect_false(any(res$status == "FAIL"))
  res2 <- check_probabilities_sum(df, prob_groups = list(g = c("p_death_t1", "p_death_t2")))
  expect_true(any(res2$status == "FAIL"))
})

test_that("frontier handles NA rows, equivalent strategies and dominance by non-frontier strategies (D6, D7)", {
  df <- data.frame(Strategy = c("F1", "F2", "B", "A", "X"),
                   Cost = c(0, 100, 60, 61, NA), QALY = c(0, 10, 5, 4.9, 3), stringsAsFactors = FALSE)
  fa <- frontier_analysis(df)
  st <- stats::setNames(fa$classified$Status, fa$classified$Strategy)
  expect_equal(unname(st["X"]), "Not evaluable")
  expect_equal(unname(st["A"]), "Dominated")
  chk <- check_dominance_logic(list(frontier_status = fa$classified))
  expect_false(any(chk$status == "FAIL"))
  twin <- data.frame(Strategy = c("A", "B", "C"), Cost = c(0, 50, 50), QALY = c(0, 5, 5), stringsAsFactors = FALSE)
  st2 <- stats::setNames(frontier_analysis(twin)$classified$Status, twin$Strategy)
  expect_equal(unname(st2["C"]), "Equivalent")
  expect_equal(unname(st2["B"]), "Frontier")
})

test_that("CEAC shares ties equally (D8)", {
  psa <- data.frame(A.cost = c(10, 10), A.q = c(1, 1), B.cost = c(10, 10), B.q = c(1, 1))
  ceac <- compute_ceac(psa, c("A", "B"), wtp_max = 10000, wtp_step = 10000)
  expect_true(all(abs(ceac$ProbCE - 0.5) < 1e-12))
})

test_that("the concentration index can rank by a subgroup override (D10)", {
  m <- toy_model()
  sg <- list(G1 = list(overrides = list(y = 1), share = 0.5, baseline_qaly = 0.8),
             G2 = list(overrides = list(y = 3), share = 0.5, baseline_qaly = 0.6))
  d <- run_distributional_cea(m, m$default_params(), sg, wtp = 100000)
  by_base <- calculate_concentration_index(d, "A", rank_by = "baseline")
  by_y <- calculate_concentration_index(d, "A", rank_by = "y")
  expect_equal(sign(by_base$index), -sign(by_y$index))   # opposite rankings
  expect_error(calculate_concentration_index(d, "A", rank_by = "nope"), "not overridden")
})

test_that("the s-curve uptake starts at the current mix and reaches the target (D11)", {
  sh <- define_market_shares(current = c(A = 1, B = 0), new_target = c(A = 0, B = 1))
  up <- define_uptake_curve(sh, years = 5, shape = "s_curve")
  expect_equal(unname(up[[1]][["B"]]), 0)
  expect_equal(unname(up[[5]][["B"]]), 1)
  expect_true(all(diff(vapply(up, function(x) x[["B"]], numeric(1))) > 0))
})

test_that("the half-cycle heuristic is not reported as a pass (D12)", {
  m <- cea_sick_sicker()
  res <- check_half_cycle(m, m$default_params())
  expect_false(any(res$status == "PASS" & grepl("HCC heuristic", res$check)))
})

test_that("bundled models rescale probabilities with the cycle length and cap PFS at OS (E13)", {
  m <- cea_sick_sicker()
  pars <- stats::setNames(as.list(m$default_params()$base), m$default_params()$name)
  os <- function(cl) { p <- pars; p$cycle_length <- cl; s <- m$simulate(p, "std")$survival; s$surv[nrow(s)] }
  expect_equal(os(0.5), os(0.25), tolerance = 0.02)       # same 10-year survival, coarser cycle
  ps <- cea_partitioned_survival()
  pp <- stats::setNames(as.list(ps$default_params()$base), ps$default_params()$name)
  pp$pfs_shape_std <- 2.0                                     # PFS would cross OS without the cap
  sv <- ps$simulate(pp, "std")$survival
  o <- sv[sv$curve == "OS", ]; f <- sv[sv$curve == "PFS", ]
  expect_true(all(o$surv[order(o$time)] >= f$surv[order(f$time)] - 1e-12))
})

test_that("validate_cea_model checks every strategy and scalar outputs (E14)", {
  bad2 <- function() list(cost = c(1, 2), qaly = 1)
  m <- function(sim) new_cea_model(
    info = function() list(key = "k", name = "n", strategies = c("A", "B")),
    params = function() cea_param("x", 1, 0, 2), simulate = sim)
  expect_error(m(function(pars, strategy) if (strategy == "B") stop("boom") else list(cost = 1, qaly = 1)), "strategy 'B'")
  expect_error(m(function(pars, strategy) bad2()), "single cost")
  expect_error(new_cea_model(info = function() list(key = "k", name = "n", strategies = c("A", "B")),
                             params = function() cea_param("x", NA, 0, 2),
                             simulate = function(pars, strategy) list(cost = 1, qaly = 1)), "non-finite base")
})

test_that("the threshold run remembers its inputs and PSA tabs explain what is missing (E9, E11)", {
  shiny::testServer(cea_app_server(list(sick_sicker = cea_sick_sicker())), {
    session$setInputs(wtp = 100000, ref_mode = "auto", psa_n = 100, tornado_metric = "INB", run_det = 1)
    session$setInputs(thr_param = "p_HS_std", thr_strategy = "t1", thr_mode = "icer",
                      thr_min = 0.02, thr_max = 0.08, thr_grid_n = 5, run_threshold = 1)
    res <- threshold_result()
    expect_equal(res$param, "p_HS_std"); expect_equal(res$strategy, "t1"); expect_equal(res$wtp, 100000)
    session$setInputs(thr_param = "u_H", thr_strategy = "t2")      # live controls change, the run does not
    expect_equal(threshold_result()$param, "p_HS_std")
    expect_true(grepl("p_HS_std", output$threshold_text))
  })
})
