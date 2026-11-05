# =============================================================================
# Construction of the CEAC, the CEAF and the expected loss curves (ELCs)
#
# Definitions follow Alarid-Escudero, Enns, Kuntz, Michaud & Jalal (2019),
# Value in Health 22(5):611-618:
#   Eq. 1  Pr(CE)_d = N_d / N           (share of iterations where d is best)
#   Eq. 2  d* = argmax_d mean_i(B_id)   (CEAF follows the optimal strategy)
#   Eq. 3-4 EL_d = mean_i(max_d B_id - B_id); min_d EL_d = EVPI
#
# Part 1 checks every curve against values worked out by hand on a 4-iteration
# toy PSA. Part 2 checks the identities that must hold on any PSA.
# =============================================================================

# ---------------------------------------------------------------------------
# Toy PSA with a known answer
#   A: cost 10, QALY 0 in every iteration
#   B: cost (0, 0, 0, 100), QALY 0.001 in every iteration
# NMB_A = -10 and NMB_B = 0.001 * WTP - cost_B, so
#   - B is best in 3 of 4 iterations until WTP > 90,000 (then in all 4), but
#   - B is optimal in expectation only once 0.001 * WTP - 25 > -10, i.e.
#     WTP > 15,000. At WTP = 0 the optimal strategy (A) is therefore NOT the
#     one most likely to be cost-effective, the case the CEAF exists to show.
# ---------------------------------------------------------------------------
toy_psa <- function() {
  data.frame(
    A.cost = rep(10, 4),
    A.q    = rep(0, 4),
    B.cost = c(0, 0, 0, 100),
    B.q    = rep(0.001, 4)
  )
}
toy_strategies <- c("A", "B")
toy_grid <- seq(0, 100000, by = 20000)

# Hand-computed expectations on toy_grid (see header for the algebra)
toy_prob_a   <- c(0.25, 0.25, 0.25, 0.25, 0.25, 0)
toy_prob_b   <- c(0.75, 0.75, 0.75, 0.75, 0.75, 1)
toy_optimal  <- c("A", "B", "B", "B", "B", "B")
toy_el_a     <- c(7.5, 22.5, 37.5, 52.5, 67.5, 85)
toy_el_b     <- c(22.5, 17.5, 12.5, 7.5, 2.5, 0)

# ===========================================================================
# Part 1: hand-computed values
# ===========================================================================
test_that("compute_ceac reproduces the hand-computed probabilities (Eq. 1)", {
  ceac <- compute_ceac(toy_psa(), toy_strategies,
                       wtp_max = 100000, wtp_step = 20000)

  expect_equal(sort(unique(ceac$WTP)), toy_grid)
  a <- ceac[ceac$Strategy == "A", ]
  b <- ceac[ceac$Strategy == "B", ]
  expect_equal(a$ProbCE[order(a$WTP)], toy_prob_a, tolerance = 1e-10)
  expect_equal(b$ProbCE[order(b$WTP)], toy_prob_b, tolerance = 1e-10)
})

test_that("compute_ceaf follows the optimal strategy, not the most probable (Eq. 2)", {
  ceaf <- compute_ceaf(toy_psa(), toy_strategies,
                       wtp_max = 100000, wtp_step = 20000)

  expect_named(ceaf, c("WTP", "Strategy", "ProbCE", "Expected_NMB"))
  expect_equal(ceaf$WTP, toy_grid)
  expect_equal(ceaf$Strategy, toy_optimal)
  # Frontier height = probability of the OPTIMAL strategy
  expect_equal(ceaf$ProbCE, c(0.25, 0.75, 0.75, 0.75, 0.75, 1), tolerance = 1e-10)
  # At WTP = 0 the frontier sits BELOW the highest CEAC (0.25 vs 0.75)
  expect_lt(ceaf$ProbCE[1], max(toy_prob_a[1], toy_prob_b[1]))
  # Expected NMB of the optimal strategy: -10 for A, 0.001 * WTP - 25 for B
  expect_equal(ceaf$Expected_NMB, c(-10, -5, 15, 35, 55, 75), tolerance = 1e-8)
})

test_that("compute_expected_loss reproduces the hand-computed losses (Eq. 3-4)", {
  el <- compute_expected_loss(toy_psa(), toy_strategies,
                              wtp_max = 100000, wtp_step = 20000)

  a <- el[el$Strategy == "A", ]
  b <- el[el$Strategy == "B", ]
  expect_equal(a$Expected_Loss[order(a$WTP)], toy_el_a, tolerance = 1e-8)
  expect_equal(b$Expected_Loss[order(b$WTP)], toy_el_b, tolerance = 1e-8)
})

test_that("on the toy PSA the lowest expected loss is the EVPI and the CEAF strategy", {
  el   <- compute_expected_loss(toy_psa(), toy_strategies,
                                wtp_max = 100000, wtp_step = 20000)
  evpi <- compute_evpi(toy_psa(), toy_strategies,
                       wtp_max = 100000, wtp_step = 20000)
  ceaf <- compute_ceaf(toy_psa(), toy_strategies,
                       wtp_max = 100000, wtp_step = 20000)

  expect_equal(evpi$EVPI, pmin(toy_el_a, toy_el_b), tolerance = 1e-8)
  for (i in seq_along(toy_grid)) {
    sub <- el[el$WTP == toy_grid[i], ]
    expect_equal(sub$Strategy[which.min(sub$Expected_Loss)], ceaf$Strategy[i])
  }
})

test_that("ties are shared in the CEAC and resolved to the first strategy in the CEAF", {
  # Two identical strategies: each iteration is a tie at the maximum NMB
  tie <- data.frame(A.cost = c(5, 8), A.q = c(1, 2),
                    B.cost = c(5, 8), B.q = c(1, 2))
  ceac <- compute_ceac(tie, c("A", "B"), wtp_max = 50000, wtp_step = 50000)
  ceaf <- compute_ceaf(tie, c("A", "B"), wtp_max = 50000, wtp_step = 50000)
  el   <- compute_expected_loss(tie, c("A", "B"), wtp_max = 50000, wtp_step = 50000)

  expect_equal(ceac$ProbCE, rep(0.5, 4))
  expect_equal(ceaf$Strategy, c("A", "A"))
  expect_equal(ceaf$ProbCE, c(0.5, 0.5))
  expect_equal(el$Expected_Loss, rep(0, 4))
})

test_that("a single strategy is always optimal with probability 1 and no loss", {
  one <- data.frame(A.cost = c(1, 2, 3), A.q = c(1, 1, 1))
  ceaf <- compute_ceaf(one, "A", wtp_max = 100000, wtp_step = 50000)
  el   <- compute_expected_loss(one, "A", wtp_max = 100000, wtp_step = 50000)

  expect_equal(ceaf$Strategy, rep("A", 3))
  expect_equal(ceaf$ProbCE, rep(1, 3))
  expect_equal(el$Expected_Loss, rep(0, 3))
})

# ===========================================================================
# Part 2: identities that hold on any PSA (bundled Sick-Sicker model)
# ===========================================================================
test_that("CEAC, CEAF, ELC and EVPI are mutually consistent on a real PSA", {
  m <- cea_sick_sicker()
  strategies <- m$info()$strategies
  set.seed(20261104)
  psa <- run_psa(m, m$default_params(), n_sim = 200)

  ceac <- compute_ceac(psa$results, strategies, wtp_max = 200000, wtp_step = 20000)
  ceaf <- compute_ceaf(psa$results, strategies, wtp_max = 200000, wtp_step = 20000)
  el   <- compute_expected_loss(psa$results, strategies,
                                wtp_max = 200000, wtp_step = 20000)
  evpi <- compute_evpi(psa$results, strategies, wtp_max = 200000, wtp_step = 20000)

  # Structure: one frontier point per WTP, drawn from the model's strategies
  expect_equal(ceaf$WTP, seq(0, 200000, by = 20000))
  expect_true(all(ceaf$Strategy %in% strategies))
  expect_true(all(el$Expected_Loss >= 0))

  for (i in seq_len(nrow(ceaf))) {
    w <- ceaf$WTP[i]
    ceac_w <- ceac[ceac$WTP == w, ]
    el_w   <- el[el$WTP == w, ]

    # Eq. 1: probabilities over strategies sum to one
    expect_equal(sum(ceac_w$ProbCE), 1, tolerance = 1e-10)

    # Eq. 2: the optimal strategy maximizes expected NMB (computed directly)
    exp_nmb <- vapply(strategies, function(s) {
      mean(w * psa$results[[paste0(s, ".q")]] - psa$results[[paste0(s, ".cost")]])
    }, numeric(1))
    expect_equal(ceaf$Strategy[i], strategies[which.max(exp_nmb)])
    expect_equal(ceaf$Expected_NMB[i], max(exp_nmb), tolerance = 1e-8)

    # The frontier lies ON the CEAC of the optimal strategy, never above the top curve
    expect_equal(ceaf$ProbCE[i],
                 ceac_w$ProbCE[ceac_w$Strategy == ceaf$Strategy[i]],
                 tolerance = 1e-10)
    expect_lte(ceaf$ProbCE[i], max(ceac_w$ProbCE) + 1e-12)

    # Eq. 3-4: lowest expected loss <=> optimal strategy, and equals the EVPI
    expect_equal(el_w$Strategy[which.min(el_w$Expected_Loss)], ceaf$Strategy[i])
    expect_equal(min(el_w$Expected_Loss), evpi$EVPI[evpi$WTP == w],
                 tolerance = 1e-6)

    # Ranking by expected loss is the reverse of ranking by expected NMB,
    # because EL_d = E[max NMB] - E[NMB_d] and the first term is common.
    expect_equal(el_w$Expected_Loss[match(strategies, el_w$Strategy)],
                 max(exp_nmb) + evpi$EVPI[evpi$WTP == w] - unname(exp_nmb),
                 tolerance = 1e-6)
  }
})

# ===========================================================================
# Part 3: plots
# ===========================================================================
test_that("plot_ceac superimposes the CEAF only when it is supplied", {
  ceac <- compute_ceac(toy_psa(), toy_strategies, wtp_max = 100000, wtp_step = 20000)
  ceaf <- compute_ceaf(toy_psa(), toy_strategies, wtp_max = 100000, wtp_step = 20000)

  p0 <- plot_ceac(ceac)
  p1 <- plot_ceac(ceac, ceaf = ceaf)
  expect_s3_class(p1, "ggplot")
  expect_length(p0$layers, 1L)
  expect_length(p1$layers, 2L)

  # The frontier layer plots exactly the CEAF points
  built <- ggplot2::ggplot_build(p1)$data[[2]]
  expect_equal(built$x, ceaf$WTP)
  expect_equal(built$y, ceaf$ProbCE)

  expect_error(plot_ceac(ceac, ceaf = data.frame(WTP = 0, ProbCE = 1)))
})

test_that("plot_expected_loss marks the lower envelope and supports a log axis", {
  el <- compute_expected_loss(toy_psa(), toy_strategies,
                              wtp_max = 100000, wtp_step = 20000)

  p0 <- plot_expected_loss(el)
  p1 <- plot_expected_loss(el, show_optimal = TRUE)
  expect_length(p0$layers, 1L)
  expect_length(p1$layers, 2L)

  # Markers sit on the minimum expected loss at each WTP (= EVPI)
  built <- ggplot2::ggplot_build(p1)$data[[2]]
  expect_equal(built$x, toy_grid)
  expect_equal(built$y, pmin(toy_el_a, toy_el_b), tolerance = 1e-8)

  # Log axis: the zero loss of B at WTP = 100,000 is dropped, nothing is -Inf
  p2 <- plot_expected_loss(el, show_optimal = TRUE, log_scale = TRUE)
  built2 <- ggplot2::ggplot_build(p2)$data
  expect_equal(nrow(built2[[1]]), nrow(el) - 1L)
  expect_true(all(is.finite(built2[[1]]$y)))
  expect_true(all(is.finite(built2[[2]]$y)))
})

test_that("the CEAC tab exposes the CEAF checkbox and the expected loss plot", {
  html <- as.character(cea_app_ui())
  expect_true(grepl('id="ceac_show_ceaf"', html, fixed = TRUE))
  expect_true(grepl('id="ceac_el_plot"', html, fixed = TRUE))
})
