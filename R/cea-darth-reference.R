#' DARTH Coding Framework Reference
#'
#' Returns a descriptive table of the DARTH Group's recommended naming
#' conventions, object structures, and coding patterns for decision-analytic
#' models in R. Based on Alarid-Escudero et al. (2019), "A Need for Change!
#' A Coding Framework for Improving Transparency in Decision Modeling",
#' *PharmacoEconomics*, 37(11), 1329-1339.
#'
#' The framework standardizes variable naming, function design, and model
#' structure to improve transparency, reproducibility, and peer review of
#' health economic models.
#'
#' @param category Optional filter. One of "prefixes", "objects", "functions",
#'   "parameters", "model_structure", or "all" (default).
#' @param print Logical; if TRUE (default), prints the table formatted for
#'   console viewing. If FALSE, returns the data.frame silently.
#' @return A data.frame with columns: Category, Convention, Example,
#'   Description. Returned invisibly when print = TRUE.
#' @export
#' @examples
#' # Show all conventions
#' darth_reference()
#'
#' # Show only naming prefixes
#' darth_reference("prefixes")
#'
#' # Get as data.frame for programmatic use
#' df <- darth_reference("all", print = FALSE)
darth_reference <- function(category = c("all", "prefixes", "objects",
                                          "functions", "parameters",
                                          "model_structure"),
                             print = TRUE) {
  category <- match.arg(category)

  rows <- list()
  add <- function(cat, convention, example, desc) {
    rows[[length(rows) + 1L]] <<- data.frame(
      Category = cat, Convention = convention,
      Example = example, Description = desc,
      stringsAsFactors = FALSE
    )
  }

  # =========================================================================
  # 1. VARIABLE NAMING PREFIXES
  # =========================================================================
  add("Prefixes", "n_*", "n_states, n_cycles, n_strategies",
      "Scalar integer: counts, dimensions")
  add("Prefixes", "v_*", "v_names_states, v_costs, v_utilities",
      "Vector (1D array)")
  add("Prefixes", "m_*", "m_P, m_TR, m_M",
      "Matrix (2D array)")
  add("Prefixes", "a_*", "a_P, a_TR",
      "Array (3D+, e.g., time-varying transition matrices)")
  add("Prefixes", "l_*", "l_params_all, l_results",
      "List (heterogeneous container)")
  add("Prefixes", "df_*", "df_psa, df_cea",
      "Data frame")
  add("Prefixes", "p_*", "p_HD, p_S1S2",
      "Probability (transition probability)")
  add("Prefixes", "r_*", "r_HD, r_S1D",
      "Rate (instantaneous hazard rate)")
  add("Prefixes", "hr_*", "hr_S1, hr_S2",
      "Hazard ratio")
  add("Prefixes", "rr_*", "rr_prog, rr_death",
      "Relative risk")
  add("Prefixes", "or_*", "or_treatment",
      "Odds ratio")
  add("Prefixes", "u_*", "u_H, u_S1, u_S2",
      "Utility (health-state utility value)")
  add("Prefixes", "du_*", "du_AE, du_treatment",
      "Disutility (decrement to utility)")
  add("Prefixes", "c_*", "c_H, c_S1, c_drug",
      "Cost (per cycle or per event)")
  add("Prefixes", "ic_*", "ic_direct, ic_indirect",
      "Incremental cost")
  add("Prefixes", "d_*", "d_c, d_e",
      "Discount rate (for costs d_c, for effects d_e)")
  add("Prefixes", "wtp_*", "wtp",
      "Willingness-to-pay threshold")

  # =========================================================================
  # 2. STANDARD OBJECT NAMES
  # =========================================================================
  add("Objects", "v_names_states", 'c("H", "S1", "S2", "D")',
      "Character vector of health state names")
  add("Objects", "n_states", "length(v_names_states)",
      "Number of health states")
  add("Objects", "n_cycles", "n_cycles <- 40",
      "Number of model cycles (time horizon / cycle length)")
  add("Objects", "n_strategies", "n_strategies <- 3",
      "Number of decision alternatives")
  add("Objects", "v_names_strategies", 'c("SoC", "TrtA", "TrtB")',
      "Character vector of strategy labels")
  add("Objects", "d_c / d_e", "d_c <- 0.03; d_e <- 0.03",
      "Annual discount rates for costs (d_c) and effects (d_e)")
  add("Objects", "v_dwc / v_dwe", "v_dwc <- 1/(1+d_c)^(0:(n_cycles-1))",
      "Discount weight vectors for costs and effects")
  add("Objects", "m_M", "m_M <- matrix(0, nrow=n_cycles+1, ncol=n_states)",
      "Markov trace matrix (rows=cycles, cols=states)")
  add("Objects", "m_P", "m_P <- matrix(0, nrow=n_states, ncol=n_states)",
      "Transition probability matrix (time-homogeneous)")
  add("Objects", "a_P", "a_P <- array(0, dim=c(n_states, n_states, n_cycles))",
      "Time-varying transition probability array (3D)")
  add("Objects", "a_TR", "a_TR <- array(0, dim=c(n_cycles+1, n_states, n_strategies))",
      "Markov trace array (one trace per strategy)")
  add("Objects", "l_params_all", "l_params_all <- list(p_HD=0.002, ...)",
      "Master parameter list (all model inputs)")

  # =========================================================================
  # 3. FUNCTION NAMING CONVENTIONS
  # =========================================================================
  add("Functions", "f_gen_psa()", "f_gen_psa(n_sim, l_params_all)",
      "Generate PSA input dataset (sample from distributions)")
  add("Functions", "f_update_params()", "f_update_params(l_params_all, params_updated)",
      "Update the base-case parameter list with new values")
  add("Functions", "f_decision_model() / f_MM_*", "f_MM_SickSicker(l_params_all)",
      "Core decision-model function: takes params, returns outcomes")
  add("Functions", "f_calculate_ce_out()", "f_calculate_ce_out(l_params_all)",
      "Wrapper: runs model + computes costs, QALYs, and CEA table")
  add("Functions", "f_wrapper()", "f_wrapper(l_params_all, wtp)",
      "Top-level wrapper: runs model + CEA + returns NMB")

  # =========================================================================
  # 4. PARAMETER TABLE CONVENTIONS
  # =========================================================================
  add("Parameters", "Transition probabilities", "p_HD <- 0.002",
      "Monthly/annual prob. Prefix p_XY = prob from state X to state Y")
  add("Parameters", "Rate-to-probability", "p <- 1 - exp(-r * t)",
      "Convert rate r to probability over time t: p = 1 - exp(-r*t)")
  add("Parameters", "Probability-to-rate", "r <- -log(1 - p) / t",
      "Convert probability p over time t to rate: r = -log(1-p)/t")
  add("Parameters", "Hazard ratio application", "p_trt <- 1 - (1-p_base)^HR",
      "Apply HR to a probability (via rate conversion)")
  add("Parameters", "Complement probability", "p_stay <- 1 - p_out1 - p_out2",
      "Staying probability = 1 minus all outgoing probabilities")
  add("Parameters", "Cost vectors", "v_c_states <- c(c_H, c_S1, c_S2, 0)",
      "Per-cycle cost by state (0 for dead)")
  add("Parameters", "Utility vectors", "v_u_states <- c(u_H, u_S1, u_S2, 0)",
      "Per-cycle utility by state (0 for dead)")

  # =========================================================================
  # 5. MODEL STRUCTURE PATTERNS
  # =========================================================================
  add("Model structure", "Cohort trace computation",
      "m_M[t+1, ] <- m_M[t, ] %*% m_P",
      "Matrix multiplication: next cycle = current state x transition matrix")
  add("Model structure", "Total costs",
      "tc <- m_M[, -1] %*% v_c_states * v_dwc",
      "Trace (excl. cycle 0) x cost vector x discount weights")
  add("Model structure", "Total QALYs",
      "te <- m_M[, -1] %*% v_u_states * v_dwe * cycle_length",
      "Trace x utility vector x discount weights x cycle length")
  add("Model structure", "Half-cycle correction",
      "tc_hcc <- (head(tc, -1) + tail(tc, -1)) / 2",
      "Trapezoidal: average of consecutive cycle payoffs")
  add("Model structure", "CEA table",
      "df_cea <- calculate_icers(cost=v_tc, effect=v_te, strategies=v_names)",
      "Function from dampack package: identifies dominated strategies")
  add("Model structure", "ICER calculation",
      "ICER <- diff(v_tc) / diff(v_te)",
      "Incremental cost / incremental effect along the frontier")
  add("Model structure", "NMB",
      "NMB <- wtp * v_te - v_tc",
      "Net monetary benefit = WTP * QALYs - Costs")
  add("Model structure", "PSA loop",
      "for(i in 1:n_sim) { l_psa[[i]] <- f_wrapper(df_psa[i,]) }",
      "Outer loop: sample params, run model, store results")
  add("Model structure", "Tornado (one-way DSA)",
      "for(p in params) { vary p from low to high, record ICER }",
      "Vary one parameter at a time, hold others at base case")
  add("Model structure", "Partitioned survival",
      "PF <- S_pfs; Prog <- S_os - S_pfs; Dead <- 1 - S_os",
      "Area under survival curves defines state occupancy")
  add("Model structure", "Weibull S(t)",
      "S(t) = exp(-(t/lambda)^gamma)",
      "Weibull survivor function with shape gamma, scale lambda")
  add("Model structure", "Proportional hazards",
      "S_trt(t) = S_base(t)^HR",
      "Treatment survival = baseline survival raised to HR power")

  # =========================================================================
  # 6. ShinyCEA MAPPING
  # =========================================================================
  add("ShinyCEA mapping", "l_params_all", "pars argument in simulate(pars, strategy)",
      "DARTH master param list = ShinyCEA simulate() pars argument")
  add("ShinyCEA mapping", "f_MM_*()", "simulate = function(pars, strategy) {...}",
      "DARTH model function = ShinyCEA simulate() closure")
  add("ShinyCEA mapping", "f_gen_psa()", "sample_psa() / sample_param_row()",
      "DARTH PSA generator = ShinyCEA sample_psa() or run_psa()")
  add("ShinyCEA mapping", "df_cea", "run_deterministic()$table",
      "DARTH CEA table = ShinyCEA deterministic results table")
  add("ShinyCEA mapping", "calculate_icers()", "frontier_analysis()",
      "DARTH dampack ICER function = ShinyCEA frontier_analysis()")
  add("ShinyCEA mapping", "f_wrapper()", "run_deterministic() / run_psa()",
      "DARTH top-level wrapper = ShinyCEA analysis pipeline")
  add("ShinyCEA mapping", "cea_param()", "pr() helper in DARTH examples",
      "ShinyCEA parameter row builder = DARTH's tibble row helper")

  df <- do.call(rbind, rows)
  rownames(df) <- NULL

  if (category != "all") {
    cat_map <- c(
      prefixes = "Prefixes",
      objects = "Objects",
      functions = "Functions",
      parameters = "Parameters",
      model_structure = "Model structure"
    )
    if (category %in% names(cat_map)) {
      df <- df[df$Category == cat_map[[category]], ]
    }
  }

  if (print) {
    cat("=== DARTH Coding Framework Reference ===\n")
    cat("Source: Alarid-Escudero et al. (2019), PharmacoEconomics 37:1329-1339\n\n")

    for (cat in unique(df$Category)) {
      cat(paste0("--- ", cat, " ---\n"))
      sub <- df[df$Category == cat, ]
      max_conv <- max(nchar(sub$Convention))
      max_ex <- min(max(nchar(sub$Example)), 45)
      for (i in seq_len(nrow(sub))) {
        cat(sprintf("  %-*s  %-*s  %s\n",
                    max_conv, sub$Convention[i],
                    max_ex, sub$Example[i],
                    sub$Description[i]))
      }
      cat("\n")
    }
    invisible(df)
  } else {
    df
  }
}
