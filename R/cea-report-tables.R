#' Format Base-Case Results Table
#'
#' Creates a publication-ready base-case results table from deterministic
#' analysis output.
#'
#' @param det_result Output from [run_deterministic()].
#' @param currency_symbol Currency symbol. Default "$".
#' @return A data.frame formatted for reporting.
#' @export
table_base_case <- function(det_result, currency_symbol = "$") {
  tab <- det_result$table
  data.frame(
    Strategy = tab$Strategy,
    Cost = paste0(currency_symbol, formatC(round(tab$Cost), format = "f",
                                            big.mark = ",", digits = 0)),
    QALY = round(tab$QALY, 3),
    NMB = paste0(currency_symbol, formatC(round(tab$NMB), format = "f",
                                           big.mark = ",", digits = 0)),
    stringsAsFactors = FALSE
  )
}

#' Format Incremental Results Table
#'
#' Creates a publication-ready incremental comparison table showing each
#' strategy versus the reference.
#'
#' @param det_result Output from [run_deterministic()].
#' @param currency_symbol Currency symbol. Default "$".
#' @return A data.frame formatted for reporting.
#' @export
table_incremental <- function(det_result, currency_symbol = "$") {
  tab <- det_result$table
  ref <- det_result$ref

  fmt_c <- function(x) {
    ifelse(is.finite(x),
           paste0(currency_symbol, formatC(round(x), format = "f",
                                            big.mark = ",", digits = 0)),
           "---")
  }

  data.frame(
    Strategy = tab$Strategy,
    Reference = ref,
    Inc_Cost = fmt_c(tab$IncCost_vsRef),
    Inc_QALY = ifelse(is.finite(tab$IncQALY_vsRef),
                      round(tab$IncQALY_vsRef, 4), "---"),
    ICER = ifelse(is.finite(tab$ICER_vsRef),
                  paste0(currency_symbol, formatC(round(tab$ICER_vsRef),
                                                   format = "f", big.mark = ",",
                                                   digits = 0), "/QALY"),
                  "---"),
    stringsAsFactors = FALSE
  )
}

#' Format PSA Summary Table
#'
#' Creates a publication-ready PSA summary with mean and 95% credible
#' intervals for costs, QALYs, and NMB.
#'
#' @param psa_results Data.frame from [run_psa()]$results.
#' @param strategies Character vector of strategy names.
#' @param wtp WTP threshold.
#' @return A data.frame with Strategy, Cost, QALY, NMB columns (formatted).
#' @export
table_psa_summary <- function(psa_results, strategies, wtp = 100000) {
  build_psa_summary(psa_results, strategies, wtp)
}
