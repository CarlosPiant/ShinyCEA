# CHEERS 2022 reporting checklist for an analysis.
#
# cheers_checklist() says, for each of the 28 items of the checklist, whether
# the information is available from the model, the study details and the
# analyses that were run. It checks that information is PRESENT, not that it
# is adequate: that judgement stays with the analyst and the reviewers.
#
# Topic labels are short descriptions in our own words; the authoritative
# wording of each item is in Husereau et al. (2022).

# Items of the checklist: number, section of a report, short topic.
.cheers_items <- data.frame(
  Item = 1:28,
  Section = c("Title", "Abstract", "Introduction",
              rep("Methods", 18),
              rep("Results", 4),
              "Discussion", "Other", "Other"),
  Topic = c("Title", "Abstract", "Background and objectives",
            "Health economic analysis plan", "Study population", "Setting and location",
            "Comparators", "Perspective", "Time horizon", "Discount rate",
            "Selection of outcomes", "Measurement of outcomes", "Valuation of outcomes",
            "Measurement and valuation of resources and costs",
            "Currency, price date and conversion", "Rationale and description of model",
            "Analytics and assumptions", "Characterizing heterogeneity",
            "Characterizing distributional effects", "Characterizing uncertainty",
            "Approach to engagement with patients and others", "Study parameters",
            "Summary of main results", "Effect of uncertainty",
            "Effect of engagement with patients and others",
            "Findings, limitations, generalizability and current knowledge",
            "Source of funding", "Conflicts of interest"),
  stringsAsFactors = FALSE
)

#' CHEERS 2022 Reporting Checklist for an Analysis
#'
#' Reports, for each of the 28 items of the CHEERS 2022 checklist, whether the
#' information is available from the model, its study details
#' ([cea_study()]), the parameter table and the analyses that were run.
#'
#' The status says whether information is present, not whether it is adequate.
#' "Manuscript" marks items that belong to the written report (title,
#' abstract, discussion) and cannot be supplied by the package.
#'
#' @param model A `cea_model` object.
#' @param study Study details; defaults to those stored in the model.
#' @param params Parameter table; defaults to the model's default parameters.
#'   Item 22 looks at its `source` column.
#' @param analyses Character vector naming the analyses that were run, any of
#'   `"deterministic"`, `"psa"`, `"scenarios"`, `"subgroups"`, `"equity"`.
#' @return A data frame with one row per item and columns `Item`, `Section`,
#'   `Topic`, `Status` (`"Reported"`, `"Partly"`, `"Not reported"` or
#'   `"Manuscript"`) and `Where` (where the information is, or what is missing).
#' @references Husereau D, Drummond M, Augustovski F, et al. Consolidated
#'   Health Economic Evaluation Reporting Standards 2022 (CHEERS 2022)
#'   statement. *Value in Health*. 2022;25(1):3-9.
#' @export
#' @examples
#' m <- cea_sick_sicker()
#' chk <- cheers_checklist(m, analyses = c("deterministic", "psa"))
#' table(chk$Status)
#' chk[chk$Status == "Not reported", c("Item", "Topic", "Where")]
cheers_checklist <- function(model, study = model$study, params = NULL, analyses = NULL) {
  study <- .as_study(study)
  if (is.null(params)) params <- model$default_params()
  analyses <- tolower(as.character(analyses))
  strategies <- model$info()$strategies

  out <- .cheers_items
  out$Status <- "Not reported"
  out$Where <- ""
  set <- function(item, status, where) {
    out$Status[out$Item == item] <<- status
    out$Where[out$Item == item] <<- where
  }

  # Items answered by one or more study fields: Reported when every field
  # that is required for the item is filled.
  required <- .study_fields[!is.na(.study_fields$item) &
                              .study_fields$field != "currency_conversion", ]
  for (it in setdiff(unique(required$item), 16)) {
    f <- required[required$item == it, ]
    has <- vapply(f$field, function(x) .study_has(study, x), logical(1))
    lab <- sub(" \\(one per line\\)$", "", f$label)
    if (all(has)) {
      set(it, "Reported", "Study details")
    } else if (any(has)) {
      set(it, "Partly", paste0("Missing: ", paste(lab[!has], collapse = ", ")))
    } else {
      set(it, "Not reported", paste0("Study details: ", paste(lab, collapse = ", ")))
    }
  }

  # 7. Comparators: every strategy needs a description
  described <- strategies %in% names(study$comparators)
  if (all(described)) {
    set(7, "Reported", "Study details: strategy descriptions")
  } else if (any(described)) {
    set(7, "Partly", paste0("No description for: ", paste(strategies[!described], collapse = ", ")))
  } else {
    set(7, "Not reported", "Study details: describe each strategy")
  }

  # 16. Model: the rationale plus something that describes the structure
  has_rationale <- .study_has(study, "model_rationale")
  has_structure <- !is.null(model$diagram) || !is.null(model$description)
  if (has_rationale && has_structure) {
    set(16, "Reported", "Study details and model diagram/description")
  } else if (has_rationale || has_structure) {
    set(16, "Partly", if (has_rationale) "Missing: model diagram or description"
                      else "Missing: rationale for the model (Study details)")
  } else {
    set(16, "Not reported", "Study details: rationale; model: diagram or description")
  }

  # 18-20, 23-24: analyses that were run
  ran <- function(x) x %in% analyses
  if (ran("subgroups")) set(18, "Reported", "Subgroup analysis") else
    set(18, "Not reported", "Run the subgroup analysis")
  if (ran("equity")) set(19, "Reported", "Distributional (equity) analysis") else
    set(19, "Not reported", "Run the equity analysis")
  if (ran("psa")) {
    set(20, "Reported", "Deterministic and probabilistic sensitivity analyses")
    set(24, "Reported", "PSA results, CEAC, expected loss, value of information")
  } else if (ran("deterministic") || ran("scenarios")) {
    set(20, "Partly", "Deterministic sensitivity only: run the PSA")
    set(24, "Partly", "Tornado/scenarios only: run the PSA")
  } else {
    set(20, "Not reported", "Run the deterministic analysis and the PSA")
    set(24, "Not reported", "Run the deterministic analysis and the PSA")
  }
  if (ran("deterministic")) set(23, "Reported", "Deterministic results and frontier") else
    set(23, "Not reported", "Run the deterministic analysis")

  # 22. Parameters: values, ranges and distributions are always in the table;
  # what can be missing is where each value comes from
  src <- if ("source" %in% names(params)) as.character(params$source) else rep(NA, nrow(params))
  has_src <- !is.na(src) & nzchar(trimws(src))
  if (all(has_src)) {
    set(22, "Reported", "Parameter table with sources")
  } else if (any(has_src)) {
    set(22, "Partly", sprintf("%d of %d parameters have no source", sum(!has_src), length(has_src)))
  } else {
    set(22, "Partly", "Values, ranges and distributions given; no parameter has a source")
  }

  # Items that only the written report can supply
  for (it in c(1, 2, 25, 26)) set(it, "Manuscript", "Written by the authors in the report")

  out
}
