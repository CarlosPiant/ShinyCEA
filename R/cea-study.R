# Study details of a cost-effectiveness analysis.
#
# The analyses of the package describe WHAT the results are; a `cea_study`
# records the context a reader needs to interpret them (who, where, from whose
# perspective, which costs and outcomes, which assumptions). These are the
# items the CHEERS 2022 reporting checklist asks for; see R/cea-cheers.R.

# One row per study field. This table is the single definition used by the
# constructor, the Study tab of the app, the reports and the checklist, so the
# four cannot drift apart. `item` is the CHEERS 2022 item the field answers
# (NA when it is not a checklist item); `long` fields get a multi-line input.
.study_fields <- data.frame(
  field = c("author", "objective", "analysis_plan", "population", "setting",
            "perspective", "time_horizon", "discount_rate", "outcome",
            "outcome_measurement", "outcome_valuation", "costing", "currency",
            "price_year", "currency_conversion", "model_rationale", "assumptions",
            "engagement", "funding", "conflicts"),
  label = c("Author(s)", "Background and objectives", "Health economic analysis plan",
            "Study population", "Setting and location", "Perspective", "Time horizon",
            "Discount rate", "Outcomes used as measure of benefit",
            "Measurement of outcomes", "Valuation of outcomes",
            "Measurement and valuation of resources and costs", "Currency",
            "Price year", "Currency conversion and inflation",
            "Rationale and description of the model", "Assumptions (one per line)",
            "Engagement with patients and others", "Source of funding",
            "Conflicts of interest"),
  item = c(NA, 3, 4, 5, 6, 8, 9, 10, 11, 12, 13, 14, 15, 15, 15, 16, 17, 21, 27, 28),
  long = c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE,
           FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE),
  hint = c("", "Decision problem and why it matters",
           "Whether a plan was written and where it is available",
           "Age, sex, disease and stage of the modelled people",
           "Country, health system, care setting", "E.g. health-care payer, societal",
           "E.g. 10 years, lifetime, and why", "Rates for costs and outcomes, and why",
           "E.g. QALYs", "How health effects were measured (e.g. EQ-5D-5L in a trial)",
           "Value set and population used to value outcomes",
           "Which resources are counted and how unit costs were obtained",
           "E.g. USD", "E.g. 2024", "How costs were inflated or converted",
           "Why this model structure, and where the model is available",
           "Structural and analytic assumptions",
           "Who was involved in the design and how", "", ""),
  stringsAsFactors = FALSE
)

# Text of a study field cleaned for storage: character, trimmed, NA and empty
# entries removed. Returns character(0) when nothing is left.
.study_text <- function(x) {
  if (is.null(x)) return(character(0))
  x <- unlist(x, use.names = TRUE)
  nms <- names(x)
  x <- trimws(as.character(x))        # trimws() drops names: put them back
  names(x) <- nms
  x[!is.na(x) & nzchar(x)]
}

#' Study Details of a Cost-Effectiveness Analysis
#'
#' Records the context of an analysis: the items of the CHEERS 2022 reporting
#' checklist that results alone cannot supply. Pass the result to
#' [new_cea_model()] (`study = `); the app shows it in the Study tab, where it
#' can be completed, and reports print it. All fields are optional; see
#' [cheers_checklist()] for what is still missing.
#'
#' @param author Author(s) of the analysis.
#' @param objective Background and objectives: the decision problem.
#' @param analysis_plan Whether a health economic analysis plan was developed
#'   and where it is available.
#' @param population Study population (age, sex, disease, stage).
#' @param setting Setting and location (country, health system, care setting).
#' @param comparators Named character vector describing each strategy, named
#'   by strategy (e.g. `c(std = "Standard care", t1 = "Drug A, 10 mg daily")`).
#' @param perspective Perspective of the analysis (e.g. health-care payer).
#' @param time_horizon Time horizon and why it is appropriate.
#' @param discount_rate Discount rate(s) for costs and outcomes, with rationale.
#' @param outcome Outcome(s) used as the measure of benefit (e.g. QALYs).
#' @param outcome_measurement How the outcomes were measured.
#' @param outcome_valuation Population and methods used to value outcomes
#'   (e.g. instrument and value set).
#' @param costing How resources were measured and costs valued.
#' @param currency Currency of the costs (e.g. "USD").
#' @param price_year Price year of the costs.
#' @param currency_conversion How costs were inflated or converted, if they were.
#' @param model_rationale Rationale for and description of the model, and
#'   where it is available.
#' @param assumptions Character vector of assumptions, one element each.
#' @param engagement Approach to engagement with patients and others affected.
#' @param funding Source of funding and the funder's role.
#' @param conflicts Conflicts of interest.
#' @return A `cea_study` object: a list holding the fields that were filled.
#' @references Husereau D, Drummond M, Augustovski F, et al. Consolidated
#'   Health Economic Evaluation Reporting Standards 2022 (CHEERS 2022)
#'   statement. *Value in Health*. 2022;25(1):3-9.
#' @export
#' @examples
#' study <- cea_study(
#'   population = "Adults aged 50 with early disease",
#'   setting = "United States, outpatient care",
#'   perspective = "Health-care payer",
#'   comparators = c(std = "Standard care", t1 = "Treatment 1"),
#'   currency = "USD", price_year = "2024",
#'   assumptions = c("Constant transition probabilities", "No treatment waning")
#' )
#' study
cea_study <- function(author = NULL, objective = NULL, analysis_plan = NULL,
                      population = NULL, setting = NULL, comparators = NULL,
                      perspective = NULL, time_horizon = NULL, discount_rate = NULL,
                      outcome = NULL, outcome_measurement = NULL,
                      outcome_valuation = NULL, costing = NULL, currency = NULL,
                      price_year = NULL, currency_conversion = NULL,
                      model_rationale = NULL, assumptions = NULL, engagement = NULL,
                      funding = NULL, conflicts = NULL) {
  args <- as.list(environment())
  out <- list()
  for (f in .study_fields$field) {
    v <- unname(.study_text(args[[f]]))
    if (!length(v)) next
    if (f != "assumptions" && length(v) > 1) {
      stop("`", f, "` must be a single string.", call. = FALSE)
    }
    out[[f]] <- v
  }
  comp <- .study_text(comparators)
  if (length(comp)) {
    if (is.null(names(comp)) || any(!nzchar(names(comp)))) {
      stop("`comparators` must be a character vector named by strategy.", call. = FALSE)
    }
    out$comparators <- comp
  }
  structure(out, class = "cea_study")
}

#' @export
print.cea_study <- function(x, ...) {
  tab <- .study_table(x)
  cat("CEA study details:", length(x), "field(s) filled\n")
  for (i in seq_len(nrow(tab))) cat("  ", tab$Field[i], ": ", tab$Value[i], "\n", sep = "")
  invisible(x)
}

# A study (or NULL, or a plain list from the app) as a cea_study object.
.as_study <- function(x) {
  if (inherits(x, "cea_study")) return(x)
  if (is.null(x)) return(cea_study())
  do.call(cea_study, x[intersect(names(x), c(.study_fields$field, "comparators"))])
}

# Is a field filled?
.study_has <- function(study, field) length(study[[field]]) > 0

# The filled fields as a two-column table (Field, Value) in checklist order,
# for printing and for the reports. Strategy descriptions come after the
# perspective, one row each; assumptions are joined with "; ".
.study_table <- function(study) {
  study <- .as_study(study)
  rows <- list()
  add <- function(field, value) {
    rows[[length(rows) + 1L]] <<- data.frame(Field = field, Value = value,
                                             stringsAsFactors = FALSE)
  }
  for (i in seq_len(nrow(.study_fields))) {
    f <- .study_fields$field[i]
    if (.study_has(study, f)) {
      label <- sub(" \\(one per line\\)$", "", .study_fields$label[i])
      add(label, paste(study[[f]], collapse = "; "))
    }
    if (f == "setting" && .study_has(study, "comparators")) {
      comp <- study$comparators
      for (s in names(comp)) add(paste0("Strategy: ", s), comp[[s]])
    }
  }
  if (!length(rows)) {
    return(data.frame(Field = character(0), Value = character(0), stringsAsFactors = FALSE))
  }
  do.call(rbind, rows)
}

# ---- Model diagram -------------------------------------------------------------

# What kind of diagram a model carries: "none", "file" (path to an image),
# "function" (draws on the current graphics device) or "ggplot".
.diagram_kind <- function(diagram) {
  if (is.null(diagram)) return("none")
  if (is.function(diagram)) return("function")
  if (inherits(diagram, "ggplot")) return("ggplot")
  if (is.character(diagram) && length(diagram) == 1 && !is.na(diagram) && nzchar(diagram)) {
    return("file")
  }
  stop("`diagram` must be a path to an image file, a function that draws the diagram, ",
       "or a ggplot object.", call. = FALSE)
}

# Height in inches of a drawn diagram that is `width` inches wide. A drawing
# function can state its preferred height (at 8 inches wide) in the attribute
# "diagram_height"; otherwise a 16:10 shape is used.
.diagram_height <- function(diagram, width = 8) {
  h <- attr(diagram, "diagram_height")
  if (is.numeric(h) && length(h) == 1 && is.finite(h) && h > 0) h * width / 8 else width * 0.625
}

# Draw a "function" or "ggplot" diagram on the current device.
.draw_diagram <- function(diagram) {
  if (is.function(diagram)) diagram() else print(diagram)
  invisible(NULL)
}

# Save the model diagram as a PNG at `file`, for the reports and the download
# button. Returns TRUE when a PNG was written. Image files that are not PNG
# cannot be converted without extra packages, so they are skipped.
.cea_diagram_png <- function(model, file, width = 8) {
  d <- model$diagram
  kind <- .diagram_kind(d)
  if (kind == "none") return(FALSE)
  if (kind == "file") {
    if (!file.exists(d) || !grepl("\\.png$", d, ignore.case = TRUE)) return(FALSE)
    return(isTRUE(file.copy(d, file, overwrite = TRUE)))
  }
  grDevices::png(file, width = width, height = .diagram_height(d, width),
                 units = "in", res = 150)
  ok <- tryCatch({ .draw_diagram(d); TRUE }, error = function(e) FALSE)
  grDevices::dev.off()
  if (!ok) unlink(file)
  ok
}

# A simple state-transition diagram in base graphics, used by the bundled
# example models. `states`: data frame with id, label, x, y (any scale);
# `edges`: data frame with from, to. Returns a drawing function for
# new_cea_model(diagram = ).
.state_diagram <- function(states, edges, title = NULL, height = 4.2) {
  draw <- function() {
    navy <- "#18213d"; cream <- "#f9f2e5"; teal <- "#1aa3a1"   # tokens of shinycea.css
    op <- graphics::par(mar = c(0.5, 0.5, if (is.null(title)) 0.5 else 2, 0.5))
    on.exit(graphics::par(op))
    w <- 0.9; h <- 0.42                                         # half-size of a state box
    graphics::plot.new()
    graphics::plot.window(xlim = range(states$x) + c(-1.2, 1.2),
                          ylim = range(states$y) + c(-0.8, 0.8), asp = NA)
    if (!is.null(title)) graphics::title(main = title, col.main = navy, cex.main = 1.1)
    pos <- function(id) states[match(id, states$id), c("x", "y")]
    for (i in seq_len(nrow(edges))) {
      a <- pos(edges$from[i]); b <- pos(edges$to[i])
      # Shorten each arrow so that it starts and ends at the box borders: the
      # box is crossed at the smaller of the two scale factors
      dx <- b$x - a$x; dy <- b$y - a$y
      k <- min(if (dx != 0) w / abs(dx) else Inf, if (dy != 0) h / abs(dy) else Inf)
      graphics::arrows(a$x + dx * k, a$y + dy * k, b$x - dx * k, b$y - dy * k,
                       length = 0.1, lwd = 1.6, col = teal)
    }
    graphics::rect(states$x - w, states$y - h, states$x + w, states$y + h,
                   col = cream, border = navy, lwd = 2)
    graphics::text(states$x, states$y, states$label, col = navy, font = 2, cex = 1)
  }
  attr(draw, "diagram_height") <- height
  draw
}
