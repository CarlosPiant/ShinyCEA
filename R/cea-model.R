#' Create a CEA Model
#'
#' @param info A function returning list(key, name, strategies).
#' @param params A function returning a tibble of default parameters.
#' @param simulate A function(pars, strategy) returning list(cost, qaly, trace?, survival?).
#' @param diagram Optional path to a diagram image file (PNG/SVG).
#' @param description Optional character string or path to a markdown file describing the model.
#' @return A validated `cea_model` object.
#' @export
new_cea_model <- function(info, params, simulate, diagram = NULL, description = NULL) {
  stopifnot(is.function(info), is.function(params), is.function(simulate))

  model <- structure(
    list(
      info = info,
      default_params = params,
      simulate = simulate,
      diagram = diagram,
      description = description
    ),
    class = "cea_model"
  )

  validate_cea_model(model)
  model
}

#' Validate a CEA Model Object
#'
#' @param model A `cea_model` object.
#' @return Invisible TRUE if valid; errors otherwise.
#' @export
validate_cea_model <- function(model) {
  if (!inherits(model, "cea_model")) {
    stop("`model` must be a 'cea_model' object created with new_cea_model().", call. = FALSE)
  }

  info <- tryCatch(model$info(), error = function(e) {
    stop("model$info() failed: ", conditionMessage(e), call. = FALSE)
  })

  required_info <- c("key", "name", "strategies")
  missing_info <- setdiff(required_info, names(info))
  if (length(missing_info)) {
    stop("model$info() is missing: ", paste(missing_info, collapse = ", "), call. = FALSE)
  }
  if (!is.character(info$strategies) || length(info$strategies) < 2) {
    stop("model$info()$strategies must be a character vector with >= 2 strategies.", call. = FALSE)
  }

  df <- tryCatch(model$default_params(), error = function(e) {
    stop("model$default_params() failed: ", conditionMessage(e), call. = FALSE)
  })
  if (!is.data.frame(df) || nrow(df) == 0) {
    stop("model$default_params() must return a non-empty data.frame.", call. = FALSE)
  }

  required_cols <- c("name", "mean", "lower", "upper", "base", "distribution")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols)) {
    stop("default_params() is missing columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  pars <- stats::setNames(as.list(df$base), df$name)
  strat1 <- info$strategies[1]
  res <- tryCatch(model$simulate(pars, strat1), error = function(e) {
    stop("model$simulate() failed for strategy '", strat1, "': ",
         conditionMessage(e), call. = FALSE)
  })

  if (!is.list(res) || !all(c("cost", "qaly") %in% names(res))) {
    stop("model$simulate() must return a list with at least 'cost' and 'qaly'.", call. = FALSE)
  }
  if (!is.finite(as.numeric(res$cost)) || !is.finite(as.numeric(res$qaly))) {
    stop("model$simulate() returned non-finite cost or qaly for strategy '", strat1, "'.", call. = FALSE)
  }

  invisible(TRUE)
}

#' @export
print.cea_model <- function(x, ...) {
  info <- x$info()
  cat("CEA Model:", info$name, "\n")
  cat("  Key:", info$key, "\n")
  cat("  Strategies:", paste(info$strategies, collapse = ", "), "\n")
  df <- x$default_params()
  cat("  Parameters:", nrow(df), "\n")
  if (!is.null(x$diagram)) cat("  Diagram:", x$diagram, "\n")
  invisible(x)
}

#' Helper to Build a Parameter Row
#'
#' @param name Parameter name.
#' @param mean Mean value.
#' @param lower Lower bound (for PSA/tornado).
#' @param upper Upper bound (for PSA/tornado).
#' @param base Base-case value (defaults to mean).
#' @param distribution Distribution for PSA sampling.
#' @param include_psa Include in PSA?
#' @param include_tornado Include in tornado?
#' @return A single-row tibble.
#' @export
cea_param <- function(name, mean, lower, upper, base = mean,
                      distribution = "fixed",
                      include_psa = TRUE, include_tornado = TRUE) {
  data.frame(
    name = name, mean = mean, lower = lower, upper = upper, base = base,
    distribution = distribution,
    include_psa = include_psa, include_tornado = include_tornado,
    stringsAsFactors = FALSE
  )
}
