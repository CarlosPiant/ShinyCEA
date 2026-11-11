#' Launch the ShinyCEA Application
#'
#' Launches an interactive Shiny app for cost-effectiveness analysis.
#' Accepts one or more `cea_model` objects. When multiple models are provided,
#' the app includes a model selector dropdown.
#'
#' @param ... One or more `cea_model` objects (created with [new_cea_model()]).
#'   If none are provided, launches with the bundled example models.
#' @param models Alternatively, a named list of `cea_model` objects.
#' @param launch.browser Logical; passed to [shiny::shinyApp()].
#' @return A Shiny app object (invisibly).
#' @export
#' @examples
#' \dontrun{
#' # Launch with bundled examples
#' run_cea_app()
#'
#' # Launch with a single custom model
#' run_cea_app(my_model)
#'
#' # Launch with multiple models
#' run_cea_app(cea_sick_sicker(), cea_partitioned_survival())
#' }
run_cea_app <- function(..., models = NULL, launch.browser = TRUE) {
  # How each model was written in the call (e.g. "sick_sicker",
  # "cea_sick_sicker()"). The app's "R code" export uses it for the line that
  # creates the model, so it must be captured before the arguments are evaluated.
  dot_exprs <- as.list(substitute(list(...)))[-1]
  models_expr <- substitute(models)
  dots <- list(...)

  if (!is.null(models)) {
    if (!is.list(models) || !all(vapply(models, inherits, logical(1), "cea_model"))) {
      stop("`models` must be a named list of cea_model objects.", call. = FALSE)
    }
    model_list <- models
    if (is.null(names(model_list))) {
      names(model_list) <- vapply(model_list, function(m) m$info()$key, character(1))
    }
    # Elements of a list held in a variable: my_models[["key"]]
    model_code <- if (is.name(models_expr)) {
      paste0(.expr_code(models_expr), "[[", vapply(names(model_list), deparse, character(1)), "]]")
    } else {
      rep(NA_character_, length(model_list))
    }
  } else if (length(dots) > 0) {
    if (!all(vapply(dots, inherits, logical(1), "cea_model"))) {
      stop("All arguments must be cea_model objects created with new_cea_model().", call. = FALSE)
    }
    nms <- vapply(dots, function(m) m$info()$key, character(1))
    model_list <- stats::setNames(dots, nms)
    model_code <- vapply(dot_exprs, .expr_code, character(1))
  } else {
    model_list <- list(
      sick_sicker = cea_sick_sicker(),
      pf_prog_death = cea_pf_prog_death(),
      ps_weibull = cea_partitioned_survival(),
      microsim = cea_microsim()
    )
    model_code <- c("cea_sick_sicker()", "cea_pf_prog_death()",
                    "cea_partitioned_survival()", "cea_microsim()")
  }
  names(model_code) <- names(model_list)

  model_choices <- stats::setNames(
    names(model_list),
    vapply(model_list, function(m) m$info()$name, character(1))
  )

  ui <- cea_app_ui(model_choices = model_choices)
  server <- cea_app_server(model_list, model_code = model_code)

  app <- shiny::shinyApp(ui = ui, server = server)

  if (interactive()) {
    shiny::runApp(app, launch.browser = launch.browser)
  }

  invisible(app)
}

# An unevaluated argument as R code. Only names and calls are usable as code:
# when the call was built programmatically (do.call), the "expression" is the
# model object itself, which has no meaningful source, so NA is returned and
# the exported script asks the user to fill the model in.
.expr_code <- function(expr) {
  if (!is.name(expr) && !is.call(expr)) return(NA_character_)
  paste(deparse(expr, width.cutoff = 500L), collapse = "\n")
}
