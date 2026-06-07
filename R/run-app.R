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
  dots <- list(...)

  if (!is.null(models)) {
    if (!is.list(models) || !all(vapply(models, inherits, logical(1), "cea_model"))) {
      stop("`models` must be a named list of cea_model objects.", call. = FALSE)
    }
    model_list <- models
  } else if (length(dots) > 0) {
    if (!all(vapply(dots, inherits, logical(1), "cea_model"))) {
      stop("All arguments must be cea_model objects created with new_cea_model().", call. = FALSE)
    }
    nms <- vapply(dots, function(m) m$info()$key, character(1))
    model_list <- stats::setNames(dots, nms)
  } else {
    model_list <- list(
      sick_sicker = cea_sick_sicker(),
      pf_prog_death = cea_pf_prog_death(),
      ps_weibull = cea_partitioned_survival(),
      microsim = cea_microsim()
    )
  }

  if (is.null(names(model_list))) {
    names(model_list) <- vapply(model_list, function(m) m$info()$key, character(1))
  }

  model_choices <- stats::setNames(
    names(model_list),
    vapply(model_list, function(m) m$info()$name, character(1))
  )

  ui <- cea_app_ui(model_choices = model_choices)
  server <- cea_app_server(model_list)

  app <- shiny::shinyApp(ui = ui, server = server)

  if (interactive()) {
    shiny::runApp(app, launch.browser = launch.browser)
  }

  invisible(app)
}
