library(ShinyCEA)
library(twig)

code <- readLines("scripts/R/asthma_shinyCEA.R")
code <- code[!grepl("^run_cea_app", code)]
eval(parse(text = paste(code, collapse = "\n")), envir = .GlobalEnv)

options(shiny.error = function() { traceback(2); cat("\n") })

ui <- shiny::fluidPage(
  shiny::actionButton("run", "Run Test"),
  shiny::verbatimTextOutput("out")
)

server <- function(input, output, session) {
  result <- shiny::eventReactive(input$run, {
    cat("[SERVER] Button clicked\n")

    # Step 1: raw simulate (no safe wrapper)
    params_df <- asthma_model$default_params()
    pars <- setNames(as.list(params_df$base), params_df$name)

    cat("[SERVER] Calling model$simulate directly for DUP...\n")
    r1 <- tryCatch(
      withCallingHandlers(
        asthma_model$simulate(pars, "DUP"),
        warning = function(w) {
          cat("[SERVER] WARNING:", conditionMessage(w), "\n")
          invokeRestart("muffleWarning")
        },
        message = function(m) {
          cat("[SERVER] MESSAGE:", conditionMessage(m))
          invokeRestart("muffleMessage")
        }
      ),
      error = function(e) {
        cat("[SERVER] ERROR:", conditionMessage(e), "\n")
        cat("[SERVER] TRACEBACK:\n")
        traceback(2)
        NULL
      }
    )

    cat("[SERVER] DUP result NULL:", is.null(r1), "\n")
    if (!is.null(r1)) cat("[SERVER] DUP cost:", r1$cost, "\n")

    list(r1 = r1)
  })

  output$out <- shiny::renderPrint({
    r <- result()
    if (is.null(r$r1)) {
      cat("FAILED - check R console\n")
    } else {
      cat("Cost:", r$r1$cost, "\n")
      cat("QALY:", r$r1$qaly, "\n")
    }
  })
}

shiny::runApp(shiny::shinyApp(ui, server), port = 7880, launch.browser = FALSE)
