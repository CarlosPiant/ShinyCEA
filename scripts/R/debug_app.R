library(ShinyCEA)
library(twig)

code <- readLines("scripts/R/asthma_shinyCEA.R")
code <- code[!grepl("^run_cea_app", code)]
eval(parse(text = paste(code, collapse = "\n")), envir = .GlobalEnv)

# Minimal debug app — same server logic but with error capture
ui <- shiny::fluidPage(
  shiny::actionButton("run", "Run Deterministic"),
  shiny::verbatimTextOutput("debug_output")
)

server <- function(input, output, session) {
  output$debug_output <- shiny::renderPrint({
    shiny::req(input$run)

    cat("Step 1: Getting params...\n")
    params_df <- sanitize_params(asthma_model$default_params())$df
    cat("  Params OK:", nrow(params_df), "rows\n")

    cat("Step 2: Testing simulate for DUP...\n")
    pars <- setNames(as.list(params_df$base), params_df$name)
    res1 <- safe_simulate(asthma_model, pars, "DUP")
    cat("  DUP result NULL:", is.null(res1), "\n")
    if (!is.null(res1)) cat("  DUP cost:", res1$cost, "qaly:", res1$qaly, "\n")

    cat("Step 3: Testing simulate for GeneTherapy...\n")
    res2 <- safe_simulate(asthma_model, pars, "GeneTherapy")
    cat("  GT result NULL:", is.null(res2), "\n")
    if (!is.null(res2)) cat("  GT cost:", res2$cost, "qaly:", res2$qaly, "\n")

    cat("Step 4: Running run_deterministic...\n")
    det <- tryCatch(
      run_deterministic(asthma_model, params_df, wtp = 100000),
      error = function(e) { cat("  ERROR:", conditionMessage(e), "\n"); NULL }
    )

    if (!is.null(det)) {
      cat("  SUCCESS! Reference:", det$ref, "\n")
      print(det$table[, c("Strategy", "Cost", "QALY", "NMB")])
    }
  })
}

shiny::runApp(shiny::shinyApp(ui, server), port = 7879, launch.browser = FALSE)
