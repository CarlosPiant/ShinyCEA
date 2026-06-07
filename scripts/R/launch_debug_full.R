devtools::load_all()
library(twig)

code <- readLines("scripts/R/asthma_shinyCEA.R")
code <- code[!grepl("^run_cea_app", code)]
eval(parse(text = paste(code, collapse = "\n")), envir = .GlobalEnv)

options(shiny.fullstacktrace = TRUE)

# Build the app the same way run_cea_app does, but force-launch it
model_list <- list(asthma_gene_therapy = asthma_model)
model_choices <- c("Asthma: Dupilumab vs Gene Therapy" = "asthma_gene_therapy")

ui <- ShinyCEA:::cea_app_ui(model_choices = model_choices)
server <- ShinyCEA:::cea_app_server(model_list)
app <- shiny::shinyApp(ui = ui, server = server)
shiny::runApp(app, port = 7880, launch.browser = FALSE)
