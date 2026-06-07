suppressMessages(library(ShinyCEA))
suppressMessages(library(twig))

# Source the model definition (skip the run_cea_app line)
code <- readLines("scripts/R/asthma_shinyCEA.R")
code <- code[!grepl("^run_cea_app", code)]
eval(parse(text = paste(code, collapse = "\n")), envir = .GlobalEnv)

# Launch via the exported function
run_cea_app(asthma_model)
