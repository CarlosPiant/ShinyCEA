suppressMessages(library(ShinyCEA))
suppressMessages(library(twig))

# Source everything except run_cea_app
code <- readLines("scripts/R/asthma_shinyCEA.R")
code <- code[!grepl("^run_cea_app", code)]
eval(parse(text = paste(code, collapse = "\n")), envir = .GlobalEnv)

# Deterministic analysis
cat("\n=== Deterministic Results (WTP = $100,000) ===\n")
det <- run_deterministic(asthma_model, asthma_model$default_params(), wtp = 100000)
cat("Reference:", det$ref, "\n\n")
print(det$table[, c("Strategy", "Cost", "QALY", "NMB", "ICER_vsRef")])

cat("\n=== Sanity checks ===\n")
cat("Time horizon: 2 years (24 monthly cycles)\n")
cat("Max possible QALY: 2 * 1.0 =", 2 * 1.0, "\n")
cat("DUP QALY:", round(det$table$QALY[det$table$Strategy == "DUP"], 4), "\n")
cat("GeneTherapy QALY:", round(det$table$QALY[det$table$Strategy == "GeneTherapy"], 4), "\n")
