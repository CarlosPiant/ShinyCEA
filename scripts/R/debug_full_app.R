# Use devtools::load_all to ensure we get the LATEST source code
devtools::load_all()
library(twig)

code <- readLines("scripts/R/asthma_shinyCEA.R")
code <- code[!grepl("^run_cea_app", code)]
eval(parse(text = paste(code, collapse = "\n")), envir = .GlobalEnv)

# Replicate exactly what run_cea_app does internally
model_list <- list(asthma_gene_therapy = asthma_model)
model_choices <- c("Asthma" = "asthma_gene_therapy")

# Test the server function directly
cat("=== Testing the full server-side flow ===\n\n")

# 1. What current_model() returns
mod <- model_list[[1]]
cat("1. Model:", mod$info()$name, "\n")

# 2. What the params observer does
s <- sanitize_params(mod$default_params())
params_df <- s$df
cat("2. Params:", nrow(params_df), "rows, notes:", paste(s$notes, collapse="; "), "\n")

# 3. What happens when Run Deterministic is clicked
cat("\n3. Calling run_deterministic...\n")
det <- tryCatch(
  run_deterministic(mod, params_df, wtp = 100000),
  error = function(e) {
    cat("   ERROR:", conditionMessage(e), "\n")
    NULL
  }
)

if (!is.null(det)) {
  cat("   SUCCESS\n")
  cat("   Reference:", det$ref, "\n")
  cat("   Strategies:", paste(det$strategies, collapse=", "), "\n")
  print(det$table[, c("Strategy", "Cost", "QALY", "NMB")])

  # 4. Check frontier
  cat("\n4. Frontier status:\n")
  print(det$frontier_status[, c("Strategy", "Cost", "QALY", "Status")])

  # 5. Check what the DT table render would show
  cat("\n5. Table columns:", paste(names(det$table), collapse=", "), "\n")
} else {
  cat("   FAILED - run_deterministic returned NULL\n")
}

# 6. Now launch the actual app
cat("\n=== Launching actual app ===\n")
run_cea_app(asthma_model)
