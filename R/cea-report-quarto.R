#' Create CEA Report Section (Quarto)
#'
#' Generates a Quarto (.qmd) section with deterministic CEA results.
#'
#' @param cea_collected Output from [collect_cea_results()].
#' @param output_dir Directory to save plots. Created if needed.
#' @return Character vector of Quarto markdown lines.
#' @export
create_cea_report <- function(cea_collected, output_dir = "report_figures") {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  lines <- character(0)

  lines <- c(lines, "# Base-Case Results", "")
  lines <- c(lines, .qmd_kable(cea_collected$tables$base_case,
                                "Base-case deterministic results."), "")

  lines <- c(lines, "# Incremental Analysis", "")
  lines <- c(lines, .qmd_kable(cea_collected$tables$incremental,
                                "Incremental results versus reference."), "")

  # Frontier
  lines <- c(lines, "# Cost-Effectiveness Frontier", "")
  if (!is.null(cea_collected$plots$frontier)) {
    fp <- file.path(output_dir, "frontier.png")
    ggplot2::ggsave(fp, cea_collected$plots$frontier, width = 8, height = 6, dpi = 300)
    lines <- c(lines, paste0("![Cost-effectiveness frontier](", fp, ")"), "")
  }

  # Tornado
  if (!is.null(cea_collected$plots$tornado)) {
    lines <- c(lines, "# Tornado Diagram (One-Way DSA)", "")
    fp <- file.path(output_dir, "tornado.png")
    ggplot2::ggsave(fp, cea_collected$plots$tornado, width = 9, height = 6, dpi = 300)
    lines <- c(lines, paste0("![Tornado diagram](", fp, ")"), "")
  }

  # Survival
  if (!is.null(cea_collected$plots$survival)) {
    lines <- c(lines, "# Survival Curves", "")
    fp <- file.path(output_dir, "survival.png")
    ggplot2::ggsave(fp, cea_collected$plots$survival, width = 8, height = 5, dpi = 300)
    lines <- c(lines, paste0("![Survival curves](", fp, ")"), "")
  }

  lines
}

#' Create PSA Appendix Section (Quarto)
#'
#' Generates a Quarto section with all PSA outputs.
#'
#' @param psa_collected Output from [collect_psa_results()].
#' @param output_dir Directory to save plots.
#' @return Character vector of Quarto markdown lines.
#' @export
create_psa_appendix <- function(psa_collected, output_dir = "report_figures") {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  lines <- character(0)

  lines <- c(lines, "# Probabilistic Sensitivity Analysis", "")
  lines <- c(lines, paste0("PSA iterations: ",
                            format(nrow(psa_collected$psa$results), big.mark = ",")), "")

  # Summary table
  lines <- c(lines, "## PSA Summary", "")
  lines <- c(lines, .qmd_kable(psa_collected$tables$psa_summary,
                                "PSA summary: mean (95% credible interval)."), "")

  # CE Plane
  if (!is.null(psa_collected$plots$ce_plane)) {
    lines <- c(lines, "## Cost-Effectiveness Plane", "")
    fp <- file.path(output_dir, "ce_plane.png")
    ggplot2::ggsave(fp, psa_collected$plots$ce_plane, width = 8, height = 6, dpi = 300)
    lines <- c(lines, paste0("![CE plane](", fp, ")"), "")
  }

  # CEAC
  if (!is.null(psa_collected$plots$ceac)) {
    lines <- c(lines, "## Cost-Effectiveness Acceptability Curve", "")
    fp <- file.path(output_dir, "ceac.png")
    ggplot2::ggsave(fp, psa_collected$plots$ceac, width = 8, height = 5, dpi = 300)
    lines <- c(lines, paste0("![CEAC](", fp, ")"), "")
  }

  # Expected Loss
  if (!is.null(psa_collected$plots$expected_loss)) {
    lines <- c(lines, "## Expected Opportunity Loss", "")
    fp <- file.path(output_dir, "expected_loss.png")
    ggplot2::ggsave(fp, psa_collected$plots$expected_loss, width = 8, height = 5, dpi = 300)
    lines <- c(lines, paste0("![Expected loss](", fp, ")"), "")
  }

  # EVPI
  if (!is.null(psa_collected$plots$evpi)) {
    lines <- c(lines, "## Expected Value of Perfect Information", "")
    fp <- file.path(output_dir, "evpi.png")
    ggplot2::ggsave(fp, psa_collected$plots$evpi, width = 8, height = 5, dpi = 300)
    lines <- c(lines, paste0("![EVPI](", fp, ")"), "")
  }

  # EVPPI
  if (!is.null(psa_collected$plots$evppi)) {
    lines <- c(lines, "## Expected Value of Partial Perfect Information", "")
    fp <- file.path(output_dir, "evppi.png")
    ggplot2::ggsave(fp, psa_collected$plots$evppi, width = 8, height = 5, dpi = 300)
    lines <- c(lines, paste0("![EVPPI](", fp, ")"), "")
  }

  lines
}

#' Create BIA Report Section (Quarto)
#'
#' Generates a Quarto section with budget impact analysis results.
#'
#' @param bia_collected Output from [collect_bia_results()].
#' @param output_dir Directory to save plots.
#' @return Character vector of Quarto markdown lines.
#' @export
create_bia_report <- function(bia_collected, output_dir = "report_figures") {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  lines <- character(0)

  lines <- c(lines, "# Budget Impact Analysis", "")

  # Annual table
  lines <- c(lines, "## Annual Budget Impact", "")
  lines <- c(lines, .qmd_kable(bia_collected$tables$annual,
                                "Annual budget impact projection."), "")

  # Plots
  if (!is.null(bia_collected$plots$budget_impact)) {
    fp <- file.path(output_dir, "bia_comparison.png")
    ggplot2::ggsave(fp, bia_collected$plots$budget_impact, width = 8, height = 5, dpi = 300)
    lines <- c(lines, paste0("![Budget impact comparison](", fp, ")"), "")
  }

  if (!is.null(bia_collected$plots$waterfall)) {
    lines <- c(lines, "## Annual Incremental Impact", "")
    fp <- file.path(output_dir, "bia_waterfall.png")
    ggplot2::ggsave(fp, bia_collected$plots$waterfall, width = 8, height = 5, dpi = 300)
    lines <- c(lines, paste0("![Budget impact waterfall](", fp, ")"), "")
  }

  lines
}

#' Render Full HTA Report
#'
#' Master orchestrator: assembles all analysis sections into a single
#' Quarto document and optionally renders it. The generated `.qmd` file
#' is fully editable by the user before final rendering.
#'
#' @param model A `cea_model` object.
#' @param params_df Parameter data.frame (defaults to model's default_params()).
#' @param wtp WTP threshold.
#' @param n_psa Number of PSA iterations. Set to 0 to skip PSA.
#' @param bia_config Optional list with `population`, `current_mix`, `new_mix`,
#'   `years` for BIA. Set to NULL to skip BIA.
#' @param scenarios Optional named list for scenario analysis.
#' @param subgroups Optional named list for subgroup analysis.
#' @param title Report title.
#' @param author Author name.
#' @param format Output format: "html" (default), "pdf", "docx", or "revealjs".
#' @param output_dir Directory for the report files.
#' @param render Logical; if TRUE, render the Quarto document. If FALSE,
#'   only generate the .qmd file for manual editing.
#' @return The path to the generated .qmd file (invisibly).
#' @export
render_hta_report <- function(model, params_df = NULL, wtp = 100000,
                               n_psa = 1000L,
                               bia_config = NULL,
                               scenarios = NULL,
                               subgroups = NULL,
                               title = NULL,
                               author = "",
                               format = c("html", "pdf", "docx", "revealjs"),
                               output_dir = "hta_report",
                               render = FALSE) {
  format <- match.arg(format)
  if (is.null(params_df)) params_df <- model$default_params()
  if (is.null(title)) title <- paste0("CEA Report: ", model$info()$name)

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  fig_dir <- file.path(output_dir, "figures")
  dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

  # --- YAML header ---
  fmt_block <- switch(format,
    "html" = "  html:\n    toc: true\n    toc-depth: 3\n    embed-resources: true\n    theme: cosmo",
    "pdf" = "  pdf:\n    toc: true\n    toc-depth: 3\n    number-sections: true",
    "docx" = "  docx:\n    toc: true\n    toc-depth: 3\n    number-sections: true",
    "revealjs" = "  revealjs:\n    theme: simple\n    slide-number: true\n    transition: fade\n    width: 1200\n    height: 700"
  )

  yaml <- c(
    "---",
    paste0('title: "', title, '"'),
    if (nzchar(author)) paste0('author: "', author, '"'),
    paste0('date: "', Sys.Date(), '"'),
    "format:",
    fmt_block,
    "---",
    ""
  )

  body <- character(0)
  is_slides <- format == "revealjs"
  sep <- if (is_slides) "\n---\n\n" else "\n"

  # --- 1. Model Description ---
  body <- c(body,
    "# Model Description", "",
    paste0("**Model:** ", model$info()$name), "",
    paste0("**Strategies:** ", paste(model$info()$strategies, collapse = ", ")), "",
    paste0("**Parameters:** ", nrow(params_df)), "",
    paste0("**WTP threshold:** $", formatC(wtp, big.mark = ","), "/QALY"), "",
    if (!is.null(model$description)) paste0(model$description, "\n") else "",
    sep
  )

  # --- 2. Deterministic CEA ---
  message("Collecting deterministic results...")
  cea <- collect_cea_results(model, params_df, wtp)
  body <- c(body, create_cea_report(cea, fig_dir), sep)

  # --- 3. PSA ---
  if (n_psa > 0) {
    message("Running PSA (", n_psa, " iterations)...")
    psa <- collect_psa_results(model, params_df, cea$det, wtp, n_psa)
    body <- c(body, create_psa_appendix(psa, fig_dir), sep)
  }

  # --- 4. Scenarios ---
  if (!is.null(scenarios)) {
    message("Running scenario analysis...")
    sc <- run_scenarios(model, params_df, scenarios, wtp)
    body <- c(body,
      "# Scenario Analysis", "",
      .qmd_kable(sc[, c("Scenario", "Strategy", "Cost", "QALY", "NMB")],
                  "Scenario analysis results."), "",
      .qmd_kable(summarize_scenarios(sc),
                  "Optimal strategy per scenario."), sep
    )
  }

  # --- 5. Subgroups ---
  if (!is.null(subgroups)) {
    message("Running subgroup analysis...")
    strategies <- model$info()$strategies
    sg_ref <- strategies[1]
    sg_target <- if (length(strategies) >= 2) strategies[2] else strategies[1]
    sg <- run_subgroups(model, params_df, subgroups,
                        target = sg_target, ref = sg_ref, wtp = wtp)
    body <- c(body,
      "# Subgroup Analysis", "",
      .qmd_kable(sg[, c("Subgroup", "IncCost", "IncQALY", "ICER", "INB")],
                  paste0("Subgroup analysis: ", sg_target, " vs ", sg_ref, ".")),
      "", sep
    )

    fp <- file.path(fig_dir, "subgroup_forest.png")
    p <- plot_subgroup_forest(sg, metric = "ICER", wtp = wtp,
                              palette = cea_palette("classic"))
    ggplot2::ggsave(fp, p, width = 8, height = 5, dpi = 300)
    body <- c(body, paste0("![Subgroup forest plot](", fp, ")"), "", sep)
  }

  # --- 6. BIA ---
  if (!is.null(bia_config)) {
    message("Running budget impact analysis...")
    bia <- do.call(collect_bia_results, c(list(model = model, params_df = params_df),
                                           bia_config))
    body <- c(body, create_bia_report(bia, fig_dir), sep)
  }

  # --- 7. Parameter Table Appendix ---
  body <- c(body,
    "# Appendix: Parameter Table", "",
    .qmd_kable(params_df[, intersect(c("name", "mean", "lower", "upper",
                                         "distribution", "include_psa"),
                                      names(params_df))],
                "Model parameters."), ""
  )

  # --- 8. Validation ---
  message("Running validation...")
  val <- generate_validation_report(model, params_df, wtp)
  body <- c(body,
    "# Appendix: Validation Summary", "",
    paste0("**Validation score:** ", val$score, "/100"), "",
    paste0("PASS: ", val$summary$Count[val$summary$Status == "PASS"],
           " | WARN: ", val$summary$Count[val$summary$Status == "WARN"],
           " | FAIL: ", val$summary$Count[val$summary$Status == "FAIL"]), ""
  )

  # --- Write .qmd ---
  qmd_path <- file.path(output_dir, "report.qmd")
  writeLines(c(yaml, body), qmd_path)
  message("Quarto report generated: ", qmd_path)

  # --- Render if requested ---
  if (render) {
    if (!requireNamespace("quarto", quietly = TRUE)) {
      message("Install the 'quarto' R package to render: install.packages('quarto')")
    } else {
      message("Rendering to ", format, "...")
      quarto::quarto_render(qmd_path)
    }
  }

  invisible(qmd_path)
}

# --- Internal helper: convert data.frame to markdown table ---
.qmd_kable <- function(df, caption = NULL) {
  if (is.null(df) || nrow(df) == 0) return("")

  # Header
  header <- paste0("| ", paste(names(df), collapse = " | "), " |")
  sep_line <- paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|")

  # Rows
  rows <- apply(df, 1, function(row) {
    paste0("| ", paste(as.character(row), collapse = " | "), " |")
  })

  lines <- c(header, sep_line, rows)
  if (!is.null(caption)) {
    lines <- c(lines, "", paste0(": ", caption))
  }
  c(lines, "")
}
