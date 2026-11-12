#' Generate a CEA Report (Word or PDF)
#'
#' Renders a structured report from deterministic and/or PSA results.
#' The report includes all available tables and figures.
#'
#' @param output_file Path for the output file (e.g., "report.docx" or "report.pdf").
#' @param format One of "word" or "pdf".
#' @param model A `cea_model` object.
#' @param det_result Output from [run_deterministic()], or NULL.
#' @param psa_result Output from [run_psa()], or NULL.
#' @param wtp WTP threshold used in the analysis.
#' @param plots A named list of ggplot objects to include. Recognized names:
#'   tornado, ce_plane, ceac, frontier, survival, traces, threshold.
#' @param psa_summary_df Optional data.frame of PSA summary statistics.
#' @param study Study details ([cea_study()]) printed in a "Study details"
#'   section; defaults to those stored in the model. The model diagram, when
#'   the model has one, is printed with them.
#' @param params_df Optional parameter table, printed as an appendix with its
#'   `source` and `note` columns.
#' @param checklist Optional output of [cheers_checklist()], printed as an
#'   appendix.
#' @param quiet Logical; suppress rmarkdown rendering messages?
#' @return The path to the rendered file (invisibly).
#' @export
generate_cea_report <- function(output_file,
                                format = c("word", "pdf"),
                                model,
                                det_result = NULL,
                                psa_result = NULL,
                                wtp = 100000,
                                plots = list(),
                                psa_summary_df = NULL,
                                study = model$study,
                                params_df = NULL,
                                checklist = NULL,
                                quiet = TRUE) {
  format <- match.arg(format)

  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("The 'rmarkdown' package is required to generate reports. ",
         "Install it with install.packages('rmarkdown').", call. = FALSE)
  }

  info <- model$info()
  tmpdir <- tempfile("cea_report_")
  dir.create(tmpdir, recursive = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  # --- Write CSV tables ---
  det_table_csv <- ""
  frontier_csv <- ""
  icers_csv <- ""
  psa_summary_csv <- ""

  if (!is.null(det_result)) {
    det_path <- file.path(tmpdir, "det_table.csv")
    utils::write.csv(det_result$table, det_path, row.names = FALSE)
    det_table_csv <- det_path

    fr_path <- file.path(tmpdir, "frontier.csv")
    utils::write.csv(det_result$frontier, fr_path, row.names = FALSE)
    frontier_csv <- fr_path

    ic_path <- file.path(tmpdir, "icers.csv")
    utils::write.csv(det_result$icers, ic_path, row.names = FALSE)
    icers_csv <- ic_path
  }

  if (!is.null(psa_summary_df)) {
    psa_path <- file.path(tmpdir, "psa_summary.csv")
    utils::write.csv(psa_summary_df, psa_path, row.names = FALSE)
    psa_summary_csv <- psa_path
  }

  # --- Study details, model diagram, parameters and checklist ---
  # Each is optional: an empty path makes the template skip the section.
  write_tab <- function(df, name) {
    if (is.null(df) || !nrow(df)) return("")
    path <- file.path(tmpdir, name)
    utils::write.csv(df, path, row.names = FALSE, na = "")
    path
  }
  study_csv <- write_tab(.study_table(study), "study.csv")
  params_csv <- write_tab(
    if (!is.null(params_df)) {
      params_df[, intersect(c("name", "base", "lower", "upper", "distribution", "source", "note"),
                            names(params_df)), drop = FALSE]
    }, "params.csv")
  checklist_csv <- write_tab(checklist, "checklist.csv")
  diagram_png <- file.path(tmpdir, "diagram.png")
  if (!.cea_diagram_png(model, diagram_png)) diagram_png <- ""

  # --- Save plots as PNG ---
  plot_paths <- list(
    tornado = "", ce_plane = "", ceac = "", frontier = "",
    survival = "", traces = "", threshold = ""
  )

  for (nm in names(plot_paths)) {
    if (!is.null(plots[[nm]]) && inherits(plots[[nm]], "ggplot")) {
      png_path <- file.path(tmpdir, paste0(nm, ".png"))
      ggplot2::ggsave(png_path, plot = plots[[nm]],
                      width = 9, height = 6, dpi = 300, units = "in")
      plot_paths[[nm]] <- png_path
    }
  }

  # --- Render ---
  template <- system.file("rmd", "cea_report.Rmd", package = "ShinyCEA")
  if (!nzchar(template)) {
    stop("Report template not found. Is ShinyCEA installed properly?", call. = FALSE)
  }

  output_format <- if (format == "word") "word_document" else "pdf_document"

  n_psa <- if (!is.null(psa_result)) nrow(psa_result$results) else 0L
  psa_dropped <- if (!is.null(psa_result)) psa_result$dropped else 0L
  ref_label <- if (!is.null(det_result)) det_result$ref else "N/A"

  # Everything is rendered inside tmpdir: the template is copied next to the
  # tables and images, which are referenced by file name, and the output is
  # written there too. With the input, resources and output in one folder,
  # pandoc finds every image whatever rmarkdown does with paths (rmarkdown
  # 2.32 relativised absolute paths and dropped the images).
  rmd <- file.path(tmpdir, "cea_report.Rmd")
  file.copy(template, rmd, overwrite = TRUE)
  rel <- function(path) if (nzchar(path)) basename(path) else ""
  rendered <- rmarkdown::render(
    input = rmd,
    output_format = output_format,
    output_file = paste0("report.", if (format == "word") "docx" else "pdf"),
    output_dir = tmpdir,
    params = list(
      model_name = info$name,
      model_key = info$key,
      strategies = info$strategies,
      wtp = wtp,
      ref_label = ref_label,
      det_table_csv = rel(det_table_csv),
      frontier_csv = rel(frontier_csv),
      icers_csv = rel(icers_csv),
      psa_summary_csv = rel(psa_summary_csv),
      tornado_png = rel(plot_paths$tornado),
      ce_plane_png = rel(plot_paths$ce_plane),
      ceac_png = rel(plot_paths$ceac),
      frontier_png = rel(plot_paths$frontier),
      survival_png = rel(plot_paths$survival),
      traces_png = rel(plot_paths$traces),
      threshold_png = rel(plot_paths$threshold),
      n_psa = n_psa,
      psa_dropped = psa_dropped,
      study_csv = rel(study_csv),
      diagram_png = rel(diagram_png),
      params_csv = rel(params_csv),
      checklist_csv = rel(checklist_csv)
    ),
    envir = new.env(parent = globalenv()),
    quiet = quiet
  )
  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(rendered, output_file, overwrite = TRUE)) {
    stop("Could not write the report to ", output_file, call. = FALSE)
  }

  invisible(normalizePath(output_file, mustWork = FALSE))
}

#' Build PSA Summary Table
#'
#' Creates a formatted summary table from PSA results suitable for
#' report inclusion or display.
#'
#' @param psa_results Data.frame from [run_psa()]$results.
#' @param strategies Character vector of strategy names.
#' @param wtp WTP threshold.
#' @return A data.frame with Strategy, Cost, QALY, NMB columns (formatted strings).
#' @export
build_psa_summary <- function(psa_results, strategies, wtp = 100000) {
  fmt <- function(v) {
    m <- mean(v)
    l <- stats::quantile(v, 0.025)
    u <- stats::quantile(v, 0.975)
    paste0(format(round(m, 0), big.mark = ","),
           " (", format(round(l, 0), big.mark = ","),
           " - ", format(round(u, 0), big.mark = ","), ")")
  }

  summ <- lapply(strategies, function(s) {
    cvec <- psa_results[[paste0(s, ".cost")]]
    qvec <- psa_results[[paste0(s, ".q")]]
    nmb <- wtp * qvec - cvec
    data.frame(
      Strategy = s,
      Cost = fmt(cvec),
      QALY = paste0(round(mean(qvec), 3),
                     " (", round(stats::quantile(qvec, 0.025), 3),
                     " - ", round(stats::quantile(qvec, 0.975), 3), ")"),
      NMB = fmt(nmb),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, summ)
}
