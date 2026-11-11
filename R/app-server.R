#' Build the CEA App Server
#' @param models Named list of cea_model objects.
#' @param model_code Optional character vector named like `models`: the R
#'   expression that creates each model (e.g. `"cea_sick_sicker()"`), used by
#'   the "R code" export. `NULL` or `NA` entries give a placeholder line.
#' @return A Shiny server function.
#' @keywords internal
cea_app_server <- function(models, model_code = NULL) {
  function(input, output, session) {
    .nc <- function(a, b) if (is.null(a)) b else a

    # --- Palette ---
    current_palette <- shiny::reactive({
      choice <- .nc(input$palette_choice, "cream")
      if (choice == "custom") {
        strat_colors <- c(
          .nc(input$custom_color_1, "#18213d"),
          .nc(input$custom_color_2, "#1aa3a1"),
          .nc(input$custom_color_3, "#c0652d"),
          .nc(input$custom_color_4, "#2a56a3"),
          .nc(input$custom_color_5, "#2e7d32"),
          .nc(input$custom_color_6, "#c62828"),
          .nc(input$custom_color_7, "#696d74"),
          .nc(input$custom_color_8, "#3f7f8e")
        )
        cea_palette_custom(
          strategies = strat_colors,
          frontier = .nc(input$custom_frontier, "#18213d"),
          dominated = .nc(input$custom_dominated, "#c62828"),
          ext_dominated = .nc(input$custom_ext_dom, "#c0652d")
        )
      } else {
        cea_palette(choice)
      }
    })

    output$palette_preview <- shiny::renderUI({
      pal <- current_palette()
      swatches <- lapply(pal$strategies[1:min(6, length(pal$strategies))], function(col) {
        shiny::tags$div(
          class = "palette-swatch",
          style = paste0("background-color:", col, ";")
        )
      })
      shiny::div(class = "palette-preview", swatches)
    })

    output$palette_demo_plot <- shiny::renderPlot({
      pal <- current_palette()
      n <- min(6, length(pal$strategies))
      df <- data.frame(
        strategy = paste0("Strategy ", seq_len(n)),
        value = seq(n, 1),
        stringsAsFactors = FALSE
      )
      df$strategy <- factor(df$strategy, levels = df$strategy)
      ggplot2::ggplot(df, ggplot2::aes(x = .data$strategy, y = .data$value,
                                        fill = .data$strategy)) +
        ggplot2::geom_col(width = 0.6, alpha = 0.85) +
        ggplot2::scale_fill_manual(values = pal$strategies[seq_len(n)]) +
        ggplot2::labs(x = NULL, y = NULL, title = "Palette preview") +
        theme_cea() +
        ggplot2::theme(legend.position = "none",
                       panel.grid.major.x = ggplot2::element_blank())
    }, bg = "transparent")

    # --- Current model ---
    current_model <- shiny::reactive({
      if (length(models) == 1) {
        models[[1]]
      } else {
        shiny::req(input$model_key)
        models[[input$model_key]]
      }
    })

    # --- Parameters (editable) ---
    params_rv <- shiny::reactiveVal(NULL)

    # Run results. They are stored in reactiveVals (not eventReactives) so
    # that a model switch can clear them and a later edit can mark them stale
    # (review findings E1, E3): results always belong to the model and inputs
    # they were computed from.
    det_rv <- shiny::reactiveVal(NULL)
    psa_rv <- shiny::reactiveVal(NULL)
    psa_state <- shiny::reactiveValues(draws = NULL, dropped = 0L)
    stale <- shiny::reactiveVal(FALSE)
    det_result <- shiny::reactive(det_rv())
    psa_results <- shiny::reactive(psa_rv())

    shiny::observe({
      mod <- current_model()
      s <- sanitize_params(mod$default_params())
      params_rv(s$df)
      if (length(s$notes)) {
        shiny::showNotification(paste(s$notes, collapse = " | "), type = "message", duration = 6)
      }
      # Predefined composites, scenarios and subgroups belong to this model:
      # always replace the stores, so a model without them starts empty (E6).
      wc_store(if (is.list(mod$composites)) mod$composites else list())
      sc_store(if (is.list(mod$scenarios)) mod$scenarios else list())
      sg_store(if (is.list(mod$subgroups)) mod$subgroups else list())
      # Results of the previous model are not comparable: drop them (E1).
      det_rv(NULL); psa_rv(NULL); psa_state$draws <- NULL; psa_state$dropped <- 0L
      stale(FALSE)
    })

    # Any change of the inputs after a run makes the displayed results stale;
    # the banner in the results tabs says so until the user runs again.
    shiny::observeEvent(list(params_rv(), input$wtp, input$ref_mode, input$ref_pinned,
                             input$psa_seed), {
      if (!is.null(det_rv()) || !is.null(psa_rv())) stale(TRUE)
    }, ignoreInit = TRUE)
    stale_banner <- function() {
      if (!isTRUE(stale())) return(NULL)
      shiny::div(class = "cea-stale",
                 "Inputs changed since this run (parameter table, WTP, reference or PSA seed). ",
                 "The results below are for the previous inputs: run again to update them.")
    }

    shiny::observeEvent(input$param_file, {
      shiny::req(input$param_file$datapath)
      df <- utils::read.csv(input$param_file$datapath, stringsAsFactors = FALSE)
      s <- sanitize_params(df)
      params_rv(s$df)
      if (length(s$notes)) {
        shiny::showNotification(paste(s$notes, collapse = " | "), type = "message", duration = 6)
      }
      # The model reads parameters by name and silently uses built-in defaults
      # for names it does not find (E7): say which names are missing.
      expected <- tryCatch(current_model()$default_params()$name, error = function(e) character(0))
      missing_p <- setdiff(expected, s$df$name)
      if (length(missing_p)) {
        shiny::showNotification(
          paste0("The file does not define ", length(missing_p), " parameter(s) of this model (",
                 paste(utils::head(missing_p, 6), collapse = ", "),
                 if (length(missing_p) > 6) ", ..." else "", "); the model will use its built-in ",
                 "defaults for them. See the Health tab."),
          type = "warning", duration = 12)
      }
    })

    output$param_table <- DT::renderDT({
      df <- params_rv()
      # Names are the contract with the model (E7) and the row-number column
      # has no data behind it (E8): neither is editable.
      locked <- c(0L, which(names(df) %in% c("name", "distribution")))
      DT::datatable(df, editable = list(target = "cell", disable = list(columns = locked)),
                    options = list(pageLength = 14, scrollX = TRUE))
    }, server = TRUE)

    proxy <- DT::dataTableProxy("param_table")
    shiny::observeEvent(input$param_table_cell_edit, {
      info <- input$param_table_cell_edit
      df <- params_rv()
      col_name <- names(df)[info$col]
      if (!length(col_name) || is.na(col_name)) return()     # row-number column
      v <- info$value
      reject <- function(msg) {
        shiny::showNotification(msg, type = "error", duration = 6)
        DT::replaceData(proxy, df, resetPaging = FALSE)      # show the stored value again
      }
      if (col_name %in% c("mean", "sd", "lower", "upper", "base")) {
        v <- .parse_numeric(v)$value
        if (!is.finite(v)) return(reject(paste0("'", info$value, "' is not a number.")))
      }
      if (col_name %in% c("include_psa", "include_tornado")) {
        v <- .parse_logical(v)$value
        if (is.na(v)) return(reject(paste0("'", info$value, "' is not TRUE/FALSE.")))
      }
      # Documentation columns are free text; an emptied cell means "no value"
      if (col_name %in% .PARAM_DOC_COLS) {
        v <- trimws(as.character(v))
        if (!nzchar(v)) v <- NA_character_
      }
      # The deterministic run uses `base`, the PSA is centred on `mean` and the
      # tornado on the bounds. When these columns held the same value before
      # the edit they describe one quantity, so they move together (E4).
      if (col_name %in% c("mean", "base")) {
        old <- df[[col_name]][info$row]
        for (linked in c("mean", "lower", "upper", "base")) {
          if (isTRUE(all.equal(df[[linked]][info$row], old))) df[[linked]][info$row] <- v
        }
      }
      df[info$row, info$col] <- v
      params_rv(df)
      DT::replaceData(proxy, df, resetPaging = FALSE)
    })

    output$download_params <- shiny::downloadHandler(
      filename = function() "parameters_current.csv",
      content = function(file) utils::write.csv(params_rv(), file, row.names = FALSE)
    )

    # --- Model Info ---
    # Model name shown in the top bar (filled once a model is selected).
    output$topbar_model <- shiny::renderUI({
      shiny::req(current_model())
      shiny::tagList("Model: ", shiny::tags$b(current_model()$info()$name))
    })

    output$model_info_title <- shiny::renderUI({
      shiny::req(current_model())
      shiny::h3(current_model()$info()$name)
    })

    output$model_info_description <- shiny::renderUI({
      shiny::req(current_model())
      desc <- current_model()$description
      if (is.null(desc)) return(shiny::HTML("<em>No description available.</em>"))
      if (file.exists(desc)) {
        shiny::includeMarkdown(desc)
      } else {
        shiny::p(desc)
      }
    })

    # Model diagram. A model can carry an image file, a function that draws
    # the diagram (e.g. plot(spec) from ShinyModels) or a ggplot; models
    # without one show nothing.
    output$model_diagram_ui <- shiny::renderUI({
      d <- current_model()$diagram
      kind <- .diagram_kind(d)
      if (kind == "none" || (kind == "file" && !file.exists(d))) return(NULL)
      shiny::tagList(
        shiny::h4("Model diagram"),
        if (kind == "file") {
          shiny::imageOutput("model_diagram_img", height = "auto")
        } else {
          # Height follows the diagram's own shape, for a plot about 800 px wide
          shiny::tagList(
            shiny::plotOutput("model_diagram_plot",
                              height = paste0(round(.diagram_height(d, 8) * 100), "px")),
            shiny::downloadButton("download_diagram_png", "Download diagram (PNG)",
                                  class = "btn-outline-secondary btn-sm"))
        },
        shiny::br(), shiny::br()
      )
    })

    output$model_diagram_plot <- shiny::renderPlot({
      # The block is inserted by renderUI, and its first size report can be a
      # few pixels wide: drawing then fails with "figure margins too large".
      # Wait for a real width; the plot redraws when the size arrives.
      w <- session$clientData$output_model_diagram_plot_width
      shiny::req(is.numeric(w), w >= 100)
      .draw_diagram(current_model()$diagram)
    }, bg = "transparent")

    output$model_diagram_img <- shiny::renderImage({
      path <- current_model()$diagram
      ext <- tolower(tools::file_ext(path))
      type <- switch(ext, svg = "image/svg+xml", jpg = , jpeg = "image/jpeg", "image/png")
      list(src = path, contentType = type, alt = "Model diagram",
           style = "max-width: 100%; height: auto;")
    }, deleteFile = FALSE)

    output$download_diagram_png <- shiny::downloadHandler(
      filename = function() paste0(current_model()$info()$key, "_diagram.png"),
      content = function(file) .cea_diagram_png(current_model(), file)
    )

    output$model_param_desc <- DT::renderDT({
      shiny::req(params_rv())
      df <- params_rv()
      DT::datatable(df[, c("name", "distribution", "mean", "lower", "upper", "base",
                            "include_psa", "include_tornado")],
                    rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
    })

    # --- Reference strategy UI ---
    output$ref_pinned_ui <- shiny::renderUI({
      shiny::req(current_model())
      strategies <- current_model()$info()$strategies
      if (identical(input$ref_mode, "pinned")) {
        shiny::selectInput("ref_pinned", "Pinned reference",
                           choices = strategies, selected = strategies[1])
      }
    })

    # --- Health tab ---
    health_df <- shiny::reactive({
      shiny::req(current_model(), params_rv())
      validate_params_and_model(current_model(), params_rv(), wtp = .nc(input$wtp, 100000))
    })

    output$health_badges <- shiny::renderUI({
      df <- health_df()
      n_pass <- sum(df$status == "PASS", na.rm = TRUE)
      n_warn <- sum(df$status == "WARN", na.rm = TRUE)
      n_fail <- sum(df$status == "FAIL", na.rm = TRUE)
      shiny::tagList(
        shiny::span(class = "badge bg-success", paste0("PASS: ", n_pass)),
        shiny::span(class = "badge bg-warning", paste0("WARN: ", n_warn)),
        shiny::span(class = "badge bg-danger", paste0("FAIL: ", n_fail))
      )
    })

    output$health_table <- DT::renderDT({
      df <- health_df()
      status_colors <- c(PASS = "#2e7d32", WARN = "#c0652d", FAIL = "#c62828")  # design tokens
      DT::datatable(df, rownames = FALSE,
                    options = list(pageLength = 12, scrollX = TRUE)) |>
        DT::formatStyle("status",
          color = DT::styleEqual(names(status_colors), rep("white", length(status_colors))),
          backgroundColor = DT::styleEqual(names(status_colors), unname(status_colors)),
          fontWeight = "bold"
        )
    })

    # --- Deterministic results ---
    shiny::observeEvent(input$run_det, {
      wtp <- suppressWarnings(as.numeric(input$wtp))
      if (!is.finite(wtp) || wtp < 0) {
        shiny::showNotification("Enter a willingness-to-pay threshold (>= 0) before running.",
                                type = "error", duration = 6)
        return()
      }
      ref <- if (identical(input$ref_mode, "pinned")) input$ref_pinned else NULL
      res <- tryCatch(run_deterministic(current_model(), params_rv(), wtp = wtp, ref_strategy = ref),
                      error = function(e) e)
      if (inherits(res, "error")) {
        shiny::showNotification(paste("Deterministic run failed:", conditionMessage(res)),
                                type = "error", duration = 12)
        return()
      }
      # Remember the inputs of the run: dependent analyses (tornado, two-way,
      # threshold) must use the WTP and reference of this run, not live inputs.
      res$wtp <- wtp
      res$ref_mode <- input$ref_mode
      res$model_key <- current_model()$info()$key
      det_rv(res)
      stale(FALSE)
    })

    output$ref_text <- shiny::renderUI({
      shiny::req(det_result())
      mode_label <- if (identical(det_result()$ref_mode, "auto")) "(auto: max NMB)" else "(pinned)"
      shiny::tagList(
        stale_banner(),
        shiny::HTML(paste0("<h4>Reference: <span class='cea-ref'>",
                           det_result()$ref, "</span> ", mode_label,
                           " at WTP ", format(det_result()$wtp, big.mark = ",", scientific = FALSE), "</h4>")))
    })

    output$det_table <- DT::renderDT({
      shiny::req(det_result())
      tab <- det_result()$table[, c("Strategy", "Cost", "QALY", "NMB",
                                     "IncCost_vsRef", "IncQALY_vsRef", "ICER_vsRef")]
      DT::datatable(tab, options = list(pageLength = 8, scrollX = TRUE)) |>
        DT::formatCurrency(c("Cost", "NMB", "IncCost_vsRef"), digits = 0) |>
        DT::formatRound(c("QALY", "IncQALY_vsRef"), digits = 3) |>
        DT::formatRound("ICER_vsRef", digits = 0)
    })

    output$download_det_csv <- shiny::downloadHandler(
      filename = function() "deterministic_results.csv",
      content = function(file) utils::write.csv(det_result()$table, file, row.names = FALSE)
    )

    output$frontier_table <- DT::renderDT({
      shiny::req(det_result())
      fr <- det_result()$frontier
      ic <- det_result()$icers
      out <- merge(fr, ic, by = "Strategy", all.x = TRUE)
      DT::datatable(out, options = list(pageLength = 6, scrollX = TRUE))
    })

    # --- Tornado ---
    tornado_target <- shiny::reactiveVal(NULL)

    shiny::observeEvent(det_result(), {
      shiny::req(det_result())
      choices <- setdiff(det_result()$strategies, det_result()$ref)
      tornado_target(choices[1])
      output$tornado_target_ui <- shiny::renderUI({
        shiny::selectInput("tornado_target", "Target strategy (vs reference)",
                           choices = choices, selected = choices[1])
      })
    })

    shiny::observeEvent(input$tornado_target, {
      tornado_target(input$tornado_target)
    })

    tornado_plot_obj <- shiny::reactive({
      shiny::req(det_result())
      tgt <- tornado_target()
      shiny::req(tgt)
      strategies <- det_result()$strategies
      shiny::validate(shiny::need(tgt %in% strategies, "Invalid target strategy."))

      df <- params_rv()
      df_tor <- df[df$include_tornado %in% TRUE & (is.finite(df$lower) | is.finite(df$upper)), ]
      if (nrow(df_tor) == 0) return(NULL)

      tor_df <- build_tornado_data(
        df_tor, det_result()$pars, current_model(),
        det_result()$ref, tgt,
        metric = input$tornado_metric, wtp = det_result()$wtp
      )
      if (is.null(tor_df) || nrow(tor_df) == 0) return(NULL)

      metric_label <- if (input$tornado_metric == "INB") {
        paste0("INB (", tgt, " vs ", det_result()$ref, ")")
      } else {
        paste0("ICER (", tgt, " vs ", det_result()$ref, ")")
      }

      plot_tornado(tor_df, base_val = unique(tor_df$base)[1],
                   metric_label = metric_label,
                   title = paste0("Tornado: ", tgt, " vs ", det_result()$ref),
                   palette = current_palette())
    })

    output$tornado_plot <- shiny::renderPlot({
      p <- tornado_plot_obj()
      shiny::validate(shiny::need(!is.null(p), "Run deterministic analysis first; ensure parameters have tornado bounds."))
      print(p)
    }, bg = "transparent")

    output$download_tornado_png <- shiny::downloadHandler(
      filename = function() "tornado.png",
      content = function(file) {
        p <- tornado_plot_obj()
        shiny::validate(shiny::need(!is.null(p), "Tornado not available."))
        ggplot2::ggsave(file, plot = p, width = 9, height = 6, dpi = 300, units = "in")
      }
    )

    # --- Two-Way DSA ---
    output$twoway_controls <- shiny::renderUI({
      shiny::req(det_result())
      df <- params_rv()
      # Two-way grids need both bounds; NA flags are not TRUE (E12).
      candidates <- df$name[df$include_tornado %in% TRUE & is.finite(df$lower) & is.finite(df$upper)]
      shiny::req(length(candidates) >= 2)

      strategies <- det_result()$strategies
      ref <- det_result()$ref
      non_ref <- setdiff(strategies, ref)

      shiny::tagList(
        shiny::fluidRow(
          shiny::column(3, shiny::selectInput("tw_param1", "Parameter 1",
                                              choices = candidates, selected = candidates[1])),
          shiny::column(3, shiny::selectInput("tw_param2", "Parameter 2",
                                              choices = candidates, selected = candidates[min(2, length(candidates))])),
          shiny::column(3, shiny::selectInput("tw_target", "Target (vs ref)",
                                              choices = non_ref, selected = non_ref[1])),
          shiny::column(3, shiny::radioButtons("tw_metric", "Metric",
                                               choices = c("INB", "ICER"),
                                               selected = "INB", inline = TRUE))
        ),
        shiny::fluidRow(
          shiny::column(3, shiny::numericInput("tw_n", "Grid size", value = 20, min = 5, max = 50)),
          shiny::column(9, shiny::actionButton("tw_run", "Run Two-Way DSA",
                                               class = "btn-outline-secondary btn-sm"))
        )
      )
    })

    twoway_result <- shiny::eventReactive(input$tw_run, {
      shiny::req(det_result(), input$tw_param1, input$tw_param2, input$tw_target)
      shiny::req(input$tw_param1 != input$tw_param2)
      df <- params_rv()

      r1_row <- df[df$name == input$tw_param1, ]
      r2_row <- df[df$name == input$tw_param2, ]

      twoway_dsa(
        current_model(), det_result()$pars,
        param1 = input$tw_param1, param2 = input$tw_param2,
        range1 = c(r1_row$lower, r1_row$upper),
        range2 = c(r2_row$lower, r2_row$upper),
        n_grid = as.integer(.nc(input$tw_n, 20)),
        ref = det_result()$ref, target = input$tw_target,
        wtp = det_result()$wtp, metric = input$tw_metric
      )
    })

    output$twoway_plot <- shiny::renderPlot({
      shiny::req(twoway_result())
      print(plot_twoway_dsa(twoway_result(), palette = current_palette()))
    }, bg = "transparent")

    output$download_twoway_png <- shiny::downloadHandler(
      filename = function() "twoway_dsa.png",
      content = function(file) {
        ggplot2::ggsave(file, plot = plot_twoway_dsa(twoway_result(),
                        palette = current_palette()),
                        width = 9, height = 7, dpi = 300, units = "in")
      }
    )

    # --- Frontier plot ---
    ce_frontier_obj <- shiny::reactive({
      shiny::req(det_result())
      plot_frontier(det_result()$frontier_status, palette = current_palette())
    })

    output$ce_frontier_plot <- shiny::renderPlot({
      shiny::req(ce_frontier_obj())
      print(ce_frontier_obj())
    }, bg = "transparent")

    output$download_frontier_png <- shiny::downloadHandler(
      filename = function() "ce_frontier.png",
      content = function(file) {
        ggplot2::ggsave(file, plot = ce_frontier_obj(), width = 9, height = 6, dpi = 300, units = "in")
      }
    )

    # --- Threshold analysis ---
    output$threshold_controls <- shiny::renderUI({
      shiny::req(det_result())
      df <- params_rv()
      strategies <- det_result()$strategies
      ref <- det_result()$ref
      strat_choices <- setdiff(strategies, ref)
      shiny::validate(shiny::need(length(strat_choices) > 0, "Need strategies besides reference."))

      row0 <- df[1, ]
      min_val <- if (is.finite(row0$lower)) row0$lower else row0$base * 0.5
      max_val <- if (is.finite(row0$upper)) row0$upper else row0$base * 1.5

      shiny::tagList(
        shiny::fluidRow(
          shiny::column(4, shiny::selectInput("thr_strategy", "Target strategy",
                                              choices = strat_choices, selected = strat_choices[1])),
          shiny::column(4, shiny::selectInput("thr_param", "Parameter",
                                              choices = df$name, selected = df$name[1])),
          shiny::column(4, shiny::radioButtons("thr_mode", "Threshold type",
                                               choices = c("ICER = WTP" = "icer", "INB = 0" = "inb"),
                                               selected = "icer"))
        ),
        shiny::fluidRow(
          shiny::column(4, shiny::numericInput("thr_grid_n", "Grid points", value = 30, min = 5, max = 200)),
          shiny::column(4, shiny::numericInput("thr_min", "Min value", value = min_val)),
          shiny::column(4, shiny::numericInput("thr_max", "Max value", value = max_val))
        ),
        shiny::actionButton("run_threshold", "Run threshold search", class = "btn-warning")
      )
    })

    # The grid limits follow the chosen parameter (they used to come from the
    # first row whatever the choice, E9).
    shiny::observeEvent(input$thr_param, {
      df <- params_rv(); row <- df[df$name == input$thr_param, ]
      shiny::req(nrow(row) == 1)
      shiny::updateNumericInput(session, "thr_min",
        value = if (is.finite(row$lower)) row$lower else row$base * 0.5)
      shiny::updateNumericInput(session, "thr_max",
        value = if (is.finite(row$upper)) row$upper else row$base * 1.5)
    })

    threshold_result <- shiny::eventReactive(input$run_threshold, {
      shiny::req(det_result())
      res <- threshold_analysis(
        current_model(), det_result()$pars, input$thr_param,
        det_result()$ref, input$thr_strategy,
        input$thr_min, input$thr_max, input$thr_grid_n,
        wtp = det_result()$wtp, mode = input$thr_mode
      )
      # Labels and plots describe the inputs of THIS run, not the live controls (E9).
      res$param <- input$thr_param; res$strategy <- input$thr_strategy
      res$ref <- det_result()$ref; res$mode <- input$thr_mode; res$wtp <- det_result()$wtp
      res
    })

    output$threshold_text <- shiny::renderPrint({
      shiny::req(threshold_result())
      res <- threshold_result()
      cat("Parameter:", res$param, "\n")
      cat("Reference:", res$ref, "| Target:", res$strategy, "\n")
      if (is.finite(res$threshold)) {
        cat("Estimated threshold:", round(res$threshold, 4), "\n")
      } else {
        cat("No crossing found.\n")
        if (!is.null(res$message)) cat(res$message, "\n")
      }
    })

    output$threshold_plot <- shiny::renderPlot({
      shiny::req(threshold_result())
      res <- threshold_result()
      print(plot_threshold(res$grid, res$threshold, res$param,
                           res$wtp, mode = res$mode,
                           palette = current_palette()))
    }, bg = "transparent")

    output$download_threshold_csv <- shiny::downloadHandler(
      filename = function() paste0("threshold_", threshold_result()$param, ".csv"),
      content = function(file) utils::write.csv(threshold_result()$grid, file, row.names = FALSE)
    )

    # --- PSA ---
    shiny::observeEvent(input$run_psa, {
      N <- as.integer(input$psa_n)
      if (!is.finite(N) || N < 10) N <- 100
      N <- max(10, min(N, 10000))

      seed <- suppressWarnings(as.numeric(input$psa_seed))
      if (length(seed) != 1 || !is.finite(seed)) {
        shiny::showNotification("Enter a PSA seed (a whole number) before running.",
                                type = "error", duration = 6)
        return()
      }
      # Seeding right before the draws makes the PSA repeatable and lets the
      # exported R code (set.seed + run_psa) reproduce these exact results.
      set.seed(seed)
      res <- tryCatch(
        run_psa(current_model(), params_rv(), n_sim = N),
        error = function(e) {
          shiny::showNotification(
            paste("PSA error:", conditionMessage(e)),
            type = "error", duration = 15
          )
          NULL
        }
      )

      if (is.null(res)) return()

      if (res$dropped > 0) {
        shiny::showNotification(
          sprintf("PSA: %d/%d draws dropped.", res$dropped, N),
          type = if (res$dropped < 0.1 * N) "message" else "warning", duration = 8
        )
      }

      psa_state$draws <- res$draws
      psa_state$dropped <- res$dropped
      psa_rv(res$results)
      stale(FALSE)
    })

    output$ref_text_psa <- shiny::renderUI({
      shiny::req(det_result())
      mode_label <- if (identical(det_result()$ref_mode, "auto")) "(auto)" else "(pinned)"
      shiny::tagList(
        stale_banner(),
        shiny::HTML(paste0("<h4>PSA Reference: <span class='cea-ref'>",
                           det_result()$ref, "</span> ", mode_label, "</h4>")))
    })

    ce_plane_obj <- shiny::reactive({
      shiny::validate(
        shiny::need(!is.null(det_result()), "Run the deterministic analysis first: it sets the strategies and the reference."),
        shiny::need(!is.null(psa_results()), "Run the PSA."))
      df <- psa_results()
      ref <- det_result()$ref
      inc_list <- lapply(setdiff(det_result()$strategies, ref), function(s) {
        data.frame(
          strategy = s,
          inc_qaly = df[[paste0(s, ".q")]] - df[[paste0(ref, ".q")]],
          inc_cost = df[[paste0(s, ".cost")]] - df[[paste0(ref, ".cost")]],
          stringsAsFactors = FALSE
        )
      })
      inc_df <- do.call(rbind, inc_list)
      plot_ce_plane(inc_df, input$wtp, palette = current_palette())
    })

    output$ce_plane <- shiny::renderPlot({
      shiny::req(ce_plane_obj())
      print(ce_plane_obj())
    }, bg = "transparent")

    output$download_ce_plane_png <- shiny::downloadHandler(
      filename = function() "ce_plane.png",
      content = function(file) {
        ggplot2::ggsave(file, plot = ce_plane_obj(), width = 9, height = 6, dpi = 300, units = "in")
      }
    )

    output$psa_summary_ci <- DT::renderDT({
      shiny::validate(
        shiny::need(!is.null(det_result()), "Run the deterministic analysis first: it sets the strategies and the reference."),
        shiny::need(!is.null(psa_results()), "Run the PSA."))
      df <- psa_results()
      strategies <- det_result()$strategies
      wtp <- input$wtp
      summ <- lapply(strategies, function(s) {
        cvec <- df[[paste0(s, ".cost")]]
        qvec <- df[[paste0(s, ".q")]]
        nmb <- wtp * qvec - cvec
        fmt <- function(v) {
          m <- mean(v); l <- stats::quantile(v, 0.025); u <- stats::quantile(v, 0.975)
          paste0(format(round(m, 0), big.mark = ","),
                 " (", format(round(l, 0), big.mark = ","),
                 "-", format(round(u, 0), big.mark = ","), ")")
        }
        data.frame(Strategy = s, Cost = fmt(cvec),
                   QALY = paste0(round(mean(qvec), 3),
                                 " (", round(stats::quantile(qvec, 0.025), 3),
                                 "-", round(stats::quantile(qvec, 0.975), 3), ")"),
                   NMB = fmt(nmb), stringsAsFactors = FALSE)
      })
      DT::datatable(do.call(rbind, summ), options = list(pageLength = 10, scrollX = TRUE))
    })

    output$download_psa_csv <- shiny::downloadHandler(
      filename = function() "psa_results.csv",
      content = function(file) {
        shiny::validate(shiny::need(!is.null(psa_results()), "Run PSA first."))
        utils::write.csv(psa_results(), file, row.names = FALSE)
      }
    )

    # --- PSA Parameters tab ---
    output$psa_param_select_ui <- shiny::renderUI({
      dr <- psa_state$draws
      if (is.null(dr) || !ncol(dr)) {
        return(shiny::tags$em("Run PSA with include_psa = TRUE parameters."))
      }
      params <- colnames(dr)
      shiny::tagList(
        shiny::selectInput("psa_param_select", "Parameters to visualize",
                           choices = params, selected = params[1], multiple = TRUE),
        shiny::sliderInput("psa_param_bins", "Histogram bins",
                           min = 10, max = 100, value = 40, step = 5)
      )
    })

    psa_param_plot_obj <- shiny::reactive({
      dr <- psa_state$draws
      shiny::req(!is.null(dr), ncol(dr) > 0, input$psa_param_select)
      sel <- input$psa_param_select

      df_list <- lapply(sel, function(nm) {
        vals <- dr[[nm]]
        vals <- vals[is.finite(vals)]
        data.frame(parameter = nm, value = vals, stringsAsFactors = FALSE)
      })
      df_long <- do.call(rbind, df_list)

      ggplot2::ggplot(df_long, ggplot2::aes(x = .data$value)) +
        ggplot2::geom_histogram(bins = .nc(input$psa_param_bins, 40), alpha = 0.8) +
        ggplot2::facet_wrap(~ parameter, scales = "free",
                            ncol = min(length(sel), 3)) +
        ggplot2::labs(x = "Draw value", y = "Frequency",
                      title = "PSA parameter draws") +
        theme_cea()
    })

    output$psa_param_plot <- shiny::renderPlot({ print(psa_param_plot_obj()) }, bg = "transparent")

    output$psa_param_summary <- DT::renderDT({
      dr <- psa_state$draws
      shiny::req(!is.null(dr), ncol(dr) > 0, input$psa_param_select)
      sel <- input$psa_param_select
      s <- lapply(sel, function(nm) {
        v <- dr[[nm]]
        data.frame(Parameter = nm, Mean = round(mean(v, na.rm = TRUE), 6),
                   SD = round(stats::sd(v, na.rm = TRUE), 6),
                   P2.5 = round(stats::quantile(v, 0.025, na.rm = TRUE), 6),
                   P97.5 = round(stats::quantile(v, 0.975, na.rm = TRUE), 6),
                   stringsAsFactors = FALSE)
      })
      DT::datatable(do.call(rbind, s), options = list(pageLength = 10, scrollX = TRUE),
                    rownames = FALSE)
    })

    output$download_psa_draws_csv <- shiny::downloadHandler(
      filename = function() "psa_parameter_draws.csv",
      content = function(file) {
        shiny::validate(shiny::need(!is.null(psa_state$draws), "No draws available."))
        utils::write.csv(psa_state$draws, file, row.names = FALSE)
      }
    )

    output$download_psa_param_png <- shiny::downloadHandler(
      filename = function() "psa_parameter_draws.png",
      content = function(file) {
        ggplot2::ggsave(file, plot = psa_param_plot_obj(), width = 10, height = 6, dpi = 300, units = "in")
      }
    )

    # --- CEAC ---
    ceac_data <- shiny::reactive({
      shiny::validate(
        shiny::need(!is.null(det_result()), "Run the deterministic analysis first: it sets the strategies and the reference."),
        shiny::need(!is.null(psa_results()), "Run the PSA."))
      compute_ceac(psa_results(), det_result()$strategies,
                   wtp_max = max(input$wtp, 200000))
    })

    # CEAF on the same WTP grid as the CEAC, so its markers sit on the curves
    ceaf_data <- shiny::reactive({
      shiny::req(ceac_data())
      compute_ceaf(psa_results(), det_result()$strategies,
                   wtp_max = max(input$wtp, 200000))
    })

    # One builder for the screen and the PNG so both honour the checkbox
    ceac_plot_obj <- shiny::reactive({
      shiny::req(ceac_data())
      plot_ceac(ceac_data(), palette = current_palette(),
                ceaf = if (isTRUE(input$ceac_show_ceaf)) ceaf_data() else NULL)
    })

    output$ceac_plot <- shiny::renderPlot({
      print(ceac_plot_obj())
    }, bg = "transparent")

    output$download_ceac_png <- shiny::downloadHandler(
      filename = function() "ceac.png",
      content = function(file) {
        ggplot2::ggsave(file, plot = ceac_plot_obj(), width = 9, height = 6, dpi = 300, units = "in")
      }
    )

    # Expected loss curves shown under the CEAC, on the CEAC's WTP grid
    # (step 5000) so the optimal-strategy markers stay readable.
    ceac_el_data <- shiny::reactive({
      shiny::req(ceac_data())
      compute_expected_loss(psa_results(), det_result()$strategies,
                            wtp_max = max(input$wtp, 200000), wtp_step = 5000)
    })

    ceac_el_plot_obj <- shiny::reactive({
      plot_expected_loss(ceac_el_data(), palette = current_palette(),
                         show_optimal = TRUE)
    })

    output$ceac_el_plot <- shiny::renderPlot({
      print(ceac_el_plot_obj())
    }, bg = "transparent")

    output$download_ceac_el_png <- shiny::downloadHandler(
      filename = function() "expected_loss_curves.png",
      content = function(file) {
        ggplot2::ggsave(file, plot = ceac_el_plot_obj(), width = 9, height = 6, dpi = 300, units = "in")
      }
    )

    output$download_ceac_el_csv <- shiny::downloadHandler(
      filename = function() "expected_loss_curves.csv",
      content = function(file) utils::write.csv(ceac_el_data(), file, row.names = FALSE)
    )

    ceac_wide <- shiny::reactive({
      shiny::req(ceac_data())
      long <- ceac_data()
      strategies <- unique(long$Strategy)
      wtp_vals <- unique(long$WTP)
      wide <- data.frame(WTP = wtp_vals, stringsAsFactors = FALSE)
      for (s in strategies) {
        wide[[s]] <- long$ProbCE[long$Strategy == s]
      }
      wide
    })

    output$ceac_table <- DT::renderDT({
      DT::datatable(ceac_wide(), options = list(pageLength = 10, scrollX = TRUE)) |>
        DT::formatCurrency("WTP", digits = 0)
    })

    output$download_ceac_csv <- shiny::downloadHandler(
      filename = function() "ceac.csv",
      content = function(file) utils::write.csv(ceac_wide(), file, row.names = FALSE)
    )

    # --- Survival ---
    # Data shaping lives in cea_survival_data() / cea_trace_data() so that the
    # exported R code can call the same functions.
    survival_df_det <- shiny::reactive({
      shiny::req(det_result())
      cea_survival_data(det_result(), params_rv())
    })

    output$survival_controls <- shiny::renderUI({
      dfd <- survival_df_det()
      shiny::req(nrow(dfd) > 0)
      curves <- sort(unique(dfd$curve))
      shiny::fluidRow(
        shiny::column(4, shiny::selectInput("surv_curve", "Curve",
                                            choices = curves, selected = curves[1])),
        shiny::column(4, shiny::checkboxInput("surv_facets", "Facet by strategy", FALSE))
      )
    })

    survival_plot_obj <- shiny::reactive({
      dfd <- survival_df_det()
      shiny::req(nrow(dfd) > 0, input$surv_curve)
      plot_survival(dfd, curve_name = input$surv_curve, facet = isTRUE(input$surv_facets),
                    palette = current_palette())
    })

    output$survival_plot <- shiny::renderPlot({
      shiny::req(survival_plot_obj())
      print(survival_plot_obj())
    }, bg = "transparent")

    output$download_survival_png <- shiny::downloadHandler(
      filename = function() paste0("survival_", .nc(input$surv_curve, "OS"), ".png"),
      content = function(file) {
        ggplot2::ggsave(file, plot = survival_plot_obj(), width = 9, height = 6, dpi = 300, units = "in")
      }
    )

    # --- Traces ---
    trace_df_det <- shiny::reactive({
      shiny::req(det_result())
      cea_trace_data(det_result(), params_rv())
    })

    output$trace_controls <- shiny::renderUI({
      dft <- trace_df_det()
      shiny::req(nrow(dft) > 0)
      states <- sort(unique(dft$state))
      shiny::fluidRow(
        shiny::column(4, shiny::radioButtons("trace_scope", "Show",
                                             choices = c("All states", "One state"))),
        shiny::column(4, shiny::conditionalPanel(
          condition = "input.trace_scope == 'One state'",
          shiny::selectInput("trace_state", "State", choices = states, selected = states[1])
        )),
        shiny::column(4, shiny::checkboxInput("trace_facet", "Facet by strategy", TRUE))
      )
    })

    trace_plot_obj <- shiny::reactive({
      dft <- trace_df_det()
      shiny::req(nrow(dft) > 0)
      state_filt <- if (identical(.nc(input$trace_scope, "All states"), "One state")) {
        input$trace_state
      } else {
        NULL
      }
      plot_trace(dft, state_filter = state_filt, facet = isTRUE(input$trace_facet),
                 palette = current_palette())
    })

    output$trace_plot <- shiny::renderPlot({
      shiny::req(trace_plot_obj())
      print(trace_plot_obj())
    }, bg = "transparent")

    output$download_trace_png <- shiny::downloadHandler(
      filename = function() "traces.png",
      content = function(file) {
        ggplot2::ggsave(file, plot = trace_plot_obj(), width = 9, height = 6, dpi = 300, units = "in")
      }
    )

    # --- VOI ---
    evpi_data <- shiny::reactive({
      shiny::validate(
        shiny::need(!is.null(det_result()), "Run the deterministic analysis first: it sets the strategies and the reference."),
        shiny::need(!is.null(psa_results()), "Run the PSA."))
      compute_evpi(psa_results(), det_result()$strategies,
                   wtp_max = max(input$wtp, 200000))
    })

    output$evpi_plot <- shiny::renderPlot({
      shiny::req(evpi_data())
      print(plot_evpi(evpi_data(), palette = current_palette()))
    }, bg = "transparent")

    output$download_evpi_csv <- shiny::downloadHandler(
      filename = function() "evpi.csv",
      content = function(file) utils::write.csv(evpi_data(), file, row.names = FALSE)
    )

    evppi_data <- shiny::reactive({
      shiny::validate(
        shiny::need(!is.null(det_result()), "Run the deterministic analysis first: it sets the strategies and the reference."),
        shiny::need(!is.null(psa_results()), "Run the PSA."))
      draws <- psa_state$draws
      shiny::req(!is.null(draws), ncol(draws) > 0)
      shiny::withProgress(message = "Computing EVPPI...", value = 0.5, {
        compute_evppi(psa_results(), draws, det_result()$strategies, wtp = input$wtp)
      })
    })

    output$evppi_plot <- shiny::renderPlot({
      shiny::req(evppi_data())
      print(plot_evppi(evppi_data(), palette = current_palette()))
    }, bg = "transparent")

    output$evppi_table <- DT::renderDT({
      shiny::req(evppi_data())
      df <- evppi_data()
      df$EVPPI <- round(df$EVPPI, 2)
      DT::datatable(df, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
    })

    output$download_evppi_csv <- shiny::downloadHandler(
      filename = function() "evppi.csv",
      content = function(file) utils::write.csv(evppi_data(), file, row.names = FALSE)
    )

    # --- Expected Loss ---
    el_data <- shiny::reactive({
      shiny::validate(
        shiny::need(!is.null(det_result()), "Run the deterministic analysis first: it sets the strategies and the reference."),
        shiny::need(!is.null(psa_results()), "Run the PSA."))
      compute_expected_loss(psa_results(), det_result()$strategies,
                            wtp_max = max(input$wtp, 200000))
    })

    output$expected_loss_plot <- shiny::renderPlot({
      shiny::req(el_data())
      print(plot_expected_loss(el_data(), palette = current_palette()))
    }, bg = "transparent")

    output$download_el_csv <- shiny::downloadHandler(
      filename = function() "expected_loss.csv",
      content = function(file) utils::write.csv(el_data(), file, row.names = FALSE)
    )

    # --- Scenarios ---
    sc_store <- shiny::reactiveVal(list())

    output$sc_param_ui <- shiny::renderUI({
      shiny::req(params_rv())
      shiny::selectInput("sc_param_name", "Parameter to override",
                         choices = params_rv()$name)
    })

    shiny::observeEvent(input$sc_add, {
      nm <- trimws(input$sc_name)
      shiny::req(nzchar(nm), input$sc_param_name)
      current <- sc_store()
      if (is.null(current[[nm]])) current[[nm]] <- list()
      current[[nm]][[input$sc_param_name]] <- input$sc_param_value
      sc_store(current)
    })

    shiny::observeEvent(input$sc_add_base, {
      current <- sc_store()
      current[["Base case"]] <- list()
      sc_store(current)
    })

    shiny::observeEvent(input$sc_clear, { sc_store(list()) })

    output$sc_list_table <- DT::renderDT({
      sc <- sc_store()
      if (length(sc) == 0) {
        return(DT::datatable(
          data.frame(Scenario = "No scenarios defined", Overrides = "",
                     stringsAsFactors = FALSE),
          options = list(dom = "t"), rownames = FALSE
        ))
      }
      rows <- lapply(names(sc), function(nm) {
        overrides <- sc[[nm]]
        desc <- if (length(overrides) == 0) {
          "(base case)"
        } else {
          paste(names(overrides), "=", unlist(overrides), collapse = "; ")
        }
        data.frame(Scenario = nm, Overrides = desc, stringsAsFactors = FALSE)
      })
      DT::datatable(do.call(rbind, rows), options = list(dom = "t", pageLength = 20),
                    rownames = FALSE)
    })

    sc_results <- shiny::eventReactive(input$sc_run, {
      sc <- sc_store()
      shiny::req(length(sc) > 0)
      shiny::withProgress(message = "Running scenarios...", value = 0, {
        ref <- if (identical(input$ref_mode, "pinned")) input$ref_pinned else NULL
        res <- run_scenarios(current_model(), params_rv(), sc,
                             wtp = input$wtp, ref_strategy = ref)
        shiny::incProgress(1)
        res
      })
    })

    output$sc_results_table <- DT::renderDT({
      shiny::req(sc_results())
      df <- sc_results()
      DT::datatable(df, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE) |>
        DT::formatCurrency(c("Cost", "NMB"), digits = 0) |>
        DT::formatRound("QALY", digits = 3) |>
        DT::formatRound("ICER_vsRef", digits = 0)
    })

    output$sc_summary_table <- DT::renderDT({
      shiny::req(sc_results())
      summ <- summarize_scenarios(sc_results())
      DT::datatable(summ, options = list(dom = "t", pageLength = 20), rownames = FALSE) |>
        DT::formatCurrency(c("Cost", "NMB"), digits = 0) |>
        DT::formatRound("QALY", digits = 3)
    })

    output$download_sc_csv <- shiny::downloadHandler(
      filename = function() "scenario_results.csv",
      content = function(file) utils::write.csv(sc_results(), file, row.names = FALSE)
    )

    # --- Forest plot ---
    output$forest_target_ui <- shiny::renderUI({
      shiny::req(sc_results())
      strats <- unique(sc_results()$Strategy)
      strats <- strats[!is.na(strats)]
      shiny::selectInput("forest_target", "Strategy",
                         choices = strats, selected = strats[1])
    })

    forest_plot_obj <- shiny::reactive({
      shiny::req(sc_results(), input$forest_target)
      plot_scenario_forest(sc_results(), target = input$forest_target,
                           metric = input$forest_metric, wtp = input$wtp,
                           palette = current_palette())
    })

    output$forest_plot <- shiny::renderPlot({
      shiny::req(forest_plot_obj())
      print(forest_plot_obj())
    }, bg = "transparent")

    output$download_forest_png <- shiny::downloadHandler(
      filename = function() "scenario_forest.png",
      content = function(file) {
        ggplot2::ggsave(file, plot = forest_plot_obj(),
                        width = 9, height = 6, dpi = 300, units = "in")
      }
    )

    # --- Weighted Comparator ---
    wc_store <- shiny::reactiveVal(list())       # saved composites
    wc_building <- shiny::reactiveVal(list())     # current composite being built

    output$wc_strategy_ui <- shiny::renderUI({
      shiny::req(current_model())
      shiny::selectInput("wc_strategy", "Strategy",
                         choices = current_model()$info()$strategies)
    })

    shiny::observeEvent(input$wc_add_strat, {
      shiny::req(input$wc_strategy, input$wc_weight)
      cur <- wc_building()
      cur[[input$wc_strategy]] <- input$wc_weight
      wc_building(cur)
    })

    shiny::observeEvent(input$wc_save, {
      nm <- trimws(input$wc_name)
      shiny::req(nzchar(nm))
      bld <- wc_building()
      shiny::req(length(bld) > 0)
      # Normalize weights to sum to 1
      total <- sum(unlist(bld))
      if (total > 0) bld <- lapply(bld, function(x) x / total)
      composites <- wc_store()
      composites[[nm]] <- unlist(bld)
      wc_store(composites)
      wc_building(list())
    })

    shiny::observeEvent(input$wc_clear, {
      wc_store(list())
      wc_building(list())
    })

    output$wc_list_table <- DT::renderDT({
      composites <- wc_store()
      bld <- wc_building()
      rows <- list()
      # Show saved composites
      for (nm in names(composites)) {
        w <- composites[[nm]]
        desc <- paste(names(w), "=", round(w, 2), collapse = "; ")
        rows[[length(rows) + 1L]] <- data.frame(
          Composite = nm, Strategies = desc, Status = "Saved",
          stringsAsFactors = FALSE
        )
      }
      # Show current build
      if (length(bld) > 0) {
        desc <- paste(names(bld), "=", round(unlist(bld), 2), collapse = "; ")
        rows[[length(rows) + 1L]] <- data.frame(
          Composite = paste0("[Building: ", input$wc_name, "]"),
          Strategies = desc, Status = "Editing",
          stringsAsFactors = FALSE
        )
      }
      if (length(rows) == 0) {
        rows[[1]] <- data.frame(
          Composite = "No composites defined", Strategies = "", Status = "",
          stringsAsFactors = FALSE
        )
      }
      DT::datatable(do.call(rbind, rows), options = list(dom = "t", pageLength = 20),
                    rownames = FALSE)
    })

    wc_result <- shiny::eventReactive(input$wc_run, {
      composites <- wc_store()
      shiny::req(length(composites) > 0)
      run_weighted_analysis(current_model(), params_rv(), composites, wtp = input$wtp)
    })

    output$wc_results_table <- DT::renderDT({
      shiny::req(wc_result())
      df <- wc_result()$composites
      DT::datatable(df, options = list(dom = "t", pageLength = 20), rownames = FALSE) |>
        DT::formatCurrency(c("Cost", "NMB"), digits = 0) |>
        DT::formatRound("QALY", digits = 3)
    })

    output$wc_pairwise_table <- DT::renderDT({
      shiny::req(wc_result())
      df <- wc_result()$pairwise
      if (nrow(df) == 0) return(NULL)
      DT::datatable(df, options = list(dom = "t", pageLength = 20), rownames = FALSE) |>
        DT::formatCurrency(c("IncCost", "INB"), digits = 0) |>
        DT::formatRound("IncQALY", digits = 4) |>
        DT::formatRound("ICER", digits = 0)
    })

    output$download_wc_csv <- shiny::downloadHandler(
      filename = function() "weighted_analysis.csv",
      content = function(file) {
        utils::write.csv(wc_result()$composites, file, row.names = FALSE)
      }
    )

    output$wc_cost_plot <- shiny::renderPlot({
      shiny::req(wc_result())
      print(plot_weighted_comparison(wc_result()$details, metric = "Cost",
                                     palette = current_palette()))
    }, bg = "transparent")

    output$wc_qaly_plot <- shiny::renderPlot({
      shiny::req(wc_result())
      print(plot_weighted_comparison(wc_result()$details, metric = "QALY",
                                     palette = current_palette()))
    }, bg = "transparent")

    output$wc_forest_plot <- shiny::renderPlot({
      shiny::req(wc_result())
      pw <- wc_result()$pairwise
      if (nrow(pw) == 0) return(NULL)
      print(plot_weighted_pairwise(pw, metric = input$wc_forest_metric,
                                   wtp = input$wtp, palette = current_palette()))
    }, bg = "transparent")

    # --- BIA ---
    output$bia_current_mix_ui <- shiny::renderUI({
      shiny::req(current_model())
      strats <- current_model()$info()$strategies
      n <- length(strats)
      default_share <- round(1 / n, 2)
      inputs <- lapply(strats, function(s) {
        shiny::numericInput(paste0("bia_cur_", s), s, value = default_share,
                            min = 0, max = 1, step = 0.05)
      })
      do.call(shiny::tagList, inputs)
    })

    output$bia_new_mix_ui <- shiny::renderUI({
      shiny::req(current_model())
      strats <- current_model()$info()$strategies
      n <- length(strats)
      inputs <- lapply(seq_along(strats), function(i) {
        val <- if (i == 1) 0 else if (i == n) 1 else 0
        shiny::numericInput(paste0("bia_new_", strats[i]), strats[i],
                            value = val, min = 0, max = 1, step = 0.05)
      })
      do.call(shiny::tagList, inputs)
    })

    bia_result <- shiny::eventReactive(input$bia_run, {
      shiny::req(current_model(), params_rv())
      strats <- current_model()$info()$strategies

      cur_mix <- vapply(strats, function(s) {
        v <- input[[paste0("bia_cur_", s)]]
        if (is.null(v) || !is.finite(v)) 0 else v
      }, numeric(1))
      names(cur_mix) <- strats

      new_final <- vapply(strats, function(s) {
        v <- input[[paste0("bia_new_", s)]]
        if (is.null(v) || !is.finite(v)) 0 else v
      }, numeric(1))
      names(new_final) <- strats

      new_mix <- if (isTRUE(input$bia_linear_uptake)) {
        linear_uptake(cur_mix, new_final, years = input$bia_years)
      } else {
        new_final
      }

      growth <- .nc(input$bia_growth, 0) / 100

      shiny::withProgress(message = "Running BIA...", value = 0.5, {
        run_bia(current_model(), params_rv(),
                population = input$bia_pop,
                current_mix = cur_mix, new_mix = new_mix,
                years = input$bia_years, annual_growth = growth)
      })
    })

    output$bia_cost_plot <- shiny::renderPlot({
      shiny::req(bia_result())
      print(plot_bia(bia_result()$impact, palette = current_palette()))
    }, bg = "transparent")

    output$bia_waterfall_plot <- shiny::renderPlot({
      shiny::req(bia_result())
      print(plot_bia_waterfall(bia_result()$impact, palette = current_palette()))
    }, bg = "transparent")

    output$bia_total_text <- shiny::renderUI({
      shiny::req(bia_result())
      total <- bia_result()$total_impact
      fmt <- formatC(abs(round(total)), format = "f", big.mark = ",", digits = 0)
      label <- if (total >= 0) paste0("$", fmt, " additional cost") else paste0("$", fmt, " savings")
      shiny::h5(paste0("Total ", input$bia_years, "-year budget impact: "),
                shiny::span(label, class = if (total >= 0) "cea-fail" else "cea-pass"))
    })

    output$bia_impact_table <- DT::renderDT({
      shiny::req(bia_result())
      DT::datatable(bia_result()$impact, options = list(dom = "t", pageLength = 10),
                    rownames = FALSE) |>
        DT::formatCurrency(c("Cost_Current", "Cost_New", "Budget_Impact"), digits = 0)
    })

    output$download_bia_csv <- shiny::downloadHandler(
      filename = function() "bia_results.csv",
      content = function(file) utils::write.csv(bia_result()$annual, file, row.names = FALSE)
    )

    # --- Subgroups ---
    sg_store <- shiny::reactiveVal(list())

    output$sg_ref_ui <- shiny::renderUI({
      shiny::req(current_model())
      strats <- current_model()$info()$strategies
      shiny::selectInput("sg_ref", "Reference strategy", choices = strats, selected = strats[1])
    })

    output$sg_target_ui <- shiny::renderUI({
      shiny::req(current_model())
      strats <- current_model()$info()$strategies
      sel <- if (length(strats) >= 2) strats[2] else strats[1]
      shiny::selectInput("sg_target", "Target strategy", choices = strats, selected = sel)
    })

    output$sg_param_ui <- shiny::renderUI({
      shiny::req(params_rv())
      shiny::selectInput("sg_param_name", "Parameter to override",
                         choices = params_rv()$name)
    })

    shiny::observeEvent(input$sg_add, {
      nm <- trimws(input$sg_name)
      shiny::req(nzchar(nm), input$sg_param_name)
      current <- sg_store()
      if (is.null(current[[nm]])) current[[nm]] <- list()
      current[[nm]][[input$sg_param_name]] <- input$sg_param_value
      sg_store(current)
    })

    shiny::observeEvent(input$sg_add_overall, {
      current <- sg_store()
      current[["Overall"]] <- list()
      sg_store(current)
    })

    shiny::observeEvent(input$sg_clear, { sg_store(list()) })

    output$sg_list_table <- DT::renderDT({
      sg <- sg_store()
      if (length(sg) == 0) {
        return(DT::datatable(
          data.frame(Subgroup = "No subgroups defined", Overrides = "",
                     stringsAsFactors = FALSE),
          options = list(dom = "t"), rownames = FALSE
        ))
      }
      rows <- lapply(names(sg), function(nm) {
        overrides <- sg[[nm]]
        desc <- if (length(overrides) == 0) "(overall)" else
          paste(names(overrides), "=", unlist(overrides), collapse = "; ")
        data.frame(Subgroup = nm, Overrides = desc, stringsAsFactors = FALSE)
      })
      DT::datatable(do.call(rbind, rows), options = list(dom = "t", pageLength = 20),
                    rownames = FALSE)
    })

    sg_results <- shiny::eventReactive(input$sg_run, {
      sg <- sg_store()
      shiny::req(length(sg) > 0, input$sg_ref, input$sg_target)
      shiny::withProgress(message = "Running subgroup analysis...", value = 0.5, {
        run_subgroups(current_model(), params_rv(), sg,
                      target = input$sg_target, ref = input$sg_ref,
                      wtp = input$wtp)
      })
    })

    output$sg_results_table <- DT::renderDT({
      shiny::req(sg_results())
      df <- sg_results()[, c("Subgroup", "Target", "Reference",
                              "IncCost", "IncQALY", "ICER", "INB")]
      DT::datatable(df, options = list(pageLength = 20, scrollX = TRUE),
                    rownames = FALSE) |>
        DT::formatCurrency(c("IncCost", "INB"), digits = 0) |>
        DT::formatRound("IncQALY", digits = 4) |>
        DT::formatRound("ICER", digits = 0)
    })

    output$download_sg_csv <- shiny::downloadHandler(
      filename = function() "subgroup_results.csv",
      content = function(file) utils::write.csv(sg_results(), file, row.names = FALSE)
    )

    sg_forest_obj <- shiny::reactive({
      shiny::req(sg_results())
      plot_subgroup_forest(sg_results(), metric = input$sg_forest_metric,
                           wtp = input$wtp, palette = current_palette())
    })

    output$sg_forest_plot <- shiny::renderPlot({
      shiny::req(sg_forest_obj())
      print(sg_forest_obj())
    }, bg = "transparent")

    output$download_sg_forest_png <- shiny::downloadHandler(
      filename = function() "subgroup_forest.png",
      content = function(file) {
        ggplot2::ggsave(file, plot = sg_forest_obj(),
                        width = 9, height = 6, dpi = 300, units = "in")
      }
    )

    # --- Equity / DCEA ---
    output$equity_ref_ui <- shiny::renderUI({
      shiny::req(current_model())
      # Use predefined subgroups if available, or show message
      mod <- current_model()
      if (!is.null(mod$subgroups) && length(mod$subgroups) >= 2) {
        shiny::p(paste0("Using ", length(mod$subgroups),
                        " predefined subgroups from model definition."),
                 class = "cea-pass")
      } else {
        shiny::p("Define subgroups in the model (via new_cea_model(subgroups=...)) for equity analysis.",
                 class = "cea-fail")
      }
    })

    equity_result <- shiny::eventReactive(input$equity_run, {
      mod <- current_model()
      shiny::req(!is.null(mod$subgroups), length(mod$subgroups) >= 2)

      # Convert model subgroups to DCEA format (add share and baseline_qaly)
      sg_names <- names(mod$subgroups)
      n_sg <- length(sg_names)
      equal_share <- 1 / n_sg

      dcea_subgroups <- lapply(sg_names, function(nm) {
        sg <- mod$subgroups[[nm]]
        overrides <- if (is.list(sg)) sg else list()
        list(
          overrides = overrides,
          share = equal_share,
          baseline_qaly = 0.80 - (match(nm, sg_names) - 1) * 0.10
        )
      })
      names(dcea_subgroups) <- sg_names

      run_distributional_cea(mod, params_rv(), dcea_subgroups, wtp = input$wtp)
    })

    output$equity_nmb_plot <- shiny::renderPlot({
      shiny::req(equity_result())
      print(plot_nmb_by_subgroup(equity_result(), "NMB",
                                  palette = current_palette()))
    }, bg = "transparent")

    output$equity_lorenz_strategy_ui <- shiny::renderUI({
      shiny::req(equity_result())
      shiny::selectInput("equity_lorenz_strat", "Strategy for Lorenz curve",
                         choices = equity_result()$strategies)
    })

    output$equity_lorenz_plot <- shiny::renderPlot({
      shiny::req(equity_result(), input$equity_lorenz_strat)
      print(plot_health_distribution(equity_result(),
                                      strategy = input$equity_lorenz_strat,
                                      palette = current_palette()))
    }, bg = "transparent")

    output$equity_ci_table <- DT::renderDT({
      shiny::req(equity_result())
      rows <- lapply(equity_result()$strategies, function(s) {
        ci <- calculate_concentration_index(equity_result(), strategy = s)
        data.frame(Strategy = s, C_Index = round(ci$index, 4),
                   Interpretation = ci$interpretation, stringsAsFactors = FALSE)
      })
      DT::datatable(do.call(rbind, rows), options = list(dom = "t"),
                    rownames = FALSE)
    })

    output$equity_weighted_table <- DT::renderDT({
      shiny::req(equity_result())
      ew <- calculate_equity_weighted_nmb(equity_result(), method = "atkinson",
                                           epsilon = .nc(input$equity_epsilon, 1))
      ew$NMB <- round(ew$NMB)
      ew$Equity_Weighted_NMB <- round(ew$Equity_Weighted_NMB)
      ew$QALY <- round(ew$QALY, 3)
      ew$Equity_Weighted_QALY <- round(ew$Equity_Weighted_QALY, 3)
      DT::datatable(ew, options = list(dom = "t"), rownames = FALSE)
    })

    output$equity_frontier_plot <- shiny::renderPlot({
      shiny::req(equity_result())
      print(equity_frontier(equity_result(), palette = current_palette()))
    }, bg = "transparent")

    # --- Validation ---
    val_result <- shiny::eventReactive(input$val_run, {
      generate_validation_report(current_model(), params_rv(), wtp = input$wtp)
    })

    output$val_score_ui <- shiny::renderUI({
      shiny::req(val_result())
      score <- val_result()$score
      color <- if (score >= 90) "#2e7d32" else if (score >= 80) "#c0652d" else "#c62828"  # design tokens
      summ <- val_result()$summary
      shiny::tagList(
        shiny::h3(paste0("Score: ", score, "/100"),
                  style = paste0("color:", color, ";")),
        shiny::div(style = "display:flex; gap:10px;",
          shiny::span(class = "badge bg-success",
                      paste0("PASS: ", summ$Count[summ$Status == "PASS"])),
          shiny::span(class = "badge bg-warning",
                      paste0("WARN: ", summ$Count[summ$Status == "WARN"])),
          shiny::span(class = "badge bg-danger",
                      paste0("FAIL: ", summ$Count[summ$Status == "FAIL"]))
        )
      )
    })

    output$val_table <- DT::renderDT({
      shiny::req(val_result())
      df <- val_result()$details
      status_colors <- c(PASS = "#2e7d32", WARN = "#c0652d", FAIL = "#c62828")  # design tokens
      DT::datatable(df, options = list(pageLength = 20, scrollX = TRUE),
                    rownames = FALSE) |>
        DT::formatStyle("status",
          color = DT::styleEqual(names(status_colors), rep("white", 3)),
          backgroundColor = DT::styleEqual(names(status_colors), unname(status_colors)),
          fontWeight = "bold"
        )
    })

    output$download_val_md <- shiny::downloadHandler(
      filename = function() "validation_report.md",
      content = function(file) {
        generate_validation_report(current_model(), params_rv(),
                                   wtp = input$wtp, output_file = file)
      }
    )

    # --- Study details and CHEERS checklist ---
    # The form is generated from .study_fields (R/cea-study.R), prefilled from
    # the model, and rebuilt when the model changes.
    output$study_form <- shiny::renderUI({
      mod <- current_model()
      study <- .as_study(mod$study)
      strats <- mod$info()$strategies
      field_input <- function(i) {
        f <- .study_fields$field[i]
        id <- paste0("study_", f)
        val <- paste(study[[f]], collapse = "\n")
        if (.study_fields$long[i]) {
          shiny::textAreaInput(id, .study_fields$label[i], value = val, width = "100%",
                               rows = if (f == "assumptions") 5 else 2,
                               placeholder = .study_fields$hint[i])
        } else {
          shiny::textInput(id, .study_fields$label[i], value = val, width = "100%",
                           placeholder = .study_fields$hint[i])
        }
      }
      # One description per strategy (CHEERS item 7), placed after the setting
      comparators <- shiny::tagList(
        shiny::h6("Comparators: describe each strategy"),
        lapply(seq_along(strats), function(i) {
          shiny::textInput(paste0("study_comp_", i), strats[i], width = "100%",
                           value = .nc(unname(study$comparators[strats[i]]), ""))
        })
      )
      after_setting <- which(.study_fields$field == "setting")
      idx <- seq_len(nrow(.study_fields))
      shiny::tagList(
        lapply(idx[idx <= after_setting], field_input),
        comparators,
        lapply(idx[idx > after_setting], field_input)
      )
    })
    # The form must exist before the Study tab is opened: the report and the
    # code export read its inputs.
    shiny::outputOptions(output, "study_form", suspendWhenHidden = FALSE)

    # Study details as edited in the form (the model's own until it is drawn)
    current_study <- shiny::reactive({
      mod <- current_model()
      if (is.null(input$study_population)) return(.as_study(mod$study))
      strats <- mod$info()$strategies
      vals <- lapply(.study_fields$field, function(f) {
        v <- .nc(input[[paste0("study_", f)]], "")
        if (f == "assumptions") strsplit(v, "\n", fixed = TRUE)[[1]] else v
      })
      names(vals) <- .study_fields$field
      comp <- vapply(seq_along(strats), function(i) .nc(input[[paste0("study_comp_", i)]], ""),
                     character(1))
      vals$comparators <- stats::setNames(comp, strats)
      do.call(cea_study, vals)
    })

    # Analyses run so far. The run-on-click results signal "not run yet" with
    # a silent error, hence the tryCatch.
    analyses_run <- shiny::reactive({
      done <- function(expr) isTRUE(tryCatch(!is.null(expr), error = function(e) FALSE))
      c(if (!is.null(det_rv())) "deterministic",
        if (!is.null(psa_rv())) "psa",
        if (done(sc_results())) "scenarios",
        if (done(sg_results())) "subgroups",
        if (done(equity_result())) "equity")
    })

    cheers_data <- shiny::reactive({
      cheers_checklist(current_model(), study = current_study(), params = params_rv(),
                       analyses = analyses_run())
    })

    output$cheers_badges <- shiny::renderUI({
      st <- cheers_data()$Status
      shiny::tagList(
        shiny::span(class = "badge bg-success", paste0("Reported: ", sum(st == "Reported"))),
        shiny::span(class = "badge bg-warning", paste0("Partly: ", sum(st == "Partly"))),
        shiny::span(class = "badge bg-danger", paste0("Not reported: ", sum(st == "Not reported"))),
        shiny::span(class = "badge bg-secondary", paste0("Manuscript: ", sum(st == "Manuscript")))
      )
    })

    output$cheers_table <- DT::renderDT({
      status_colors <- c(Reported = "#2e7d32", Partly = "#c0652d",
                         `Not reported` = "#c62828", Manuscript = "#696d74")  # design tokens
      DT::datatable(cheers_data(), rownames = FALSE,
                    options = list(pageLength = 28, dom = "t", scrollX = TRUE)) |>
        DT::formatStyle("Status",
          color = DT::styleEqual(names(status_colors), rep("white", length(status_colors))),
          backgroundColor = DT::styleEqual(names(status_colors), unname(status_colors)),
          fontWeight = "bold")
    })

    output$download_cheers_csv <- shiny::downloadHandler(
      filename = function() "cheers_checklist.csv",
      content = function(file) utils::write.csv(cheers_data(), file, row.names = FALSE)
    )

    # --- R code export ---
    # Everything the generator needs, as a plain list (see R/cea-code.R). Inputs
    # of tabs not opened yet are NULL; the generator then uses the app defaults.
    code_state <- shiny::reactive({
      mod <- current_model()
      key <- if (length(models) == 1) names(models)[1] else input$model_key
      strats <- mod$info()$strategies
      # Market shares typed on the BIA tab (NULL until that tab has been opened)
      mix <- function(prefix) {
        v <- lapply(strats, function(s) input[[paste0(prefix, s)]])
        if (any(vapply(v, is.null, logical(1)))) return(NULL)
        v <- vapply(v, function(x) if (is.finite(x)) as.numeric(x) else 0, numeric(1))
        stats::setNames(v, strats)
      }
      list(
        model_code = if (is.null(model_code) || is.null(key)) NA_character_ else unname(model_code[key]),
        model_name = mod$info()$name,
        strategies = strats,
        default_params = sanitize_params(mod$default_params())$df,
        params = params_rv(),
        wtp = input$wtp, ref_mode = input$ref_mode, ref_pinned = input$ref_pinned,
        psa_n = input$psa_n, psa_seed = input$psa_seed,
        palette_choice = .nc(input$palette_choice, "cream"),
        palette_custom = if (identical(input$palette_choice, "custom")) {
          pal <- current_palette()
          list(strategies = pal$strategies, frontier = input$custom_frontier,
               dominated = input$custom_dominated, ext_dominated = input$custom_ext_dom)
        },
        tornado_metric = input$tornado_metric, tornado_target = tornado_target(),
        tw = list(param1 = input$tw_param1, param2 = input$tw_param2, target = input$tw_target,
                  metric = input$tw_metric, n = input$tw_n),
        thr = list(strategy = input$thr_strategy, param = input$thr_param, mode = input$thr_mode,
                   grid_n = input$thr_grid_n, min = input$thr_min, max = input$thr_max),
        psa_param_select = input$psa_param_select, psa_param_bins = input$psa_param_bins,
        ceac_show_ceaf = input$ceac_show_ceaf,
        surv_curve = input$surv_curve, surv_facets = input$surv_facets,
        trace_scope = input$trace_scope, trace_state = input$trace_state,
        trace_facet = input$trace_facet,
        scenarios = sc_store(), forest_target = input$forest_target,
        forest_metric = input$forest_metric,
        composites = wc_store(), wc_forest_metric = input$wc_forest_metric,
        bia = list(pop = input$bia_pop, years = input$bia_years, growth = input$bia_growth,
                   cur_mix = mix("bia_cur_"), new_mix = mix("bia_new_"),
                   linear = input$bia_linear_uptake),
        subgroups = sg_store(), sg_ref = input$sg_ref, sg_target = input$sg_target,
        sg_forest_metric = input$sg_forest_metric,
        study = current_study(), analyses = analyses_run(),
        model_subgroups = mod$subgroups, equity_epsilon = input$equity_epsilon,
        equity_lorenz_strat = input$equity_lorenz_strat
      )
    })

    # Code for the tab being viewed, or for every tab ("Full script")
    code_lines <- shiny::reactive({
      tab <- if (identical(input$code_scope, "full")) "full" else .nc(input$main_tabs, "Model Info")
      cea_analysis_code(tab, code_state())
    })

    shiny::observeEvent(input$show_code, {
      shiny::showModal(shiny::modalDialog(
        title = "R code to reproduce this analysis",
        size = "l", easyClose = TRUE,
        shiny::radioButtons("code_scope", NULL, inline = TRUE,
                            choices = c("This tab" = "tab", "Full script (all tabs)" = "full")),
        shiny::p("Paste this into your analysis script. It uses the model, parameter values ",
                 "and settings currently selected in the app.", class = "cea-hint"),
        shiny::verbatimTextOutput("code_text"),
        footer = shiny::tagList(
          # The browser copies the text of the code block; no server round trip
          shiny::tags$button(
            type = "button", class = "btn btn-outline-secondary btn-sm",
            onclick = paste0(
              "var b = this; navigator.clipboard.writeText(",
              "document.getElementById('code_text').innerText).then(function() {",
              "b.innerText = 'Copied'; setTimeout(function() { b.innerText = 'Copy'; }, 1500); });"),
            "Copy"),
          shiny::downloadButton("download_code", "Download (.R)",
                                class = "btn-outline-secondary btn-sm"),
          shiny::modalButton("Close")
        )
      ))
    })

    output$code_text <- shiny::renderText(paste(code_lines(), collapse = "\n"))

    output$download_code <- shiny::downloadHandler(
      filename = function() {
        tab <- if (identical(input$code_scope, "full")) "full" else .nc(input$main_tabs, "setup")
        paste0("shinycea_", gsub("[^a-z0-9]+", "_", tolower(tab)), ".R")
      },
      content = function(file) writeLines(code_lines(), file)
    )

    # --- Reports ---
    .collect_report_plots <- function() {
      safe_get <- function(expr) {
        tryCatch(expr, error = function(e) NULL)
      }
      plots <- list()
      plots$tornado <- safe_get(tornado_plot_obj())
      plots$ce_plane <- safe_get(ce_plane_obj())
      plots$ceac <- safe_get({
        cd <- ceac_data()
        if (!is.null(cd) && nrow(cd) > 0) plot_ceac(cd, palette = current_palette()) else NULL
      })
      plots$frontier <- safe_get(ce_frontier_obj())
      plots$threshold <- safe_get({                  # (E10: was never collected)
        res <- threshold_result()
        if (is.null(res)) NULL else plot_threshold(res$grid, res$threshold, res$param, res$wtp,
                                                   mode = res$mode, palette = current_palette())
      })
      plots$survival <- safe_get(survival_plot_obj())
      plots$traces <- safe_get(trace_plot_obj())
      plots
    }

    .collect_psa_summary <- function() {
      tryCatch({
        df <- psa_results()
        if (is.null(df) || nrow(df) == 0) return(NULL)
        build_psa_summary(df, det_result()$strategies, wtp = input$wtp)
      }, error = function(e) NULL)
    }

    output$download_report_docx <- shiny::downloadHandler(
      filename = function() "cea_report.docx",
      content = function(file) {
        shiny::req(det_result())
        shiny::withProgress(message = "Building Word report...", value = 0.2, {
          psa_res <- tryCatch(
            list(results = psa_results(), dropped = psa_state$dropped),
            error = function(e) NULL
          )
          shiny::incProgress(0.3)
          generate_cea_report(
            output_file = file,
            format = "word",
            model = current_model(),
            det_result = det_result(),
            psa_result = psa_res,
            wtp = input$wtp,
            plots = .collect_report_plots(),
            psa_summary_df = .collect_psa_summary(),
            study = current_study(),
            params_df = params_rv(),
            checklist = cheers_data()
          )
          shiny::incProgress(0.5)
        })
      }
    )

    output$download_report_pdf <- shiny::downloadHandler(
      filename = function() "cea_report.pdf",
      content = function(file) {
        shiny::req(det_result())
        shiny::withProgress(message = "Building PDF report...", value = 0.2, {
          psa_res <- tryCatch(
            list(results = psa_results(), dropped = psa_state$dropped),
            error = function(e) NULL
          )
          shiny::incProgress(0.3)
          generate_cea_report(
            output_file = file,
            format = "pdf",
            model = current_model(),
            det_result = det_result(),
            psa_result = psa_res,
            wtp = input$wtp,
            plots = .collect_report_plots(),
            psa_summary_df = .collect_psa_summary(),
            study = current_study(),
            params_df = params_rv(),
            checklist = cheers_data()
          )
          shiny::incProgress(0.5)
        })
      }
    )
  }
}
