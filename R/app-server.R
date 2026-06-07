#' Build the CEA App Server
#' @param models Named list of cea_model objects.
#' @return A Shiny server function.
#' @keywords internal
cea_app_server <- function(models) {
  function(input, output, session) {
    .nc <- function(a, b) if (is.null(a)) b else a

    # --- Palette ---
    current_palette <- shiny::reactive({
      choice <- .nc(input$palette_choice, "uw")
      if (choice == "custom") {
        strat_colors <- c(
          .nc(input$custom_color_1, "#2C3E50"),
          .nc(input$custom_color_2, "#E74C3C"),
          .nc(input$custom_color_3, "#27AE60"),
          .nc(input$custom_color_4, "#3498DB"),
          .nc(input$custom_color_5, "#9B59B6"),
          .nc(input$custom_color_6, "#F39C12"),
          .nc(input$custom_color_7, "#1ABC9C"),
          .nc(input$custom_color_8, "#E67E22")
        )
        cea_palette_custom(
          strategies = strat_colors,
          frontier = .nc(input$custom_frontier, "#2C3E50"),
          dominated = .nc(input$custom_dominated, "#E74C3C"),
          ext_dominated = .nc(input$custom_ext_dom, "#F39C12")
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
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(legend.position = "none",
                       panel.grid.major.x = ggplot2::element_blank())
    })

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

    shiny::observe({
      mod <- current_model()
      s <- sanitize_params(mod$default_params())
      params_rv(s$df)
      if (length(s$notes)) {
        shiny::showNotification(paste(s$notes, collapse = " | "), type = "message", duration = 6)
      }
      # Load predefined composites, scenarios, and subgroups from the model
      if (!is.null(mod$composites) && is.list(mod$composites)) {
        wc_store(mod$composites)
      }
      if (!is.null(mod$scenarios) && is.list(mod$scenarios)) {
        sc_store(mod$scenarios)
      }
      if (!is.null(mod$subgroups) && is.list(mod$subgroups)) {
        sg_store(mod$subgroups)
      }
    })

    shiny::observeEvent(input$param_file, {
      shiny::req(input$param_file$datapath)
      df <- utils::read.csv(input$param_file$datapath, stringsAsFactors = FALSE)
      s <- sanitize_params(df)
      params_rv(s$df)
      if (length(s$notes)) {
        shiny::showNotification(paste(s$notes, collapse = " | "), type = "message", duration = 6)
      }
    })

    output$param_table <- DT::renderDT({
      DT::datatable(params_rv(), editable = "cell",
                    options = list(pageLength = 14, scrollX = TRUE))
    }, server = TRUE)

    proxy <- DT::dataTableProxy("param_table")
    shiny::observeEvent(input$param_table_cell_edit, {
      info <- input$param_table_cell_edit
      df <- params_rv()
      v <- info$value
      col_name <- names(df)[info$col]
      if (col_name %in% c("mean", "sd", "lower", "upper", "base")) v <- as.numeric(v)
      if (col_name %in% c("include_psa", "include_tornado")) v <- as.logical(as.character(v))
      df[info$row, info$col] <- v
      params_rv(df)
      DT::replaceData(proxy, df, resetPaging = FALSE)
    })

    output$download_params <- shiny::downloadHandler(
      filename = function() "parameters_current.csv",
      content = function(file) utils::write.csv(params_rv(), file, row.names = FALSE)
    )

    # --- Model Info ---
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
      status_colors <- c(PASS = "#1b9e77", WARN = "#d95f02", FAIL = "#d73027")
      DT::datatable(df, rownames = FALSE,
                    options = list(pageLength = 12, scrollX = TRUE)) |>
        DT::formatStyle("status",
          color = DT::styleEqual(names(status_colors), rep("white", length(status_colors))),
          backgroundColor = DT::styleEqual(names(status_colors), unname(status_colors)),
          fontWeight = "bold"
        )
    })

    # --- Deterministic results ---
    det_result <- shiny::eventReactive(input$run_det, {
      cat("[ShinyCEA] eventReactive START\n", file = stderr())
      ref <- if (identical(input$ref_mode, "pinned")) input$ref_pinned else NULL
      t0 <- proc.time()
      res <- run_deterministic(current_model(), params_rv(),
                               wtp = input$wtp, ref_strategy = ref)
      cat("[ShinyCEA] eventReactive DONE in", (proc.time() - t0)[3], "s\n", file = stderr())
      res
    })

    output$ref_text <- shiny::renderUI({
      shiny::req(det_result())
      mode_label <- if (identical(input$ref_mode, "auto")) "(auto: max NMB)" else "(pinned)"
      shiny::HTML(paste0("<h4>Reference: <span style='color:#4b2e83;'>",
                         det_result()$ref, "</span> ", mode_label, "</h4>"))
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
      df_tor <- df[df$include_tornado == TRUE & (is.finite(df$lower) | is.finite(df$upper)), ]
      if (nrow(df_tor) == 0) return(NULL)

      tor_df <- build_tornado_data(
        df_tor, det_result()$pars, current_model(),
        det_result()$ref, tgt,
        metric = input$tornado_metric, wtp = input$wtp
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
    })

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
      candidates <- df$name[df$include_tornado & (is.finite(df$lower) | is.finite(df$upper))]
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
        wtp = input$wtp, metric = input$tw_metric
      )
    })

    output$twoway_plot <- shiny::renderPlot({
      shiny::req(twoway_result())
      print(plot_twoway_dsa(twoway_result(), palette = current_palette()))
    })

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
    })

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

    threshold_result <- shiny::eventReactive(input$run_threshold, {
      shiny::req(det_result())
      threshold_analysis(
        current_model(), det_result()$pars, input$thr_param,
        det_result()$ref, input$thr_strategy,
        input$thr_min, input$thr_max, input$thr_grid_n,
        wtp = input$wtp, mode = input$thr_mode
      )
    })

    output$threshold_text <- shiny::renderPrint({
      shiny::req(threshold_result())
      res <- threshold_result()
      cat("Parameter:", input$thr_param, "\n")
      cat("Reference:", det_result()$ref, "| Target:", input$thr_strategy, "\n")
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
      print(plot_threshold(res$grid, res$threshold, input$thr_param,
                           input$wtp, mode = input$thr_mode,
                           palette = current_palette()))
    })

    output$download_threshold_csv <- shiny::downloadHandler(
      filename = function() paste0("threshold_", input$thr_param, ".csv"),
      content = function(file) utils::write.csv(threshold_result()$grid, file, row.names = FALSE)
    )

    # --- PSA ---
    psa_state <- shiny::reactiveValues(draws = NULL)

    psa_results <- shiny::eventReactive(input$run_psa, {
      N <- as.integer(input$psa_n)
      if (!is.finite(N) || N < 10) N <- 100
      N <- max(10, min(N, 10000))

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

      if (is.null(res)) return(NULL)

      if (res$dropped > 0) {
        shiny::showNotification(
          sprintf("PSA: %d/%d draws dropped.", res$dropped, N),
          type = if (res$dropped < 0.1 * N) "message" else "warning", duration = 8
        )
      }

      psa_state$draws <- res$draws
      res$results
    })

    output$ref_text_psa <- shiny::renderUI({
      shiny::req(det_result())
      mode_label <- if (identical(input$ref_mode, "auto")) "(auto)" else "(pinned)"
      shiny::HTML(paste0("<h4>PSA Reference: <span style='color:#4b2e83;'>",
                         det_result()$ref, "</span> ", mode_label, "</h4>"))
    })

    ce_plane_obj <- shiny::reactive({
      shiny::req(psa_results(), det_result())
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
    })

    output$download_ce_plane_png <- shiny::downloadHandler(
      filename = function() "ce_plane.png",
      content = function(file) {
        ggplot2::ggsave(file, plot = ce_plane_obj(), width = 9, height = 6, dpi = 300, units = "in")
      }
    )

    output$psa_summary_ci <- DT::renderDT({
      shiny::req(psa_results(), det_result())
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
        ggplot2::theme_minimal(base_size = 12)
    })

    output$psa_param_plot <- shiny::renderPlot({ print(psa_param_plot_obj()) })

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
      shiny::req(psa_results(), det_result())
      compute_ceac(psa_results(), det_result()$strategies,
                   wtp_max = max(input$wtp, 200000))
    })

    output$ceac_plot <- shiny::renderPlot({
      shiny::req(ceac_data())
      print(plot_ceac(ceac_data(), palette = current_palette()))
    })

    output$download_ceac_png <- shiny::downloadHandler(
      filename = function() "ceac.png",
      content = function(file) {
        ggplot2::ggsave(file, plot = plot_ceac(ceac_data(), palette = current_palette()), width = 9, height = 6, dpi = 300, units = "in")
      }
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
    .find_cycle_length <- function(df) {
      cl <- tryCatch({
        if ("cycle_length" %in% df$name) as.numeric(df$base[df$name == "cycle_length"][1])
        else if ("cycle_length_months" %in% df$name) as.numeric(df$base[df$name == "cycle_length_months"][1]) / 12
        else 1
      }, error = function(e) 1)
      if (is.na(cl) || cl <= 0) 1 else cl
    }

    .extract_surv <- function(res, cycle_len) {
      if ("survival" %in% names(res) && is.data.frame(res$survival)) {
        df <- res$survival
        if (!all(c("time", "surv") %in% names(df))) {
          return(data.frame(time = numeric(0), surv = numeric(0), curve = character(0),
                            stringsAsFactors = FALSE))
        }
        if (!"curve" %in% names(df)) df$curve <- "OS"
        return(df[, c("time", "surv", "curve")])
      }
      if ("trace" %in% names(res) && is.matrix(res$trace)) {
        dcol <- which(grepl("^d$|death|dead", tolower(colnames(res$trace))))
        if (length(dcol) > 0) {
          os <- 1 - res$trace[, dcol[1]]
          time <- seq(0, nrow(res$trace) - 1) * cycle_len
          return(data.frame(time = time, surv = as.numeric(os), curve = "OS",
                            stringsAsFactors = FALSE))
        }
      }
      data.frame(time = numeric(0), surv = numeric(0), curve = character(0),
                 stringsAsFactors = FALSE)
    }

    survival_df_det <- shiny::reactive({
      shiny::req(det_result())
      cl <- .find_cycle_length(params_rv())
      out <- lapply(names(det_result()$res), function(s) {
        df <- .extract_surv(det_result()$res[[s]], cl)
        if (nrow(df) == 0) return(NULL)
        df$strategy <- s
        df
      })
      do.call(rbind, out[!vapply(out, is.null, logical(1))])
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
    })

    output$download_survival_png <- shiny::downloadHandler(
      filename = function() paste0("survival_", .nc(input$surv_curve, "OS"), ".png"),
      content = function(file) {
        ggplot2::ggsave(file, plot = survival_plot_obj(), width = 9, height = 6, dpi = 300, units = "in")
      }
    )

    # --- Traces ---
    .derive_trace_from_surv <- function(surv_df) {
      if (!all(c("time", "surv", "curve") %in% names(surv_df))) {
        return(data.frame(time = numeric(0), state = character(0), prob = numeric(0),
                          stringsAsFactors = FALSE))
      }
      os <- surv_df[tolower(surv_df$curve) == "os", ]
      pfs <- surv_df[tolower(surv_df$curve) == "pfs", ]
      os <- os[order(os$time), ]
      if (nrow(os) == 0) {
        return(data.frame(time = numeric(0), state = character(0), prob = numeric(0),
                          stringsAsFactors = FALSE))
      }
      if (nrow(pfs) == 0) {
        death <- pmin(pmax(1 - os$surv, 0), 1)
        return(rbind(
          data.frame(time = os$time, state = "Alive", prob = os$surv, stringsAsFactors = FALSE),
          data.frame(time = os$time, state = "Death", prob = death, stringsAsFactors = FALSE)
        ))
      }
      pfs <- pfs[order(pfs$time), ]
      pfs_interp <- if (!identical(os$time, pfs$time)) {
        stats::approx(pfs$time, pfs$surv, xout = os$time, rule = 2, ties = "ordered")$y
      } else {
        pfs$surv
      }
      pf <- pmin(pmax(pfs_interp, 0), 1)
      prog <- pmin(pmax(os$surv - pfs_interp, 0), 1)
      death <- pmin(pmax(1 - os$surv, 0), 1)
      rbind(
        data.frame(time = os$time, state = "PF", prob = pf, stringsAsFactors = FALSE),
        data.frame(time = os$time, state = "Progressed", prob = prog, stringsAsFactors = FALSE),
        data.frame(time = os$time, state = "Death", prob = death, stringsAsFactors = FALSE)
      )
    }

    trace_df_det <- shiny::reactive({
      shiny::req(det_result())
      cl <- .find_cycle_length(params_rv())
      out <- lapply(names(det_result()$res), function(s) {
        r <- det_result()$res[[s]]
        if (!is.null(r$trace) && is.matrix(r$trace) && nrow(r$trace) > 0) {
          tr <- r$trace
          time <- seq(0, nrow(tr) - 1) * cl
          d <- as.data.frame(tr)
          d$time <- time
          d_long <- stats::reshape(d, direction = "long",
                                   varying = setdiff(names(d), "time"),
                                   v.names = "prob", timevar = "state",
                                   times = setdiff(names(d), "time"),
                                   idvar = "time")
          d_long$strategy <- s
          rownames(d_long) <- NULL
          return(d_long[, c("strategy", "time", "state", "prob")])
        }
        if (!is.null(r$survival) && is.data.frame(r$survival) && nrow(r$survival) > 0) {
          dt <- .derive_trace_from_surv(r$survival)
          if (nrow(dt) > 0) {
            dt$strategy <- s
            return(dt[, c("strategy", "time", "state", "prob")])
          }
        }
        NULL
      })
      do.call(rbind, out[!vapply(out, is.null, logical(1))])
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
    })

    output$download_trace_png <- shiny::downloadHandler(
      filename = function() "traces.png",
      content = function(file) {
        ggplot2::ggsave(file, plot = trace_plot_obj(), width = 9, height = 6, dpi = 300, units = "in")
      }
    )

    # --- VOI ---
    evpi_data <- shiny::reactive({
      shiny::req(psa_results(), det_result())
      compute_evpi(psa_results(), det_result()$strategies,
                   wtp_max = max(input$wtp, 200000))
    })

    output$evpi_plot <- shiny::renderPlot({
      shiny::req(evpi_data())
      print(plot_evpi(evpi_data(), palette = current_palette()))
    })

    output$download_evpi_csv <- shiny::downloadHandler(
      filename = function() "evpi.csv",
      content = function(file) utils::write.csv(evpi_data(), file, row.names = FALSE)
    )

    evppi_data <- shiny::reactive({
      shiny::req(psa_results(), det_result())
      draws <- psa_state$draws
      shiny::req(!is.null(draws), ncol(draws) > 0)
      shiny::withProgress(message = "Computing EVPPI...", value = 0.5, {
        compute_evppi(psa_results(), draws, det_result()$strategies, wtp = input$wtp)
      })
    })

    output$evppi_plot <- shiny::renderPlot({
      shiny::req(evppi_data())
      print(plot_evppi(evppi_data(), palette = current_palette()))
    })

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
      shiny::req(psa_results(), det_result())
      compute_expected_loss(psa_results(), det_result()$strategies,
                            wtp_max = max(input$wtp, 200000))
    })

    output$expected_loss_plot <- shiny::renderPlot({
      shiny::req(el_data())
      print(plot_expected_loss(el_data(), palette = current_palette()))
    })

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
    })

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
    })

    output$wc_qaly_plot <- shiny::renderPlot({
      shiny::req(wc_result())
      print(plot_weighted_comparison(wc_result()$details, metric = "QALY",
                                     palette = current_palette()))
    })

    output$wc_forest_plot <- shiny::renderPlot({
      shiny::req(wc_result())
      pw <- wc_result()$pairwise
      if (nrow(pw) == 0) return(NULL)
      print(plot_weighted_pairwise(pw, metric = input$wc_forest_metric,
                                   wtp = input$wtp, palette = current_palette()))
    })

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
    })

    output$bia_waterfall_plot <- shiny::renderPlot({
      shiny::req(bia_result())
      print(plot_bia_waterfall(bia_result()$impact, palette = current_palette()))
    })

    output$bia_total_text <- shiny::renderUI({
      shiny::req(bia_result())
      total <- bia_result()$total_impact
      fmt <- formatC(abs(round(total)), format = "f", big.mark = ",", digits = 0)
      label <- if (total >= 0) paste0("$", fmt, " additional cost") else paste0("$", fmt, " savings")
      shiny::h5(paste0("Total ", input$bia_years, "-year budget impact: "),
                shiny::span(label, style = paste0("color:", if (total >= 0) "#E74C3C" else "#27AE60", ";")))
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
    })

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
                 style = "color: #27AE60; font-size: 0.85rem;")
      } else {
        shiny::p("Define subgroups in the model (via new_cea_model(subgroups=...)) for equity analysis.",
                 style = "color: #E74C3C; font-size: 0.85rem;")
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
    })

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
    })

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
    })

    # --- Validation ---
    val_result <- shiny::eventReactive(input$val_run, {
      generate_validation_report(current_model(), params_rv(), wtp = input$wtp)
    })

    output$val_score_ui <- shiny::renderUI({
      shiny::req(val_result())
      score <- val_result()$score
      color <- if (score >= 90) "#27AE60" else if (score >= 80) "#F39C12" else "#E74C3C"
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
      status_colors <- c(PASS = "#1b9e77", WARN = "#d95f02", FAIL = "#d73027")
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
            list(results = psa_results(), dropped = 0L),
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
            psa_summary_df = .collect_psa_summary()
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
            list(results = psa_results(), dropped = 0L),
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
            psa_summary_df = .collect_psa_summary()
          )
          shiny::incProgress(0.5)
        })
      }
    )
  }
}
