#' Build the CEA App UI
#' @param model_choices Named character vector of model keys (for multi-model mode).
#' @return A Shiny UI definition.
#' @keywords internal
cea_app_ui <- function(model_choices = NULL) {
  multi_model <- !is.null(model_choices) && length(model_choices) > 1
  shiny::addResourcePath("shinycea", system.file("www", package = "ShinyCEA"))

  shiny::fluidPage(
    # Same bslib theme as the ShinyModels editor (R/editor-ui.R): navy primary,
    # cream background, Barlow body font. Everything else comes from shinycea.css.
    theme = bslib::bs_theme(version = 5, primary = "#18213d", bg = "#f6eddd", fg = "#4f535b",
                            base_font = bslib::font_collection("Barlow", "system-ui", "sans-serif")),
    title = "ShinyCEA",
    shiny::tags$head(
      # Fonts of the shared design; the stylesheet names system fallbacks, so
      # the app also works without internet access.
      shiny::tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
      shiny::tags$link(rel = "stylesheet", href = paste0(
        "https://fonts.googleapis.com/css2?family=Barlow:wght@400;600&",
        "family=Barlow+Semi+Condensed:wght@600;700&family=JetBrains+Mono:wght@400;500&display=swap")),
      shiny::tags$link(rel = "stylesheet", href = .cea_asset("shinycea.css"))
    ),
    # Top bar in the style of the ShinyModels toolbar; the model name is
    # filled by the server so the user sees which model is being analysed.
    shiny::div(
      class = "cea-topbar",
      shiny::span(class = "cea-title", "ShinyCEA"),
      shiny::span(class = "cea-subtitle", "Cost-effectiveness analysis"),
      shiny::uiOutput("topbar_model", inline = TRUE, container = shiny::span, class = "cea-model")
    ),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        if (multi_model) {
          shiny::div(class = "sidebar-section",
            shiny::h6("Model"),
            shiny::selectInput("model_key", NULL, choices = model_choices)
          )
        },
        shiny::div(class = "sidebar-section",
          shiny::h6("Data"),
          shiny::fileInput("param_file", NULL,
                           buttonLabel = "Browse...",
                           placeholder = "Upload CSV",
                           accept = ".csv")
        ),
        shiny::div(class = "sidebar-section",
          shiny::h6("Reference"),
          shiny::radioButtons("ref_mode", NULL,
                              choices = c("Auto (max NMB)" = "auto", "Pinned" = "pinned"),
                              inline = TRUE, selected = "auto"),
          shiny::uiOutput("ref_pinned_ui")
        ),
        shiny::div(class = "sidebar-section",
          shiny::h6("Analysis"),
          shiny::radioButtons("tornado_metric", "Tornado metric",
                              choices = c("INB", "ICER"), selected = "INB", inline = TRUE),
          shiny::numericInput("wtp", "WTP ($/QALY)", value = 100000, min = 0, step = 1000),
          shiny::numericInput("psa_n", "PSA iterations", value = 1000, min = 100, step = 100)
        ),
        shiny::hr(),
        shiny::div(class = "sidebar-section",
          shiny::h6("Reports"),
          shiny::downloadButton("download_report_docx", "Word (.docx)",
                                class = "btn-outline-secondary btn-sm"),
          shiny::downloadButton("download_report_pdf", "PDF (.pdf)",
                                class = "btn-outline-secondary btn-sm")
        ),
        shiny::hr(),
        shiny::actionButton("run_det", "Run Deterministic", class = "btn-primary btn-sm",
                            style = "width: 100%; margin-bottom: 6px;"),
        shiny::actionButton("run_psa", "Run PSA", class = "btn-success btn-sm",
                            style = "width: 100%;"),
        width = 3, class = "cea-sidebar"
      ),
      shiny::mainPanel(
        shiny::tabsetPanel(
          id = "main_tabs",
          shiny::tabPanel("Model Info",
            shiny::uiOutput("model_info_title"),
            shiny::uiOutput("model_info_description"),
            shiny::h4("Parameter Dictionary"),
            DT::DTOutput("model_param_desc")
          ),
          shiny::tabPanel("Parameters",
            shiny::h4("Edit parameters"),
            DT::DTOutput("param_table"),
            shiny::br(),
            shiny::downloadButton("download_params", "Download current parameters CSV",
                                  class = "btn-outline-secondary btn-sm")
          ),
          shiny::tabPanel("Health",
            shiny::fluidRow(
              shiny::column(12,
                shiny::div(style = "display:flex; gap:8px; margin:8px 0 14px 0;",
                  shiny::uiOutput("health_badges")
                )
              )
            ),
            DT::DTOutput("health_table")
          ),
          shiny::tabPanel("Deterministic",
            shiny::uiOutput("ref_text"),
            shiny::h4("Results table"),
            DT::DTOutput("det_table"),
            shiny::downloadButton("download_det_csv", "Download results (CSV)",
                                  class = "btn-outline-secondary btn-sm"),
            shiny::br(), shiny::br(),
            shiny::h4("Frontier (Dominance & Extended Dominance)"),
            DT::DTOutput("frontier_table"),
            shiny::br(),
            shiny::fluidRow(
              shiny::column(8,
                shiny::h4("Tornado Diagram"),
                shiny::uiOutput("tornado_target_ui"),
                shiny::plotOutput("tornado_plot", height = "500px")
              ),
              shiny::column(4,
                shiny::br(), shiny::br(),
                shiny::downloadButton("download_tornado_png", "Download Tornado (PNG)",
                                      class = "btn-outline-secondary btn-sm")
              )
            ),
            shiny::br(),
            shiny::hr(),
            shiny::h4("Two-Way Sensitivity Analysis"),
            shiny::p("Vary two parameters simultaneously to see how the decision changes.",
                     class = "cea-hint"),
            shiny::uiOutput("twoway_controls"),
            shiny::plotOutput("twoway_plot", height = "500px"),
            shiny::downloadButton("download_twoway_png", "Download Heatmap (PNG)",
                                  class = "btn-outline-secondary btn-sm"),
            shiny::br(), shiny::br(),
            shiny::h4("Cost-Effectiveness Frontier Plot"),
            shiny::plotOutput("ce_frontier_plot"),
            shiny::downloadButton("download_frontier_png", "Download Frontier (PNG)",
                                  class = "btn-outline-secondary btn-sm")
          ),
          shiny::tabPanel("Threshold",
            shiny::h4("ICER/INB threshold for a single parameter"),
            shiny::uiOutput("threshold_controls"),
            shiny::br(),
            shiny::verbatimTextOutput("threshold_text"),
            shiny::plotOutput("threshold_plot"),
            shiny::downloadButton("download_threshold_csv", "Download threshold grid (CSV)",
                                  class = "btn-outline-secondary btn-sm")
          ),
          shiny::tabPanel("PSA",
            shiny::uiOutput("ref_text_psa"),
            shiny::h4("Cost-Effectiveness Plane"),
            shiny::plotOutput("ce_plane"),
            shiny::downloadButton("download_ce_plane_png", "Download CE Plane (PNG)",
                                  class = "btn-outline-secondary btn-sm"),
            shiny::br(), shiny::br(),
            shiny::h4("PSA Summary (mean with 95% CI)"),
            DT::DTOutput("psa_summary_ci"),
            shiny::downloadButton("download_psa_csv", "Download PSA results (CSV)",
                                  class = "btn-outline-secondary btn-sm")
          ),
          shiny::tabPanel("PSA Parameters",
            shiny::h4("Distributions of PSA Parameter Draws"),
            shiny::uiOutput("psa_param_select_ui"),
            shiny::plotOutput("psa_param_plot"),
            shiny::fluidRow(
              shiny::column(6, shiny::downloadButton("download_psa_draws_csv", "Download draws (CSV)",
                                                     class = "btn-outline-secondary btn-sm")),
              shiny::column(6, shiny::downloadButton("download_psa_param_png", "Download plot (PNG)",
                                                     class = "btn-outline-secondary btn-sm"))
            ),
            shiny::br(),
            shiny::h4("Summary statistics"),
            DT::DTOutput("psa_param_summary")
          ),
          shiny::tabPanel("CEAC",
            shiny::h4("Cost-Effectiveness Acceptability Curve"),
            shiny::plotOutput("ceac_plot"),
            shiny::downloadButton("download_ceac_png", "Download CEAC (PNG)",
                                  class = "btn-outline-secondary btn-sm"),
            shiny::br(), shiny::br(),
            DT::DTOutput("ceac_table"),
            shiny::downloadButton("download_ceac_csv", "Download CEAC (CSV)",
                                  class = "btn-outline-secondary btn-sm")
          ),
          shiny::tabPanel("VOI",
            shiny::h4("Expected Value of Perfect Information (EVPI)"),
            shiny::p("EVPI quantifies the maximum value of eliminating all parameter uncertainty.",
                     class = "cea-hint"),
            shiny::plotOutput("evpi_plot"),
            shiny::downloadButton("download_evpi_csv", "Download EVPI (CSV)",
                                  class = "btn-outline-secondary btn-sm"),
            shiny::br(), shiny::br(),
            shiny::h4("Expected Loss by Strategy"),
            shiny::p("Expected opportunity loss shows the magnitude of choosing the wrong strategy. The optimal strategy minimizes expected loss.",
                     class = "cea-hint"),
            shiny::plotOutput("expected_loss_plot"),
            shiny::downloadButton("download_el_csv", "Download Expected Loss (CSV)",
                                  class = "btn-outline-secondary btn-sm"),
            shiny::br(), shiny::br(),
            shiny::h4("Expected Value of Partial Perfect Information (EVPPI)"),
            shiny::p("EVPPI identifies which parameters drive the most decision uncertainty.",
                     class = "cea-hint"),
            shiny::plotOutput("evppi_plot"),
            shiny::br(),
            DT::DTOutput("evppi_table"),
            shiny::downloadButton("download_evppi_csv", "Download EVPPI (CSV)",
                                  class = "btn-outline-secondary btn-sm")
          ),
          shiny::tabPanel("Survival",
            shiny::uiOutput("survival_controls"),
            shiny::plotOutput("survival_plot"),
            shiny::downloadButton("download_survival_png", "Download Survival (PNG)",
                                  class = "btn-outline-secondary btn-sm")
          ),
          shiny::tabPanel("Traces",
            shiny::uiOutput("trace_controls"),
            shiny::plotOutput("trace_plot"),
            shiny::downloadButton("download_trace_png", "Download Trace (PNG)",
                                  class = "btn-outline-secondary btn-sm")
          ),
          shiny::tabPanel("Scenarios",
            shiny::h4("Scenario Analysis"),
            shiny::p("Define parameter overrides for each scenario, then run all at once to compare.",
                     class = "cea-hint"),
            shiny::fluidRow(
              shiny::column(4,
                shiny::textInput("sc_name", "Scenario name", value = ""),
                shiny::uiOutput("sc_param_ui"),
                shiny::numericInput("sc_param_value", "New value", value = 0),
                shiny::actionButton("sc_add", "Add scenario", class = "btn-outline-secondary btn-sm"),
                shiny::actionButton("sc_add_base", "Add base case", class = "btn-outline-secondary btn-sm")
              ),
              shiny::column(8,
                shiny::h5("Defined scenarios"),
                DT::DTOutput("sc_list_table"),
                shiny::actionButton("sc_clear", "Clear all", class = "btn-outline-secondary btn-sm")
              )
            ),
            shiny::hr(),
            shiny::actionButton("sc_run", "Run all scenarios", class = "btn-primary btn-sm"),
            shiny::br(), shiny::br(),
            shiny::h4("Results comparison"),
            DT::DTOutput("sc_results_table"),
            shiny::downloadButton("download_sc_csv", "Download scenarios (CSV)",
                                  class = "btn-outline-secondary btn-sm"),
            shiny::br(), shiny::br(),
            shiny::h4("Optimal strategy per scenario"),
            DT::DTOutput("sc_summary_table"),
            shiny::br(), shiny::br(),
            shiny::h4("Forest plot"),
            shiny::p("Compare a strategy's ICER or NMB across all scenarios.",
                     class = "cea-hint"),
            shiny::fluidRow(
              shiny::column(4, shiny::uiOutput("forest_target_ui")),
              shiny::column(4, shiny::radioButtons("forest_metric", "Metric",
                                                    choices = c("ICER", "NMB"),
                                                    selected = "ICER", inline = TRUE))
            ),
            shiny::plotOutput("forest_plot", height = "400px"),
            shiny::downloadButton("download_forest_png", "Download Forest Plot (PNG)",
                                  class = "btn-outline-secondary btn-sm")
          ),
          shiny::tabPanel("Weighted",
            shiny::h4("Weighted Comparator Analysis"),
            shiny::p("Combine strategies into weighted composites (e.g., by market share) and compare.",
                     class = "cea-hint"),
            shiny::fluidRow(
              shiny::column(4,
                shiny::textInput("wc_name", "Composite name", value = ""),
                shiny::uiOutput("wc_strategy_ui"),
                shiny::numericInput("wc_weight", "Weight (0-1)", value = 0.5,
                                    min = 0, max = 1, step = 0.05),
                shiny::actionButton("wc_add_strat", "Add strategy to composite",
                                    class = "btn-outline-secondary btn-sm"),
                shiny::actionButton("wc_save", "Save composite",
                                    class = "btn-outline-secondary btn-sm"),
                shiny::actionButton("wc_clear", "Clear all",
                                    class = "btn-outline-secondary btn-sm")
              ),
              shiny::column(8,
                shiny::h5("Defined composites"),
                DT::DTOutput("wc_list_table")
              )
            ),
            shiny::hr(),
            shiny::actionButton("wc_run", "Run weighted analysis",
                                class = "btn-primary btn-sm"),
            shiny::br(), shiny::br(),
            shiny::h4("Composite results"),
            DT::DTOutput("wc_results_table"),
            shiny::br(),
            shiny::h4("Pairwise comparisons"),
            DT::DTOutput("wc_pairwise_table"),
            shiny::downloadButton("download_wc_csv", "Download results (CSV)",
                                  class = "btn-outline-secondary btn-sm"),
            shiny::br(), shiny::br(),
            shiny::fluidRow(
              shiny::column(6,
                shiny::h4("Cost breakdown"),
                shiny::plotOutput("wc_cost_plot")
              ),
              shiny::column(6,
                shiny::h4("QALY breakdown"),
                shiny::plotOutput("wc_qaly_plot")
              )
            ),
            shiny::br(),
            shiny::h4("Pairwise forest plot"),
            shiny::radioButtons("wc_forest_metric", NULL,
                                choices = c("ICER", "INB"), selected = "INB", inline = TRUE),
            shiny::plotOutput("wc_forest_plot", height = "350px")
          ),
          shiny::tabPanel("BIA",
            shiny::h4("Budget Impact Analysis"),
            shiny::p("Project total budget impact of adopting a new strategy mix over 3-5 years. ",
                     "Per-patient costs are the model's lifetime cost divided by its time horizon ",
                     "(average annual cost per patient).",
                     class = "cea-hint"),
            shiny::fluidRow(
              shiny::column(3,
                shiny::numericInput("bia_pop", "Target population", value = 100000,
                                    min = 1, step = 1000),
                shiny::numericInput("bia_years", "Projection years", value = 5,
                                    min = 1, max = 10, step = 1),
                shiny::numericInput("bia_growth", "Annual pop. growth (%)", value = 0,
                                    min = -5, max = 10, step = 0.5)
              ),
              shiny::column(4,
                shiny::h6("Current market shares", class = "cea-hint"),
                shiny::uiOutput("bia_current_mix_ui")
              ),
              shiny::column(4,
                shiny::h6("New market shares (final year)", class = "cea-hint"),
                shiny::uiOutput("bia_new_mix_ui")
              )
            ),
            shiny::checkboxInput("bia_linear_uptake", "Linear uptake transition", value = TRUE),
            shiny::actionButton("bia_run", "Run BIA", class = "btn-primary btn-sm"),
            shiny::br(), shiny::br(),
            shiny::fluidRow(
              shiny::column(6,
                shiny::h4("Cost comparison"),
                shiny::plotOutput("bia_cost_plot")
              ),
              shiny::column(6,
                shiny::h4("Annual budget impact"),
                shiny::plotOutput("bia_waterfall_plot")
              )
            ),
            shiny::br(),
            shiny::uiOutput("bia_total_text"),
            DT::DTOutput("bia_impact_table"),
            shiny::downloadButton("download_bia_csv", "Download BIA (CSV)",
                                  class = "btn-outline-secondary btn-sm")
          ),
          shiny::tabPanel("Subgroups",
            shiny::h4("Subgroup Analysis"),
            shiny::p("Compare ICERs across patient subgroups defined by parameter overrides.",
                     class = "cea-hint"),
            shiny::fluidRow(
              shiny::column(4,
                shiny::uiOutput("sg_ref_ui"),
                shiny::uiOutput("sg_target_ui"),
                shiny::textInput("sg_name", "Subgroup name", value = ""),
                shiny::uiOutput("sg_param_ui"),
                shiny::numericInput("sg_param_value", "New value", value = 0),
                shiny::actionButton("sg_add", "Add subgroup",
                                    class = "btn-outline-secondary btn-sm"),
                shiny::actionButton("sg_add_overall", "Add overall population",
                                    class = "btn-outline-secondary btn-sm"),
                shiny::actionButton("sg_clear", "Clear all",
                                    class = "btn-outline-secondary btn-sm")
              ),
              shiny::column(8,
                shiny::h5("Defined subgroups"),
                DT::DTOutput("sg_list_table")
              )
            ),
            shiny::hr(),
            shiny::actionButton("sg_run", "Run subgroup analysis",
                                class = "btn-primary btn-sm"),
            shiny::br(), shiny::br(),
            shiny::h4("Results"),
            DT::DTOutput("sg_results_table"),
            shiny::downloadButton("download_sg_csv", "Download subgroups (CSV)",
                                  class = "btn-outline-secondary btn-sm"),
            shiny::br(), shiny::br(),
            shiny::h4("Forest plot"),
            shiny::radioButtons("sg_forest_metric", "Metric",
                                choices = c("ICER", "INB"), selected = "ICER",
                                inline = TRUE),
            shiny::plotOutput("sg_forest_plot", height = "400px"),
            shiny::downloadButton("download_sg_forest_png", "Download Forest Plot (PNG)",
                                  class = "btn-outline-secondary btn-sm")
          ),
          shiny::tabPanel("Equity",
            shiny::h4("Distributional Cost-Effectiveness Analysis"),
            shiny::p("Analyze how costs and benefits distribute across population subgroups.",
                     class = "cea-hint"),
            shiny::uiOutput("equity_ref_ui"),
            shiny::numericInput("equity_epsilon", "Inequality aversion (epsilon)",
                                value = 1, min = 0, max = 10, step = 0.5),
            shiny::actionButton("equity_run", "Run Equity Analysis",
                                class = "btn-primary btn-sm"),
            shiny::br(), shiny::br(),
            shiny::h4("NMB by Subgroup"),
            shiny::plotOutput("equity_nmb_plot"),
            shiny::h4("Lorenz Curve"),
            shiny::uiOutput("equity_lorenz_strategy_ui"),
            shiny::plotOutput("equity_lorenz_plot"),
            shiny::h4("Concentration Index"),
            DT::DTOutput("equity_ci_table"),
            shiny::h4("Equity-Weighted NMB"),
            DT::DTOutput("equity_weighted_table"),
            shiny::h4("Equity-Efficiency Frontier"),
            shiny::plotOutput("equity_frontier_plot")
          ),
          shiny::tabPanel("Validation",
            shiny::h4("Model Validation Report"),
            shiny::p("Automated checks for transition matrices, probabilities, discounting, PSA inputs, and dominance logic.",
                     class = "cea-hint"),
            shiny::actionButton("val_run", "Run Validation",
                                class = "btn-primary btn-sm"),
            shiny::br(), shiny::br(),
            shiny::uiOutput("val_score_ui"),
            DT::DTOutput("val_table"),
            shiny::downloadButton("download_val_md", "Download Report (Markdown)",
                                  class = "btn-outline-secondary btn-sm")
          ),
          shiny::tabPanel("Settings",
            shiny::icon("gear"),
            value = "settings",
            shiny::h4("Figure Color Palette"),
            shiny::p("Choose a preset palette or define custom colors for all generated figures.",
                     class = "cea-hint"),
            shiny::radioButtons("palette_choice", "Preset palette",
              choices = c(
                "ShinyModels cream" = "cream",
                "University of Washington" = "uw",
                "Classic" = "classic",
                "Lancet" = "lancet",
                "Custom" = "custom"
              ),
              selected = "cream"
            ),
            shiny::uiOutput("palette_preview"),
            shiny::conditionalPanel(
              condition = "input.palette_choice == 'custom'",
              shiny::hr(),
              shiny::h5("Custom colors"),
              shiny::p("Pick colors for strategies and dominance categories.",
                       class = "cea-hint"),
              shiny::fluidRow(
                shiny::column(3, shiny::textInput("custom_color_1", "Strategy 1", value = "#18213d")),
                shiny::column(3, shiny::textInput("custom_color_2", "Strategy 2", value = "#1aa3a1")),
                shiny::column(3, shiny::textInput("custom_color_3", "Strategy 3", value = "#c0652d")),
                shiny::column(3, shiny::textInput("custom_color_4", "Strategy 4", value = "#2a56a3"))
              ),
              shiny::fluidRow(
                shiny::column(3, shiny::textInput("custom_color_5", "Strategy 5", value = "#2e7d32")),
                shiny::column(3, shiny::textInput("custom_color_6", "Strategy 6", value = "#c62828")),
                shiny::column(3, shiny::textInput("custom_color_7", "Strategy 7", value = "#696d74")),
                shiny::column(3, shiny::textInput("custom_color_8", "Strategy 8", value = "#3f7f8e"))
              ),
              shiny::hr(),
              shiny::fluidRow(
                shiny::column(4, shiny::textInput("custom_frontier", "Frontier", value = "#18213d")),
                shiny::column(4, shiny::textInput("custom_dominated", "Dominated", value = "#c62828")),
                shiny::column(4, shiny::textInput("custom_ext_dom", "Ext. dominated", value = "#c0652d"))
              )
            ),
            shiny::br(),
            shiny::h4("Preview"),
            shiny::plotOutput("palette_demo_plot", height = "250px")
          )
        )
      )
    )
  )
}

# URL of a file in inst/www, with its modification time as a cache buster
# (the same pattern ShinyModels uses for its editor assets).
.cea_asset <- function(file) {
  path <- system.file("www", file, package = "ShinyCEA")
  stamp <- if (nzchar(path)) as.integer(file.mtime(path)) else 0L
  paste0("shinycea/", file, "?v=", stamp)
}
