#' Build the CEA App UI
#' @param model_choices Named character vector of model keys (for multi-model mode).
#' @return A Shiny UI definition.
#' @keywords internal
cea_app_ui <- function(model_choices = NULL) {
  multi_model <- !is.null(model_choices) && length(model_choices) > 1

  shiny::fluidPage(
    theme = bslib::bs_theme(
      version = 5, bootswatch = "cosmo",
      primary = "#2C3E50", secondary = "#95A5A6",
      success = "#27AE60", info = "#3498DB",
      warning = "#F39C12", danger = "#E74C3C",
      bg = "#FAFBFC", fg = "#2C3E50",
      base_font = bslib::font_google("Inter"),
      heading_font = bslib::font_google("Inter")
    ),
    shiny::tags$head(shiny::tags$style(shiny::HTML("
      .nav-tabs .nav-link.active, .nav-tabs .nav-item.show .nav-link {
        color: #2C3E50 !important;
        font-weight: 600;
        border-bottom: 2px solid #2C3E50 !important;
      }
      .nav-tabs .nav-link {
        color: #7F8C8D !important;
        border: none !important;
        padding-bottom: 8px;
      }
      .nav-tabs .nav-link:hover { color: #2C3E50 !important; }
      .nav-tabs { border-bottom: 1px solid #DEE2E6; margin-bottom: 20px; }
      .sidebar { background-color: #F5F6F8; border-right: 1px solid #E8EAED; }
      .btn-primary { background-color: #2C3E50; border-color: #2C3E50; }
      .btn-primary:hover { background-color: #1A252F; border-color: #1A252F; }
      .btn-success { background-color: #27AE60; border-color: #27AE60; }
      .btn-success:hover { background-color: #1E8449; border-color: #1E8449; }
      .btn-outline-secondary { color: #5D6D7E; border-color: #BDC3C7; }
      .btn-outline-secondary:hover { background-color: #ECF0F1; color: #2C3E50; }
      h4 { color: #2C3E50; font-weight: 600; margin-top: 16px; }
      .well { background-color: #F5F6F8; border: 1px solid #E8EAED; }
      .shiny-download-link {
        display: inline-block; margin: 4px 0;
        font-size: 0.85rem;
      }
      .app-title {
        font-weight: 700; font-size: 1.3rem; color: #2C3E50;
        letter-spacing: -0.01em;
      }
      .app-subtitle { font-size: 0.8rem; color: #95A5A6; margin-top: 2px; }
      .sidebar-section { margin-bottom: 14px; }
      .sidebar-section h6 {
        font-size: 0.75rem; text-transform: uppercase; letter-spacing: 0.05em;
        color: #95A5A6; margin-bottom: 8px; font-weight: 600;
      }
      .palette-preview {
        display: inline-flex; gap: 3px; margin-top: 4px;
      }
      .palette-swatch {
        width: 20px; height: 20px; border-radius: 3px; border: 1px solid #DEE2E6;
      }
    "))),
    shiny::div(
      class = "app-title", "ShinyCEA",
      shiny::div(class = "app-subtitle", "Cost-Effectiveness Analysis Framework")
    ),
    shiny::br(),
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
        width = 3
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
                     style = "color: #7F8C8D; font-size: 0.9rem;"),
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
                     style = "color: #7F8C8D; font-size: 0.9rem;"),
            shiny::plotOutput("evpi_plot"),
            shiny::downloadButton("download_evpi_csv", "Download EVPI (CSV)",
                                  class = "btn-outline-secondary btn-sm"),
            shiny::br(), shiny::br(),
            shiny::h4("Expected Loss by Strategy"),
            shiny::p("Expected opportunity loss shows the magnitude of choosing the wrong strategy. The optimal strategy minimizes expected loss.",
                     style = "color: #7F8C8D; font-size: 0.9rem;"),
            shiny::plotOutput("expected_loss_plot"),
            shiny::downloadButton("download_el_csv", "Download Expected Loss (CSV)",
                                  class = "btn-outline-secondary btn-sm"),
            shiny::br(), shiny::br(),
            shiny::h4("Expected Value of Partial Perfect Information (EVPPI)"),
            shiny::p("EVPPI identifies which parameters drive the most decision uncertainty.",
                     style = "color: #7F8C8D; font-size: 0.9rem;"),
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
                     style = "color: #7F8C8D; font-size: 0.9rem;"),
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
                     style = "color: #7F8C8D; font-size: 0.9rem;"),
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
                     style = "color: #7F8C8D; font-size: 0.9rem;"),
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
            shiny::p("Project total budget impact of adopting a new strategy mix over 3-5 years.",
                     style = "color: #7F8C8D; font-size: 0.9rem;"),
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
                shiny::h6("Current market shares", style = "color: #7F8C8D; font-size: 0.75rem; text-transform: uppercase;"),
                shiny::uiOutput("bia_current_mix_ui")
              ),
              shiny::column(4,
                shiny::h6("New market shares (final year)", style = "color: #7F8C8D; font-size: 0.75rem; text-transform: uppercase;"),
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
                     style = "color: #7F8C8D; font-size: 0.9rem;"),
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
                     style = "color: #7F8C8D; font-size: 0.9rem;"),
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
                     style = "color: #7F8C8D; font-size: 0.9rem;"),
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
                     style = "color: #7F8C8D; font-size: 0.9rem;"),
            shiny::radioButtons("palette_choice", "Preset palette",
              choices = c(
                "University of Washington" = "uw",
                "Classic" = "classic",
                "Lancet" = "lancet",
                "Custom" = "custom"
              ),
              selected = "uw"
            ),
            shiny::uiOutput("palette_preview"),
            shiny::conditionalPanel(
              condition = "input.palette_choice == 'custom'",
              shiny::hr(),
              shiny::h5("Custom colors"),
              shiny::p("Pick colors for strategies and dominance categories.",
                       style = "color: #7F8C8D; font-size: 0.85rem;"),
              shiny::fluidRow(
                shiny::column(3, shiny::textInput("custom_color_1", "Strategy 1", value = "#2C3E50")),
                shiny::column(3, shiny::textInput("custom_color_2", "Strategy 2", value = "#E74C3C")),
                shiny::column(3, shiny::textInput("custom_color_3", "Strategy 3", value = "#27AE60")),
                shiny::column(3, shiny::textInput("custom_color_4", "Strategy 4", value = "#3498DB"))
              ),
              shiny::fluidRow(
                shiny::column(3, shiny::textInput("custom_color_5", "Strategy 5", value = "#9B59B6")),
                shiny::column(3, shiny::textInput("custom_color_6", "Strategy 6", value = "#F39C12")),
                shiny::column(3, shiny::textInput("custom_color_7", "Strategy 7", value = "#1ABC9C")),
                shiny::column(3, shiny::textInput("custom_color_8", "Strategy 8", value = "#E67E22"))
              ),
              shiny::hr(),
              shiny::fluidRow(
                shiny::column(4, shiny::textInput("custom_frontier", "Frontier", value = "#2C3E50")),
                shiny::column(4, shiny::textInput("custom_dominated", "Dominated", value = "#E74C3C")),
                shiny::column(4, shiny::textInput("custom_ext_dom", "Ext. dominated", value = "#F39C12"))
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
