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
