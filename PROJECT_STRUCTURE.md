# ShinyCEA — Project Structure

R package with Shiny application for cost-effectiveness analysis (CEA).

```
ShinyCEA/
├── R/                          # Package R source files
│   ├── ShinyCEA-package.R      #   Package-level docs and imports
│   ├── cea-model.R             #   new_cea_model(), validate_cea_model(), cea_param()
│   ├── cea-params.R            #   sanitize_params(), sample_param_row()
│   ├── cea-simulate.R          #   safe_simulate(), run_deterministic(), run_psa(), compute_ceac()
│   ├── cea-analysis.R          #   cea_table(), frontier_analysis(), threshold_analysis(), build_tornado_data()
│   ├── cea-plots.R             #   plot_tornado(), plot_ce_plane(), plot_ceac(), plot_frontier(), etc.
│   ├── models-sick-sicker.R    #   cea_sick_sicker() — bundled example
│   ├── models-partitioned-survival.R  #   cea_partitioned_survival() — bundled example
│   ├── app-ui.R                #   Shiny UI builder
│   ├── app-server.R            #   Shiny server builder
│   └── run-app.R               #   run_cea_app() — user-facing launcher
├── man/                        # roxygen2-generated documentation (.Rd files)
├── tests/
│   └── testthat/               # Unit tests (testthat framework)
├── vignettes/                  # Long-form documentation and tutorials
├── data-raw/                   # Scripts to prepare included datasets
├── inst/                       # Files installed with the package
├── data/                       # Raw and processed analysis data
├── scripts/R/                  # Standalone analysis scripts (outside package)
├── writing/                    # Manuscript and documentation (.qmd, Typst math)
├── output/
│   ├── tables/                 # Generated tables
│   └── figures/                # Generated figures
├── quality_reports/            # Review reports, plans, session logs
├── .workflow-state/            # Workflow tracking state
├── .claude/                    # ClaudIA workflow infrastructure
├── DESCRIPTION                 # Package metadata (R CMD check: OK)
├── NAMESPACE                   # Exports/imports (roxygen2-generated)
├── LICENSE                     # MIT license
├── .Rbuildignore               # Files excluded from R CMD build
├── CLAUDE.md                   # Project instructions for Claude Code
└── PROJECT_STRUCTURE.md        # This file
```
