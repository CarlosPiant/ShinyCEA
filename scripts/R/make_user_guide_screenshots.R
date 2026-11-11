# Screenshots for the user guide vignette (vignettes/user-guide.Rmd).
#
# What it does: drives the ShinyCEA app in headless Chrome, runs the analyses
#   of the Sick-Sicker example and saves one PNG per step of the guide.
# Inputs: a running app with the Sick-Sicker model only, e.g. from the
#   repository root in another R session:
#     devtools::load_all()
#     shiny::runApp(shiny::shinyApp(
#       cea_app_ui(c("Sick-Sicker (4-state Markov)" = "sick_sicker")),
#       cea_app_server(list(sick_sicker = cea_sick_sicker()),
#                      model_code = c(sick_sicker = "cea_sick_sicker()"))),
#       port = 7879, launch.browser = FALSE)
#   The URL can be given as the first command-line argument.
# Outputs: vignettes/figures/guide-*.png (overwritten on each run).
# Seed: the PSA uses the app's default seed (20250605), so the figures are
#   the same on every run.
# Usage (from the repository root): Rscript scripts/R/make_user_guide_screenshots.R

library(chromote)

args <- commandArgs(trailingOnly = TRUE)
url <- if (length(args) >= 1) args[1] else "http://localhost:7879"
out_dir <- file.path("vignettes", "figures")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Viewport of the screenshots: wide enough for the sidebar plus the tabs
vp_w <- 1200L
vp_h <- 800L

b <- ChromoteSession$new(width = vp_w, height = vp_h)
b$Page$navigate(url)

# ---- Helpers -----------------------------------------------------------------

# Run JavaScript in the page and return its value
js <- function(code) b$Runtime$evaluate(code, returnByValue = TRUE)$result$value

# Wait until Shiny has finished recalculating (no "busy" flag for a moment),
# so screenshots never show half-drawn outputs
wait_idle <- function(timeout = 120) {
  Sys.sleep(1)
  t0 <- Sys.time()
  repeat {
    busy <- isTRUE(js("document.documentElement.classList.contains('shiny-busy')"))
    if (!busy) { Sys.sleep(1.5); break }
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout) stop("App still busy")
    Sys.sleep(0.5)
  }
}

click <- function(id) { js(sprintf("document.getElementById('%s').click()", id)); wait_idle() }
open_tab <- function(name) {
  js(sprintf("$('a[data-value=\"%s\"]').tab('show'); window.scrollTo(0, 0);", name))
  wait_idle()
}

# Screenshot of what the user sees (the viewport), or of one element
shot_view <- function(name) {
  b$screenshot(file.path(out_dir, paste0("guide-", name, ".png")),
               cliprect = c(0, js("window.scrollY"), vp_w, vp_h), show = FALSE)
}
shot_element <- function(name, selector) {
  # Element screenshots are positioned from the top of the page: with the page
  # scrolled they capture the wrong region, so go back to the top first
  js("window.scrollTo(0, 0)"); Sys.sleep(0.5)
  b$screenshot(file.path(out_dir, paste0("guide-", name, ".png")),
               selector = selector, show = FALSE)
}
scroll_to <- function(id) {
  # 60 px of margin above the element, so its heading is not cut
  js(sprintf("window.scrollTo(0, document.getElementById('%s').getBoundingClientRect().top + window.scrollY - 60)", id))
  Sys.sleep(1)
}

# ---- Walk through the guide ----------------------------------------------------

wait_idle()
shot_view("01-start")                       # the app as it opens (Model Info)

open_tab("Parameters")
shot_view("02-parameters")

# 500 iterations keep the script quick; the guide tells users about 1,000+
js("$('#psa_n').val(500).trigger('change')")
click("run_det")
open_tab("Deterministic")
shot_view("03-deterministic")
shot_element("04-tornado", "#tornado_plot")

click("run_psa")
open_tab("PSA")
shot_view("05-psa")

open_tab("CEAC")
click("ceac_show_ceaf")                     # frontier superimposed on the CEAC
shot_view("06-ceac")
shot_element("07-expected-loss", "#ceac_el_plot")

open_tab("VOI")
shot_element("08-evppi", "#evppi_plot")

open_tab("Traces")
shot_view("09-traces")

open_tab("Scenarios")
click("sc_run")                             # the model's predefined scenarios
scroll_to("sc_run")
shot_view("10-scenarios")

open_tab("BIA")
click("bia_run")
shot_view("11-bia")

open_tab("Subgroups")
click("sg_run")
shot_element("12-subgroups", "#sg_forest_plot")

# Study details and the CHEERS checklist, after the analyses so that the
# checklist shows which items they cover
open_tab("Study")
shot_view("14-study")
shot_element("15-cheers", "#cheers_table")   # the whole checklist, all 28 items

open_tab("CEAC")
click("show_code")                          # the "R code for this tab" window
Sys.sleep(1)
shot_view("13-code")

b$close()
message("Saved: ", paste(list.files(out_dir, pattern = "^guide-"), collapse = ", "))
