#' Get a Named Color Palette
#'
#' Returns a list of colors for use in CEA plots. Three presets are available,
#' or users can supply fully custom colors.
#'
#' @param name One of "uw" (University of Washington), "classic", or "lancet".
#' @return A named list with colors: primary, secondary, accent, light, dark,
#'   frontier, dominated, ext_dominated, and strategies (a vector of 8 colors
#'   for strategy-level plots).
#' @export
cea_palette <- function(name = c("uw", "classic", "lancet")) {
  name <- match.arg(name)

  switch(name,
    uw = list(
      primary       = "#4B2E83",
      secondary     = "#B7A57A",
      accent        = "#85754D",
      light         = "#D8C6E8",
      dark          = "#33006F",
      frontier      = "#4B2E83",
      dominated     = "#B22E2E",
      ext_dominated = "#C77D1A",
      strategies    = c("#4B2E83", "#B7A57A", "#2E7D32", "#C62828",
                        "#1565C0", "#F57F17", "#6A1B9A", "#00695C")
    ),
    classic = list(
      primary       = "#2C3E50",
      secondary     = "#E74C3C",
      accent        = "#3498DB",
      light         = "#ECF0F1",
      dark          = "#1A252F",
      frontier      = "#2C3E50",
      dominated     = "#E74C3C",
      ext_dominated = "#F39C12",
      strategies    = c("#2C3E50", "#E74C3C", "#27AE60", "#3498DB",
                        "#9B59B6", "#F39C12", "#1ABC9C", "#E67E22")
    ),
    lancet = list(
      primary       = "#00468B",
      secondary     = "#ED0000",
      accent        = "#42B540",
      light         = "#D6E8F0",
      dark          = "#002A55",
      frontier      = "#00468B",
      dominated     = "#ED0000",
      ext_dominated = "#FF6F00",
      strategies    = c("#00468B", "#ED0000", "#42B540", "#0099B4",
                        "#925E9F", "#FDAF91", "#AD002A", "#ADB6B6")
    )
  )
}

#' Create a Custom Color Palette
#'
#' Build a palette from user-specified colors. Any omitted colors fall back
#' to the "classic" preset.
#'
#' @param strategies Character vector of hex colors for strategies (min 2).
#' @param frontier Hex color for frontier points/lines.
#' @param dominated Hex color for dominated strategies.
#' @param ext_dominated Hex color for extendedly dominated strategies.
#' @return A named list matching [cea_palette()] structure.
#' @export
cea_palette_custom <- function(strategies,
                               frontier = "#2C3E50",
                               dominated = "#E74C3C",
                               ext_dominated = "#F39C12") {
  if (length(strategies) < 2) {
    stop("Need at least 2 strategy colors.", call. = FALSE)
  }
  list(
    primary       = strategies[1],
    secondary     = strategies[2],
    accent        = if (length(strategies) >= 3) strategies[3] else "#3498DB",
    light         = "#ECF0F1",
    dark          = "#1A252F",
    frontier      = frontier,
    dominated     = dominated,
    ext_dominated = ext_dominated,
    strategies    = strategies
  )
}

#' ggplot2 Color Scale Using a CEA Palette
#'
#' @param palette A palette list from [cea_palette()] or [cea_palette_custom()].
#' @param ... Additional arguments passed to [ggplot2::scale_color_manual()].
#' @return A ggplot2 scale.
#' @export
scale_color_cea <- function(palette, ...) {
  ggplot2::scale_color_manual(values = palette$strategies, ...)
}

#' ggplot2 Fill Scale Using a CEA Palette
#'
#' @param palette A palette list from [cea_palette()] or [cea_palette_custom()].
#' @param ... Additional arguments passed to [ggplot2::scale_fill_manual()].
#' @return A ggplot2 scale.
#' @export
scale_fill_cea <- function(palette, ...) {
  ggplot2::scale_fill_manual(values = palette$strategies, ...)
}
