#' foundry10: ggplot2 Theme and Color Palettes for foundry10
#'
#' Provides a branded ggplot2 theme and a full suite of color scales for
#' foundry10 data visualizations. All palettes are drawn from the official
#' foundry10 brand guidelines.
#'
#' @section Main theme:
#' - [theme_foundry10()] -- the primary ggplot2 theme
#' - [preview_theme_foundry10()] -- renders a sample plot showing the full theme
#'
#' @section Color scales:
#' - [scale_fill_foundry10_d()] / [scale_color_foundry10_d()] -- qualitative
#'   (categorical, up to 6 levels)
#' - [scale_fill_foundry10_seq()] / [scale_color_foundry10_seq()] -- continuous
#'   sequential (orange or cyan)
#' - [scale_fill_foundry10_seq_d()] / [scale_color_foundry10_seq_d()] --
#'   discrete sequential (2-4 ordered levels)
#' - [scale_fill_foundry10_div()] / [scale_color_foundry10_div()] -- continuous
#'   diverging
#' - [scale_fill_foundry10_div_d()] / [scale_color_foundry10_div_d()] --
#'   discrete diverging / Likert (2-9 levels)
#'
#' @section Color utilities:
#' - [foundry10_colors] -- the full brand color list
#' - [foundry10_color()] -- look up a color by name
#' - [foundry10_palette()] -- retrieve a palette group
#' - [preview_foundry10_palette()] -- render a color swatch
#'
#' @section Data viz semantics (per foundry10 Visual Style Guide):
#' Orange colors signal negative/"bad" values; cyan colors signal
#' positive/"good" values. The qualitative palette is for categorical data
#' where no color carries directional meaning.
#'
#' @keywords internal
"_PACKAGE"
