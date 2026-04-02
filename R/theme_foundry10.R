# theme_foundry10.R
# A bold, branded ggplot2 theme with custom palettes
# Created March 2026 by Kara Weisman with assistance from ClaudeAI
# Fonts: Helvetica Neue (falls back to Arial)
# Brand colors per org guidelines

#' @importFrom rlang .data
NULL

# -----------------------------------------------------------------------------
# 0. FONT SETUP (internal)
# -----------------------------------------------------------------------------

#' @keywords internal
.foundry10_font <- function() {
  available <- tryCatch({
    systemfonts::system_fonts()$family
  }, error = function(e) character(0))

  if ("Helvetica Neue" %in% available) {
    "Helvetica Neue"
  } else if ("Arial" %in% available) {
    "Arial"
  } else {
    "sans"
  }
}

# -----------------------------------------------------------------------------
# 1. FOUNDRY10 COLOR PALETTES
# -----------------------------------------------------------------------------

#' foundry10 brand color palettes
#'
#' A named list containing all official foundry10 brand colors, organized by
#' palette group. Use [foundry10_color()] to look up individual colors by name,
#' or [foundry10_palette()] to retrieve a full group.
#'
#' @format A named list with five elements:
#' \describe{
#'   \item{primary}{4 core brand colors: orange, cyan, black, offwhite}
#'   \item{secondary}{8 secondary colors: cyan and orange variants}
#'   \item{tints_orange}{4 orange tints from 100% to 10%}
#'   \item{tints_cyan}{4 cyan tints from 100% to 10%}
#'   \item{gray}{6 grayscale values from absolute black to absolute white}
#' }
#' @export
foundry10_colors <- list(

  primary   = c(orange_foundry10      = "#D14E1D",
                cyan_foundry10        = "#01646F",
                black                 = "#111111",
                offwhite              = "#F9F9F9"),

  secondary = c(cyan_warm             = "#077A8A",
                cyan_cool             = "#007079",
                cyan_bright           = "#078B9C",
                cyan_verydark         = "#0E2528",
                orange_light          = "#F9A65F",
                orange_soft           = "#E77C53",
                orange_text           = "#CA4A1C",
                orange_websafebackground = "#BE5127"),

  tints_orange = c("#D14E1D", "#D96F50", "#EBC5B9", "#F4E6E2"),
  tints_cyan   = c("#01646F", "#326A72", "#AEC4C6", "#E0ECED"),

  gray = c(black_absolute  = "#000000",
           gray_dark        = "#575857",
           gray_mediumdark  = "#939598",
           gray_light       = "#E2E3E4",
           gray_verylight   = "#F2F2F2",
           white_absolute   = "#FFFFFF")
)

#' Look up a foundry10 brand color by name
#'
#' Returns the hex code for a named color from [foundry10_colors].
#'
#' @param name Character. One or more color names, e.g. `"orange_foundry10"`,
#'   `"cyan_warm"`, `"gray_light"`. See [foundry10_colors] for all valid names.
#' @return A named character vector of hex codes (names stripped).
#' @export
#' @examples
#' foundry10_color("orange_foundry10")   # "#D14E1D"
#' foundry10_color("cyan_cool")          # "#007079"
#' foundry10_color(c("orange_foundry10", "cyan_foundry10"))
foundry10_color <- function(name) {
  all_colors <- unlist(foundry10_colors)
  names(all_colors) <- sub("^.*\\.", "", names(all_colors))
  matched <- all_colors[name]
  if (any(is.na(matched))) {
    warning("Color(s) not found: ",
            paste(name[is.na(matched)], collapse = ", "))
  }
  unname(matched)
}

#' Return a named vector of foundry10 colors by palette group
#'
#' @param palette Character. One of `"primary"`, `"secondary"`,
#'   `"tints_orange"`, `"tints_cyan"`, `"gray"`, or `"all"`.
#' @return A named character vector of hex codes.
#' @export
#' @examples
#' foundry10_palette("primary")
#' foundry10_palette("gray")
foundry10_palette <- function(palette = "primary") {
  if (palette == "all") return(unlist(foundry10_colors))
  if (!palette %in% names(foundry10_colors)) {
    stop("Unknown palette '", palette, "'. ",
         "Choose from: ",
         paste(names(foundry10_colors), collapse = ", "), ", all")
  }
  foundry10_colors[[palette]]
}

#' Preview a foundry10 palette as a color swatch
#'
#' Renders a ggplot2 tile chart showing all colors in the selected palette
#' group, labeled with color names and hex codes.
#'
#' @param palette Character. Passed to [foundry10_palette()]. Default
#'   `"primary"`.
#' @return A ggplot2 object (rendered as a side effect).
#' @export
#' @examples
#' \dontrun{
#' preview_foundry10_palette("primary")
#' preview_foundry10_palette("secondary")
#' preview_foundry10_palette("gray")
#' }
preview_foundry10_palette <- function(palette = "primary") {
  cols <- foundry10_palette(palette)
  df <- data.frame(
    color = factor(names(cols), levels = names(cols)),
    x     = seq_along(cols),
    hex   = cols
  )
  ggplot2::ggplot(df, ggplot2::aes(x = .data$x, y = 1,
                                   fill = .data$color)) +
    ggplot2::geom_tile(width = 0.9, height = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = .data$hex),
                       y = 0.65, size = 3,
                       family = .foundry10_font(),
                       color = "#111111") +
    ggplot2::geom_text(ggplot2::aes(label = .data$color),
                       y = 1.35, size = 3,
                       family = .foundry10_font(),
                       color = "#111111") +
    ggplot2::scale_fill_manual(values = stats::setNames(cols, names(cols))) +
    ggplot2::scale_y_continuous(limits = c(0.4, 1.6)) +
    ggplot2::labs(title = paste0("foundry10 palette: ", palette)) +
    ggplot2::theme_void(base_family = .foundry10_font()) +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(size = 13, face = "bold",
                                         margin = ggplot2::margin(b = 8))
    )
}

# -----------------------------------------------------------------------------
# 2. DISCRETE / QUALITATIVE PALETTE
# -----------------------------------------------------------------------------

# 6-color palette drawn entirely from brand guidelines.
# Intentionally capped at 6 -- the brand palette does not contain additional
# colors with sufficient hue/luminance separation for data use.
# Tested against deuteranopia, protanopia, and tritanopia simulations.
#
# ORDER RATIONALE:
#   1. foundry10 orange        -- warm anchor, high chroma
#   2. foundry10 dark cyan     -- cool anchor, distinct hue + luminance from orange
#   3. very dark cyan          -- dark anchor, near-black
#   4. light orange            -- light warm, high luminance contrast vs cyan
#   5. medium dark gray        -- neutral separator, universally distinct
#   6. 20% tint foundry10 cyan -- airy light cool, luminance-distinct from #01646F
#
# DATA VIZ SEMANTICS (per foundry10 Visual Style Guide):
#   - Orange = negative / "bad" values; cyan = positive / "good" values.
#   - This palette is for CATEGORICAL data where no color carries a
#     positive/negative meaning. For directional data use scale_fill_foundry10_div().
#   - Avoid assigning orange (slot 1) to a "yes", "good", or positive category.

.palette_qualitative <- c(
  "#D14E1D",  # 1. foundry10 orange
  "#01646F",  # 2. foundry10 dark cyan
  "#0E2528",  # 3. very dark cyan
  "#F9A65F",  # 4. light orange
  "#939598",  # 5. medium dark gray
  "#AEC4C6"   # 6. 20% tint foundry10 cyan
)

#' Discrete foundry10 color scale (fill)
#'
#' A qualitative 6-color palette drawn from the foundry10 brand guidelines,
#' tested for colorblind safety. Intended for categorical data with no
#' positive/negative valence. See `vignette("palettes")` for usage guidance.
#'
#' @param ... Arguments passed to [ggplot2::discrete_scale()].
#' @return A ggplot2 scale object.
#' @export
#' @examples
#' \dontrun{
#' library(ggplot2)
#' mpg6 <- mpg[mpg$class != "minivan", ]
#' ggplot(mpg6, aes(displ, hwy, fill = class)) +
#'   geom_col() +
#'   scale_fill_foundry10_d()
#' }
scale_fill_foundry10_d <- function(...) {
  pal <- function(n) {
    if (n > length(.palette_qualitative)) {
      warning("Requested more colors than available in the foundry10 ",
              "discrete palette (max 6).")
    }
    .palette_qualitative[seq_len(min(n, length(.palette_qualitative)))]
  }
  ggplot2::discrete_scale("fill", "foundry10_d", palette = pal, ...)
}

#' Discrete foundry10 color scale (color/colour)
#'
#' A qualitative 6-color palette drawn from the foundry10 brand guidelines,
#' tested for colorblind safety. Intended for categorical data with no
#' positive/negative valence.
#'
#' @param ... Arguments passed to [ggplot2::discrete_scale()].
#' @return A ggplot2 scale object.
#' @export
#' @examples
#' \dontrun{
#' library(ggplot2)
#' mpg6 <- mpg[mpg$class != "minivan", ]
#' ggplot(mpg6, aes(displ, hwy, color = class)) +
#'   geom_point() +
#'   scale_color_foundry10_d()
#' }
scale_color_foundry10_d <- function(...) {
  pal <- function(n) {
    if (n > length(.palette_qualitative)) {
      warning("Requested more colors than available in the foundry10 ",
              "discrete palette (max 6).")
    }
    .palette_qualitative[seq_len(min(n, length(.palette_qualitative)))]
  }
  ggplot2::discrete_scale("colour", "foundry10_d", palette = pal, ...)
}

#' @rdname scale_color_foundry10_d
#' @export
scale_colour_foundry10_d <- scale_color_foundry10_d

# -----------------------------------------------------------------------------
# 3. SEQUENTIAL PALETTE (light -> dark, orange or cyan tints)
# -----------------------------------------------------------------------------

# Anchors use only documented brand tints -- no extrapolation.
# tint = "orange": use for negative/"bad" values (per style guide).
# tint = "cyan":   use for positive/"good" values (per style guide).

#' Sequential foundry10 palette function
#'
#' Returns a vector of n colors interpolated between the four documented
#' foundry10 brand tints for the selected color family.
#'
#' @param n Integer. Number of colors.
#' @param tint Character. `"orange"` (default) or `"cyan"`.
#' @param direction Integer. `1` = light to dark (default), `-1` = reversed.
#' @return Character vector of hex codes.
#' @export
#' @examples
#' foundry10_pal_seq(5)
#' foundry10_pal_seq(5, tint = "cyan")
#' foundry10_pal_seq(5, direction = -1)
foundry10_pal_seq <- function(n, tint = "orange", direction = 1) {
  tint <- match.arg(tint, c("orange", "cyan"))
  ramp <- switch(tint,
    orange = grDevices::colorRampPalette(
      c("#F4E6E2", "#EBC5B9", "#D96F50", "#D14E1D"))(n),
    cyan   = grDevices::colorRampPalette(
      c("#E0ECED", "#AEC4C6", "#326A72", "#01646F"))(n)
  )
  if (direction == -1) rev(ramp) else ramp
}

#' Sequential foundry10 fill scale
#'
#' Continuous sequential scale using foundry10 brand tints. Per the foundry10
#' Visual Style Guide, use `tint = "orange"` for negative/concerning data and
#' `tint = "cyan"` for positive/favorable data.
#'
#' @param tint Character. `"orange"` (default) or `"cyan"`.
#' @param direction Integer. `1` = light to dark (default), `-1` = reversed.
#' @param ... Arguments passed to [ggplot2::scale_fill_gradientn()].
#' @return A ggplot2 scale object.
#' @export
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
#'   geom_tile() +
#'   scale_fill_foundry10_seq()
#' ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
#'   geom_tile() +
#'   scale_fill_foundry10_seq(tint = "cyan")
#' }
scale_fill_foundry10_seq <- function(tint = "orange", direction = 1, ...) {
  ggplot2::scale_fill_gradientn(
    colors = foundry10_pal_seq(256, tint, direction), ...)
}

#' Sequential foundry10 color scale
#'
#' @rdname scale_fill_foundry10_seq
#' @export
scale_color_foundry10_seq <- function(tint = "orange", direction = 1, ...) {
  ggplot2::scale_color_gradientn(
    colors = foundry10_pal_seq(256, tint, direction), ...)
}

#' @rdname scale_fill_foundry10_seq
#' @export
scale_colour_foundry10_seq <- scale_color_foundry10_seq

# -----------------------------------------------------------------------------
# 3b. DISCRETE SEQUENTIAL PALETTE (n = 2 to 4)
# -----------------------------------------------------------------------------

# Fixed lookup table using only documented brand tints -- no interpolation.
# Capped at n = 4 (the number of documented tint stops per color family).
# n=3 skips the 80% tint to preserve even perceptual spacing.

.palette_seq_d <- list(
  orange = list(
    "2" = c("#F4E6E2", "#D14E1D"),
    "3" = c("#F4E6E2", "#EBC5B9", "#D14E1D"),
    "4" = c("#F4E6E2", "#EBC5B9", "#D96F50", "#D14E1D")
  ),
  cyan = list(
    "2" = c("#E0ECED", "#01646F"),
    "3" = c("#E0ECED", "#AEC4C6", "#01646F"),
    "4" = c("#E0ECED", "#AEC4C6", "#326A72", "#01646F")
  )
)

#' Discrete sequential foundry10 palette
#'
#' Returns n hex colors from light to dark within a single brand color family,
#' using only the four documented brand tints. Capped at n = 4.
#'
#' @param n Integer between 2 and 4.
#' @param tint Character. `"orange"` (default) or `"cyan"`.
#' @param direction Integer. `1` = light to dark (default), `-1` = reversed.
#' @return Character vector of n hex codes.
#' @export
#' @examples
#' foundry10_pal_seq_d(4)
#' foundry10_pal_seq_d(3, tint = "cyan")
foundry10_pal_seq_d <- function(n, tint = "orange", direction = 1) {
  tint <- match.arg(tint, c("orange", "cyan"))
  if (!n %in% 2:4) {
    stop("foundry10_pal_seq_d() supports n = 2 to 4. Got n = ", n, ".")
  }
  pal <- .palette_seq_d[[tint]][[as.character(n)]]
  if (direction == -1) rev(pal) else pal
}

#' Discrete sequential foundry10 fill scale
#'
#' Uses fixed brand tint stops (no interpolation). Suitable for ordered
#' categorical variables with 2 to 4 levels.
#'
#' @param n Integer between 2 and 4 matching the number of categories.
#' @param tint Character. `"orange"` (default) or `"cyan"`.
#' @param direction Integer. `1` = light to dark (default), `-1` = reversed.
#' @param ... Arguments passed to [ggplot2::scale_fill_manual()].
#' @return A ggplot2 scale object.
#' @export
#' @examples
#' \dontrun{
#' library(ggplot2)
#' df <- data.frame(
#'   level = factor(c("Low", "Medium", "High", "Very high"),
#'                  levels = c("Low", "Medium", "High", "Very high")),
#'   value = c(12, 34, 56, 78)
#' )
#' ggplot(df, aes(x = level, y = value, fill = level)) +
#'   geom_col() +
#'   scale_fill_foundry10_seq_d(n = 4) +
#'   theme(legend.position = "none")
#' }
scale_fill_foundry10_seq_d <- function(n, tint = "orange", direction = 1, ...) {
  ggplot2::scale_fill_manual(
    values = foundry10_pal_seq_d(n, tint, direction), ...)
}

#' Discrete sequential foundry10 color scale
#'
#' @rdname scale_fill_foundry10_seq_d
#' @export
scale_color_foundry10_seq_d <- function(n, tint = "orange",
                                        direction = 1, ...) {
  ggplot2::scale_color_manual(
    values = foundry10_pal_seq_d(n, tint, direction), ...)
}

#' @rdname scale_fill_foundry10_seq_d
#' @export
scale_colour_foundry10_seq_d <- scale_color_foundry10_seq_d

# -----------------------------------------------------------------------------
# 4. DIVERGING PALETTE (orange low -> gray mid -> cyan high)
# -----------------------------------------------------------------------------

#' Diverging foundry10 palette function
#'
#' Returns n colors interpolated across the foundry10 diverging ramp:
#' orange (low/negative) -> light gray (neutral) -> cyan (high/positive).
#'
#' @param n Integer. Number of colors.
#' @param direction Integer. `1` = orange-to-cyan (default), `-1` = reversed.
#' @return Character vector of hex codes.
#' @export
#' @examples
#' foundry10_pal_div(7)
foundry10_pal_div <- function(n, direction = 1) {
  ramp <- grDevices::colorRampPalette(
    c("#D14E1D", "#E2E3E4", "#01646F"))(n)
  if (direction == -1) rev(ramp) else ramp
}

#' Diverging foundry10 fill scale
#'
#' Continuous diverging scale: orange (low/negative) -> gray (neutral) ->
#' cyan (high/positive), per the foundry10 Visual Style Guide. Use
#' `limits = c(-x, x)` to anchor the midpoint at zero.
#'
#' @param direction Integer. `1` = orange-to-cyan (default), `-1` = reversed.
#' @param ... Arguments passed to [ggplot2::scale_fill_gradientn()].
#' @return A ggplot2 scale object.
#' @export
#' @examples
#' \dontrun{
#' library(ggplot2)
#' set.seed(42)
#' df <- expand.grid(x = 1:10, y = 1:5)
#' df$value <- rnorm(50, sd = 15)
#' ggplot(df, aes(x, y, fill = value)) +
#'   geom_tile() +
#'   scale_fill_foundry10_div(limits = c(-30, 30))
#' }
scale_fill_foundry10_div <- function(direction = 1, ...) {
  ggplot2::scale_fill_gradientn(
    colors = foundry10_pal_div(256, direction), ...)
}

#' Diverging foundry10 color scale
#'
#' @rdname scale_fill_foundry10_div
#' @export
scale_color_foundry10_div <- function(direction = 1, ...) {
  ggplot2::scale_color_gradientn(
    colors = foundry10_pal_div(256, direction), ...)
}

#' @rdname scale_fill_foundry10_div
#' @export
scale_colour_foundry10_div <- scale_color_foundry10_div

# -----------------------------------------------------------------------------
# 4b. DISCRETE DIVERGING PALETTE (n = 2 to 9)
# -----------------------------------------------------------------------------

# Follows ColorBrewer/ggplot2 naming conventions:
#   "div" = diverging, "d" = discrete.
# Per style guide: orange = negative/"bad", cyan = positive/"good".
# All stops are documented brand colors -- no extrapolation.
#
# COLOR STOPS:
#   Orange side (most -> least negative):
#     #D14E1D  foundry10 orange (primary)
#     #D96F50  80% tint
#     #EBC5B9  20% tint
#     #F4E6E2  10% tint
#   Neutral (odd n only): #E2E3E4 brand light gray
#   Cyan side (least -> most positive):
#     #E0ECED  10% tint
#     #AEC4C6  20% tint
#     #326A72  80% tint
#     #01646F  foundry10 dark cyan (primary)

.palette_div_d <- list(
  "2" = c("#D14E1D", "#01646F"),
  "3" = c("#D14E1D", "#E2E3E4", "#01646F"),
  "4" = c("#D14E1D", "#EBC5B9", "#AEC4C6", "#01646F"),
  "5" = c("#D14E1D", "#EBC5B9", "#E2E3E4", "#AEC4C6", "#01646F"),
  "6" = c("#D14E1D", "#EBC5B9", "#F4E6E2", "#E0ECED", "#AEC4C6", "#01646F"),
  "7" = c("#D14E1D", "#EBC5B9", "#F4E6E2", "#E2E3E4",
           "#E0ECED", "#AEC4C6", "#01646F"),
  "8" = c("#D14E1D", "#D96F50", "#EBC5B9", "#F4E6E2",
           "#E0ECED", "#AEC4C6", "#326A72", "#01646F"),
  "9" = c("#D14E1D", "#D96F50", "#EBC5B9", "#F4E6E2", "#E2E3E4",
           "#E0ECED", "#AEC4C6", "#326A72", "#01646F")
)

#' Discrete diverging foundry10 palette
#'
#' Returns n hex colors ordered from most negative (orange) through neutral
#' (gray, odd n only) to most positive (cyan). Follows ColorBrewer "div"
#' conventions. Suitable for Likert-scale data and any ordered categorical
#' variable with a meaningful midpoint.
#'
#' @param n Integer between 2 and 9.
#' @param direction Integer. `1` = negative-to-positive (default),
#'   `-1` = reversed.
#' @return Character vector of n hex codes.
#' @export
#' @examples
#' foundry10_pal_div_d(5)
#' foundry10_pal_div_d(7)
foundry10_pal_div_d <- function(n, direction = 1) {
  if (!n %in% 2:9) {
    stop("foundry10_pal_div_d() supports n = 2 to 9. Got n = ", n, ".")
  }
  pal <- .palette_div_d[[as.character(n)]]
  if (direction == -1) rev(pal) else pal
}

#' Discrete diverging foundry10 fill scale
#'
#' Uses fixed brand color stops (no interpolation). Suitable for ordered
#' categorical data with a meaningful midpoint, such as Likert-scale survey
#' responses. Supports 2 to 9 response levels.
#'
#' Factor levels must run from most negative to most positive for colors to
#' map correctly.
#'
#' @param n Integer between 2 and 9 matching the number of response levels.
#' @param direction Integer. `1` = negative-to-positive (default),
#'   `-1` = reversed.
#' @param ... Arguments passed to [ggplot2::scale_fill_manual()].
#' @return A ggplot2 scale object.
#' @export
#' @examples
#' \dontrun{
#' library(ggplot2)
#' set.seed(1)
#' df <- data.frame(
#'   response = factor(
#'     sample(c("Strongly disagree", "Disagree", "Neutral",
#'              "Agree", "Strongly agree"), 200, replace = TRUE),
#'     levels = c("Strongly disagree", "Disagree", "Neutral",
#'                "Agree", "Strongly agree")
#'   )
#' )
#' ggplot(df, aes(x = response, fill = response)) +
#'   geom_bar() +
#'   scale_fill_foundry10_div_d(n = 5) +
#'   theme(legend.position = "none")
#' }
scale_fill_foundry10_div_d <- function(n, direction = 1, ...) {
  ggplot2::scale_fill_manual(
    values = foundry10_pal_div_d(n, direction), ...)
}

#' Discrete diverging foundry10 color scale
#'
#' @rdname scale_fill_foundry10_div_d
#' @export
scale_color_foundry10_div_d <- function(n, direction = 1, ...) {
  ggplot2::scale_color_manual(
    values = foundry10_pal_div_d(n, direction), ...)
}

#' @rdname scale_fill_foundry10_div_d
#' @export
scale_colour_foundry10_div_d <- scale_color_foundry10_div_d

# -----------------------------------------------------------------------------
# 5. THEME
# -----------------------------------------------------------------------------

#' Bold foundry10 ggplot2 theme
#'
#' A clean, branded ggplot2 theme using the foundry10 visual identity:
#' Helvetica Neue typography, brand color accents, and generous whitespace
#' per the foundry10 Visual Style Guide.
#'
#' @param base_size Numeric. Base font size in pt (default 12). Use 14-16 for
#'   slides, 10-11 for dense layouts.
#' @param base_family Character. Font family. Defaults to Helvetica Neue if
#'   available, then Arial, then `"sans"`.
#' @param grid Logical. Show subtle horizontal grid lines (default `TRUE`).
#'   Set `FALSE` for heatmaps and tile charts.
#' @param strip_fill Character. Facet strip background: `"white"` (default),
#'   `"orange"`, `"cyan"`, or `"black"`. Text is automatically white or black
#'   for contrast.
#' @return A ggplot2 theme object.
#' @export
#' @examples
#' \dontrun{
#' library(ggplot2)
#' mpg6 <- mpg[mpg$class != "minivan", ]
#' ggplot(mpg6, aes(displ, hwy, color = class)) +
#'   geom_point(size = 2.5, alpha = 0.85) +
#'   scale_color_foundry10_d() +
#'   theme_foundry10() +
#'   labs(title    = "Engine displacement vs highway MPG",
#'        subtitle = "By vehicle class",
#'        x = "Displacement (L)", y = "Highway MPG")
#' }
theme_foundry10 <- function(base_size   = 12,
                            base_family = .foundry10_font(),
                            grid        = TRUE,
                            strip_fill  = "white") {

  strip_fill      <- match.arg(strip_fill,
                                c("orange", "cyan", "black", "white"))
  strip_fill_hex  <- switch(strip_fill,
                            orange = "#D14E1D",
                            cyan   = "#01646F",
                            black  = "#111111",
                            white  = "#FFFFFF")
  strip_text_color <- if (strip_fill == "white") "#111111" else "#FFFFFF"

  half  <- base_size / 2
  third <- base_size / 3

  ggplot2::theme(

    text = ggplot2::element_text(family = base_family,
                                 color  = "#111111",
                                 size   = base_size),

    # Title: 1.75x base -- exceeds style guide 150% heading minimum
    plot.title = ggplot2::element_text(
      family = base_family, face = "bold",
      size   = base_size * 1.75, color = "#111111",
      margin = ggplot2::margin(b = half), hjust = 0),
    plot.title.position = "plot",

    # Subtitle: 1.2x -- distinct three-level hierarchy: title > subtitle > labels
    plot.subtitle = ggplot2::element_text(
      family = base_family, face = "plain",
      size   = base_size * 1.2, color = "#575857",
      margin = ggplot2::margin(b = base_size), hjust = 0),

    plot.caption = ggplot2::element_text(
      family = base_family, size = base_size * 0.8,
      color  = "#939598", hjust = 1,
      margin = ggplot2::margin(t = half)),
    plot.caption.position = "plot",

    plot.background  = ggplot2::element_rect(fill = "#FFFFFF", color = NA),
    panel.background = ggplot2::element_rect(fill = "#FFFFFF", color = NA),

    # Light gray border on all sides
    panel.border = ggplot2::element_rect(color = "#E2E3E4",
                                         linewidth = 0.5),

    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank(),
    panel.grid.minor.y = ggplot2::element_blank(),
    panel.grid.major.y = if (grid) {
      ggplot2::element_line(color = "#E2E3E4", linewidth = 0.35,
                            linetype = "solid")
    } else {
      ggplot2::element_blank()
    },

    # Axes use #575857 (brand dark gray) not orange: #D14E1D fails WCAG AA
    # contrast at small text sizes per the foundry10 Visual Style Guide.
    axis.title = ggplot2::element_text(
      family = base_family, face = "bold",
      size   = base_size * 0.9, color = "#575857"),
    axis.title.x = ggplot2::element_text(
      margin = ggplot2::margin(t = half)),
    axis.title.y = ggplot2::element_text(
      margin = ggplot2::margin(r = half)),

    axis.text = ggplot2::element_text(
      family = base_family, size = base_size * 0.85,
      color  = "#575857"),
    axis.text.x = ggplot2::element_text(
      margin = ggplot2::margin(t = third)),
    axis.text.y = ggplot2::element_text(
      margin = ggplot2::margin(r = third)),

    axis.ticks        = ggplot2::element_line(color = "#E2E3E4",
                                              linewidth = 0.35),
    axis.ticks.length = ggplot2::unit(4, "pt"),

    legend.position   = "bottom",
    legend.direction  = "horizontal",
    legend.background = ggplot2::element_rect(fill = NA, color = NA),
    legend.key        = ggplot2::element_rect(fill = NA, color = NA),
    legend.key.size   = ggplot2::unit(base_size * 0.9, "pt"),
    legend.spacing.x  = ggplot2::unit(half, "pt"),
    legend.spacing.y  = ggplot2::unit(third, "pt"),
    legend.margin     = ggplot2::margin(t = half),

    legend.title = ggplot2::element_text(
      family = base_family, face = "bold",
      size   = base_size * 0.85, color = "#111111"),
    legend.text = ggplot2::element_text(
      family = base_family,
      size   = base_size * 0.85, color = "#575857"),

    # Facet strips: strip_fill controls fill; text contrast auto-selected
    strip.background = ggplot2::element_rect(fill = strip_fill_hex,
                                             color = NA),
    strip.text = ggplot2::element_text(
      family = base_family, face = "bold",
      size   = base_size * 0.9, color = strip_text_color,
      margin = ggplot2::margin(t = third + 2, b = third + 2,
                               l = half,      r = half)),

    # Top and left wider for left-aligned title breathing room.
    # NOTE: axis text at 0.85x base = 10.2pt at default (base_size = 12),
    # above the style guide 9pt print minimum. Set base_size >= 10.6 for print.
    plot.margin = ggplot2::margin(t = base_size * 1.5, r = base_size,
                                  b = base_size, l = base_size * 1.25)
  )
}

#' Preview the foundry10 theme with a representative sample plot
#'
#' Renders a faceted scatter plot using the built-in `mpg` dataset,
#' showcasing the full theme: title, subtitle, caption, legend, facet strips,
#' grid lines, axis labels, and the discrete color palette.
#'
#' @param base_size Numeric. Passed to [theme_foundry10()] (default 12).
#' @param strip_fill Character. Passed to [theme_foundry10()]: `"white"`
#'   (default), `"orange"`, `"cyan"`, or `"black"`.
#' @param grid Logical. Passed to [theme_foundry10()] (default `TRUE`).
#' @return A ggplot2 object.
#' @export
#' @examples
#' \dontrun{
#' preview_theme_foundry10()
#' preview_theme_foundry10(strip_fill = "orange")
#' preview_theme_foundry10(base_size = 14, grid = FALSE)
#' }
preview_theme_foundry10 <- function(base_size   = 12,
                                    strip_fill  = "white",
                                    grid        = TRUE) {
  mpg6 <- ggplot2::mpg[ggplot2::mpg$class != "minivan", ]
  mpg6$drv_label <- factor(
    mpg6$drv,
    levels = c("f", "r", "4"),
    labels = c("Front-wheel", "Rear-wheel", "4-wheel")
  )
  ggplot2::ggplot(mpg6, ggplot2::aes(.data$displ, .data$hwy,
                                     color = .data$class)) +
    ggplot2::geom_point(size = 2, alpha = 0.8) +
    ggplot2::facet_wrap(~ .data$drv_label) +
    scale_color_foundry10_d() +
    theme_foundry10(base_size   = base_size,
                    strip_fill  = strip_fill,
                    grid        = grid) +
    ggplot2::labs(
      title    = "Engine displacement vs. highway MPG",
      subtitle = "By drive type -- passenger vehicles, 1999 & 2008",
      caption  = "Source: EPA fuel economy data (ggplot2::mpg)",
      color    = "Vehicle class",
      x        = "Displacement (L)",
      y        = "Highway MPG"
    )
}
