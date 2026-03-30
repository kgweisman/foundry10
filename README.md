# foundry10

[![R-CMD-check](https://github.com/kgweisman/foundry10/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kgweisman/foundry10/actions/workflows/R-CMD-check.yaml)

A branded ggplot2 theme and color palette suite for [foundry10](https://www.foundry10.org) data visualizations.

## Installation

```r
# install.packages("remotes")
remotes::install_github("kgweisman/foundry10")
```

## Quick start

```r
library(ggplot2)
library(foundry10)

mpg6 <- mpg[mpg$class != "minivan", ]
ggplot(mpg6, aes(displ, hwy, color = class)) +
  geom_point(size = 2.5, alpha = 0.85) +
  scale_color_foundry10_d() +
  theme_foundry10() +
  labs(title    = "Engine displacement vs highway MPG",
       subtitle = "By vehicle class",
       x = "Displacement (L)", y = "Highway MPG")
```

See `vignette("getting-started", package = "foundry10")` for full documentation.

## Color scales

| Type | Function |
|---|---|
| Qualitative (categorical) | `scale_fill_foundry10_d()` |
| Sequential | `scale_fill_foundry10_seq(tint = "orange" or "cyan")` |
| Sequential discrete (2-4 levels) | `scale_fill_foundry10_seq_d(n)` |
| Diverging continuous | `scale_fill_foundry10_div()` |
| Diverging discrete / Likert (2-9 levels) | `scale_fill_foundry10_div_d(n)` |

All scales have `fill` and `color` variants, `scale_colour_` aliases, and accept `direction = -1` to reverse.

Per the foundry10 Visual Style Guide: **orange = negative/bad**, **cyan = positive/good**.

## Theme options

```r
theme_foundry10(
  base_size   = 12,         # font size in pt; use 14-16 for slides
  grid        = TRUE,       # set FALSE for heatmaps
  strip_color = "white"     # facet strip: "white", "orange", "cyan", "black"
)
```

Preview the theme: `preview_theme_foundry10()`

## Color utilities

```r
foundry10_color("orange_foundry10")      # "#D14E1D"
foundry10_palette("primary")             # named vector of primary colors
preview_foundry10_palette("secondary")   # renders a color swatch
```

---

Created March 2026 by Kara Weisman with assistance from ClaudeAI.
