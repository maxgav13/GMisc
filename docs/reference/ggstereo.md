# Create a stereographic projection base plot

Builds an empty stereonet (equal-angle or equal-area) using ggplot2,
with an optional great-circle and small-circle grid. The returned ggplot
object can be extended with additional layers.

## Usage

``` r
ggstereo(wulff = F, show.grid = TRUE, grid.col = "grey50", tl = 0.05)
```

## Arguments

- wulff:

  Logical. If `TRUE` an equal-angle (Wulff) net is used; if `FALSE`
  (default) an equal-area (Schmidt) net is used.

- show.grid:

  Logical. If `TRUE` (default) a great-circle and small-circle grid is
  drawn.

- grid.col:

  Character. Colour of the grid lines. Default `"grey50"`.

- tl:

  Numeric. Line width (`linewidth`) of the grid lines. Default `0.05`.

## Value

A ggplot object containing the stereonet base plot.

## Examples

``` r
ggstereo()

ggstereo(wulff = TRUE, grid.col = "grey70", tl = 0.1)

ggstereo(show.grid = FALSE)
```
