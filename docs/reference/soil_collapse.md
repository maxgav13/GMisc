# Soil collapse and expansion potential chart

Draws a soil collapse and expansion potential chart—dry density vs
liquid limit—with two boundary curves separating zones of collapse and
expansion potential (low, medium, high, and very high). The boundary
curves are drawn; three vertical dashed reference lines are drawn at
liquid limits of 40, 50, and 63. Each zone is labelled directly on the
chart.

## Usage

``` r
soil_collapse(lang = "en", ll.max = 100)
```

## Arguments

- lang:

  Character. Language for axis titles: `"en"` (default) for English,
  `"es"` for Spanish.

- ll.max:

  Numeric. Maximum value for the liquid limit axis. Default is 100.

## Value

A ggplot object with liquid limit on the x-axis and dry density on the
y-axis, containing the two boundary curves, three dashed reference lines
at LL = 40, 50, and 63, and zone labels (`"Collapse"` / `"Expansion"`).

## References

Holtz, R. D., Kovacs, W. D., & Sheahan, T. C. (2023). An Introduction to
Geotechnical Engineering (3rd ed.). Pearson Education.

## See also

Other classification plots, soil mechanics:
[`Casagrande_chart()`](https://maxgav13.github.io/GMisc/reference/Casagrande_chart.md),
[`clay_activity()`](https://maxgav13.github.io/GMisc/reference/clay_activity.md),
[`soil_expansion()`](https://maxgav13.github.io/GMisc/reference/soil_expansion.md),
[`soil_swelling()`](https://maxgav13.github.io/GMisc/reference/soil_swelling.md),
[`ternary_shepard()`](https://maxgav13.github.io/GMisc/reference/ternary_shepard.md),
[`ternary_usda()`](https://maxgav13.github.io/GMisc/reference/ternary_usda.md)

## Examples

``` r
# English labels (default)
soil_collapse()


dat = data.frame(
  ll = c(20, 40, 60, 80),
  dry = c(1.5, 1.15, 1.2, 1.0)
)

# Spanish labels
soil_collapse(lang = "es") +
 ggplot2::geom_point(data = dat,
                     ggplot2::aes(x = ll, y = dry),
                     size = 3, inherit.aes = FALSE)
```
