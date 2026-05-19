# Soil expansion potential chart

Draws a soil expansion potential chart—plasticity index vs clay
percentage—with four boundary curves that separate the zones of low,
medium, high, and very high expansion potential.

## Usage

``` r
soil_expansion(lang = "en")
```

## Arguments

- lang:

  Character. Language for axis titles: `"en"` (default) for English,
  `"es"` for Spanish.

## Value

A ggplot object with clay percentage on the x-axis and plasticity index
on the y-axis, containing the four expansion potential boundary curves
and their zone labels.

## References

Holtz, R. D., Kovacs, W. D., & Sheahan, T. C. (2023). An Introduction to
Geotechnical Engineering (3rd ed.). Pearson Education.

## See also

Other classification plots, soil mechanics:
[`Casagrande_chart()`](https://maxgav13.github.io/GMisc/reference/Casagrande_chart.md),
[`clay_activity()`](https://maxgav13.github.io/GMisc/reference/clay_activity.md),
[`soil_collapse()`](https://maxgav13.github.io/GMisc/reference/soil_collapse.md),
[`soil_swelling()`](https://maxgav13.github.io/GMisc/reference/soil_swelling.md),
[`ternary_shepard()`](https://maxgav13.github.io/GMisc/reference/ternary_shepard.md),
[`ternary_usda()`](https://maxgav13.github.io/GMisc/reference/ternary_usda.md)

## Examples

``` r
# English labels (default)
soil_expansion()


dat = data.frame(
  clay = c(20, 40, 60),
  pi = c(10, 23, 40)
)
# Spanish labels
soil_expansion(lang = "es") +
ggplot2::geom_point(data = dat,
                     ggplot2::aes(x = clay, y = pi),
                     size = 3, inherit.aes = FALSE)
```
