# Soil swelling potential chart

Draws a soil swelling potential chart—clay activity vs clay
percentage—with three boundary curves separating zones of low, medium,
high, and very high swelling potential. Boundary curves are drawn; each
zone is labelled directly on the chart with.

## Usage

``` r
soil_swelling(lang = "en", clay.max = 70)
```

## Arguments

- lang:

  Character. Language for axis and legend titles: `"en"` (default) for
  English, `"es"` for Spanish.

- clay.max:

  Numeric. Maximum value for the clay percentage axis. Default is 70.

## Value

A ggplot object with clay percentage on the x-axis and clay activity on
the y-axis, containing the three boundary curves coloured by expansion
level (`"1.5%"`, `"5%"`, `"25%"`) and their zone labels (`"Low"` /
`"Medium"` / `"High"` / `"Very high"`).

## References

Holtz, R. D., Kovacs, W. D., & Sheahan, T. C. (2023). An Introduction to
Geotechnical Engineering (3rd ed.). Pearson Education.

## See also

Other classification plots, soil mechanics:
[`Casagrande_chart()`](https://maxgav13.github.io/GMisc/reference/Casagrande_chart.md),
[`clay_activity()`](https://maxgav13.github.io/GMisc/reference/clay_activity.md),
[`soil_collapse()`](https://maxgav13.github.io/GMisc/reference/soil_collapse.md),
[`soil_expansion()`](https://maxgav13.github.io/GMisc/reference/soil_expansion.md),
[`ternary_shepard()`](https://maxgav13.github.io/GMisc/reference/ternary_shepard.md),
[`ternary_usda()`](https://maxgav13.github.io/GMisc/reference/ternary_usda.md)

## Examples

``` r
# English labels (default)
soil_swelling()


dat = data.frame(
  clay = c(20, 40, 60),
  activity = c(.5, 1, 2)
)
# Spanish labels
soil_swelling(lang = "es") +
ggplot2::geom_point(data = dat,
                    ggplot2::aes(x = clay, y = activity),
                    size = 3, inherit.aes = FALSE)
```
