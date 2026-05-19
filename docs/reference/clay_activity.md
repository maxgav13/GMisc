# Clay activity chart

Draws a clay activity chart—plasticity index vs clay
percentage—following. Reference lines for eight activity values are
fitted by linear regression on built-in data and plotted as styled
lines. Activity values are labelled directly on the chart and/or shown
in a legend.

## Usage

``` r
clay_activity(
  show.legend = T,
  legend.pos = "top",
  show.labels = T,
  labels.pos = "high",
  lang = "en"
)
```

## Arguments

- show.legend:

  Logical. If `TRUE` (default) a legend mapping line type to activity
  value is shown; if `FALSE` the legend is suppressed.

- legend.pos:

  Character. Position of the legend passed to
  `theme(legend.position = ...)`. Default `"top"`.

- show.labels:

  Logical. If `TRUE` (default) activity values are annotated directly on
  the reference lines; if `FALSE`\` labels are omitted.

- labels.pos:

  Character. Position of the direct labels: `"high"` (default) places
  labels near the top of the plot; `"low"` places labels near the
  bottom.

- lang:

  Character. Language for axis titles: `"en"` (default) for English,
  `"es"` for Spanish.

## Value

A ggplot object with clay percentage on the x-axis (0–100) and
plasticity index on the y-axis (0–500), containing the eight reference
activity lines and optional direct labels.

## References

Das, B. M., & Sobhan, K. (2018). Principles of geotechnical engineering
(Ninth edition). Cengage Learning.

## See also

Other classification plots, soil mechanics:
[`Casagrande_chart()`](https://maxgav13.github.io/GMisc/reference/Casagrande_chart.md),
[`soil_collapse()`](https://maxgav13.github.io/GMisc/reference/soil_collapse.md),
[`soil_expansion()`](https://maxgav13.github.io/GMisc/reference/soil_expansion.md),
[`soil_swelling()`](https://maxgav13.github.io/GMisc/reference/soil_swelling.md),
[`ternary_shepard()`](https://maxgav13.github.io/GMisc/reference/ternary_shepard.md),
[`ternary_usda()`](https://maxgav13.github.io/GMisc/reference/ternary_usda.md)

## Examples

``` r
# Reference chart only (no sample data)
clay_activity()


# Spanish axis labels, no legend
clay_activity(show.legend = FALSE, lang = "es")
```
