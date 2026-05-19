# Casagrande plasticity chart (LL vs PI)

Create the Casagrande plasticity chart plotting Liquid Limit (LL) vs
Plasticity Index (PI). The plot shows the A-line and U-line, a polygon
representing typical classification zones, and labels for standard
regions used in soil plasticity interpretation.

## Usage

``` r
Casagrande_chart(ll.max = 110, pi.max = 90, lang = "en")
```

## Arguments

- ll.max:

  numeric. Maximum value for the liquid limit (LL) axis (x-axis).
  Default 110.

- pi.max:

  numeric. Maximum value for the plasticity index (PI) axis (y-axis).
  Default 90.

- lang:

  character. Label language; `'es'` for Spanish or `'en'` for English.
  Default `'en'`.

## Value

A ggplot2 object (class 'gg' and 'ggplot').

## Details

The function computes the A-line and U-line values for LL vs PI and
draws a polygon approximating the CL-ML zone. It returns a ggplot2
object so the caller can further customize the plot (titles, themes,
annotations) or save it.

## References

Holtz, R. D., Kovacs, W. D. & Sheahan, T. C. (2011). An Introduction to
Geotechnical Engineering. Prentice Hall.

## See also

Other classification plots, soil mechanics:
[`clay_activity()`](https://maxgav13.github.io/GMisc/reference/clay_activity.md),
[`soil_collapse()`](https://maxgav13.github.io/GMisc/reference/soil_collapse.md),
[`soil_expansion()`](https://maxgav13.github.io/GMisc/reference/soil_expansion.md),
[`soil_swelling()`](https://maxgav13.github.io/GMisc/reference/soil_swelling.md),
[`ternary_shepard()`](https://maxgav13.github.io/GMisc/reference/ternary_shepard.md),
[`ternary_usda()`](https://maxgav13.github.io/GMisc/reference/ternary_usda.md)

## Examples

``` r
Casagrande_chart()
#> Warning: Removed 42 rows containing missing values or values outside the scale range
#> (`geom_line()`).

Casagrande_chart(ll.max = 80, pi.max = 60) + ggplot2::ggtitle('Casagrande chart')
#> Warning: Removed 42 rows containing missing values or values outside the scale range
#> (`geom_line()`).

soil_df = data.frame(ll = c(67, 58, 77, 51),
                     pi = c(39, 19, 22, 20),
                     sample = paste0('M', 1:4))
Casagrande_chart(lang = 'es') +
ggplot2::geom_point(ggplot2::aes(ll, pi, shape = sample, color = sample), soil_df, size = 3) +
ggplot2::scale_shape_manual(values = 1:4)
#> Warning: Removed 42 rows containing missing values or values outside the scale range
#> (`geom_line()`).

```
