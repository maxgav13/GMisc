# Proctor Compaction Test Analysis

Analyses results from a standard or modified Proctor compaction test.
Fits a LOESS curve to find optimum moisture content and maximum dry unit
weight, builds zero-air-void (ZAV) lines for selected saturation levels,
and returns summary statistics plus two diagnostic plots.

## Usage

``` r
compaction_test(
  w,
  gamma,
  Gs = 2.68,
  S_lines = c(70, 80, 90, 100),
  target_compaction = 0.95,
  lang = c("en", "es")
)
```

## Arguments

- w:

  Numeric vector. Water content values (percent) measured in the test.

- gamma:

  Numeric vector. Moist unit weight values (kN/m³) corresponding to each
  `w` value. Must be the same length as `w`.

- Gs:

  Numeric scalar. Specific gravity of soil solids. Default is `2.68`.

- S_lines:

  Numeric vector. Saturation levels (percent) for which ZAV lines are
  drawn on the compaction curve plot. Default is `c(70, 80, 90, 100)`.

- target_compaction:

  Numeric scalar. Relative compaction target as a proportion (e.g.
  `0.95` for 95 percent). Used to draw the reference horizontal line on
  the compaction curve. Default is `0.95`.

- lang:

  Character scalar. Language for plot labels. Either `"en"` (default)
  for English or `"es"` for Spanish.

## Value

A list with:

- `proctor`:

  Tibble with the input data plus computed `gamma_d`, void ratio `e`,
  and degree of saturation `S` for each test point.

- `optimum`:

  One-row tibble with the optimum water content (`w`) and maximum dry
  unit weight (`gamma_d`) obtained from the LOESS fit.

- `opt_properties`:

  Tibble with LOESS-predicted values of `gamma`, `gamma_d`, `e`, and `S`
  at the optimum water content.

- `compaction_plot`:

  A `ggplot2` object showing the compaction curve, ZAV lines, optimum
  point, and the `target_compaction` reference line.

- `properties_plot`:

  A faceted `ggplot2` object showing how `gamma`, `gamma_d`, `e`, and
  `S` vary with water content.

## See also

Other soil mechanics:
[`Casagrande_LL()`](https://maxgav13.github.io/GMisc/reference/Casagrande_LL.md),
[`DS_plot()`](https://maxgav13.github.io/GMisc/reference/direct_shear_plots.md),
[`Mohr_Circle()`](https://maxgav13.github.io/GMisc/reference/Mohr_Circle.md),
[`bearing_capacity()`](https://maxgav13.github.io/GMisc/reference/bearing_capacity.md),
[`fallcone_limits()`](https://maxgav13.github.io/GMisc/reference/fallcone_limits.md),
[`induced_stress()`](https://maxgav13.github.io/GMisc/reference/induced_stress.md),
[`insitu_stress()`](https://maxgav13.github.io/GMisc/reference/insitu_stress.md),
[`particle_size_graph()`](https://maxgav13.github.io/GMisc/reference/particle_size_graph.md)

## Examples

``` r
w     <- c(5, 7, 9.5, 11.8, 14.1, 17)
gamma <- c(18.46, 19.64, 20.85, 21.03, 20.84, 20.54)

res <- compaction_test(w, gamma, Gs = 2.68)
res$optimum
#> # A tibble: 1 × 2
#>       w gamma_d
#>   <dbl>   <dbl>
#> 1   9.8    19.1
res$compaction_plot
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

res$properties_plot
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'


# Spanish labels
res_es <- compaction_test(w, gamma, Gs = 2.68, lang = "es")
res_es$compaction_plot
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

```
