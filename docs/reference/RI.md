# Intraclass Correlation Coefficient for layer boundary determination

Calculates the intraclass correlation coefficient (RI) for a perforation
log, and shows the locations where this coefficient is higher than 0.7
and 0.8. The coefficient is used to find layer boundaries in a
perforation log.

## Usage

``` r
RI(data, k = 6)
```

## Arguments

- data:

  data A data frame containing the depth in the first column, and the
  value of interest in the second column

- k:

  The window length for the number of data points to include in the
  calculation of RI. Always and even (par) number. Larger values make
  the resulting statistic smoother

## Value

ggplot and plotly objects showing the RI statistic and lines marking the
critical values of 0.7 and 0.8, and suggested boundaries

## Details

The example data given is intended to show the structure needed for
input data. The user should follow this structure, which in general
corresponds with a data frame with a sequence in the first column and
the observed/measured values in the second column

## References

Mora, R. (2013). Uso de metodos estadisticos para la determinacion de
capas homogeneas de suelos volcanicos en un sitio de las laderas del
Volcan Irazu, Cartago, Costa Rica. - Rev. Geol. Amer. Central, 49:
101-108.

## See also

Other layer detection:
[`T2_stat()`](https://maxgav13.github.io/GMisc/reference/T2_stat.md),
[`T_stat()`](https://maxgav13.github.io/GMisc/reference/T_stat.md),
[`cp_aic_eta()`](https://maxgav13.github.io/GMisc/reference/cp_aic_eta.md),
[`cp_layers()`](https://maxgav13.github.io/GMisc/reference/cp_layers.md),
[`layers_window()`](https://maxgav13.github.io/GMisc/reference/layers_window.md),
[`sc_bic()`](https://maxgav13.github.io/GMisc/reference/sc_bic.md),
[`sc_layers()`](https://maxgav13.github.io/GMisc/reference/sc_layers.md)

## Examples

``` r
RI(DPM_data, k = 6)
#> Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
#> ℹ Please use tidy evaluation idioms with `aes()`.
#> ℹ See also `vignette("ggplot2-in-packages")` for more information.
#> ℹ The deprecated feature was likely used in the GMisc package.
#>   Please report the issue at <https://github.com/maxgav13/GMisc/issues>.
#> $GGPLOT

#> 
#> $PLOTLY
#> 
#> $Bounds.7
#> [1] 3.8 7.5 7.9 8.7 9.4 9.7
#> 
#> $Bounds.8
#> [1] 7.9 9.4 9.7
#> 
```
