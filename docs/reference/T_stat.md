# T-statistic for layer boundary determination

Calculates the T-statistic and Cohen's d effect size for a perforation
log. The coefficient is used to find layer boundaries in a perforation
log.

## Usage

``` r
T_stat(data, k = 6)
```

## Arguments

- data:

  A data frame containing the depth of perforation in the first column,
  and the value of interest in the second column

- k:

  The window length for the number of data points to include in the
  calculation of T-statistic. Always and even (par) number. Larger
  values make the resulting statistic smoother

## Value

ggplot and plotly objects showing the Cohen's d statistic and lines
marking the Cohen's U3 values for 0.95, 0.99, and 0.999, and suggested
boundaries

## Details

The example data given is intended to show the structure needed for
input data. The user should follow this structure, which in general
corresponds with a data frame with a sequence in the first column and
the observed/measured values in the second column

## References

Mora, R. (2013). Uso de metodos estadisticos para la identifacion de
capas de suelos volcanicos con el ensayo del cono de pentracion en los
terrenos de la Universidad de Costa Rica, Montes de Oca, San Jose, Costa
Rica. - Rev. Geol. Amer. Central, 49: 109-120.

## See also

Other layer detection:
[`RI()`](https://maxgav13.github.io/GMisc/reference/RI.md),
[`T2_stat()`](https://maxgav13.github.io/GMisc/reference/T2_stat.md),
[`cp_aic_eta()`](https://maxgav13.github.io/GMisc/reference/cp_aic_eta.md),
[`cp_layers()`](https://maxgav13.github.io/GMisc/reference/cp_layers.md),
[`layers_window()`](https://maxgav13.github.io/GMisc/reference/layers_window.md),
[`sc_bic()`](https://maxgav13.github.io/GMisc/reference/sc_bic.md),
[`sc_layers()`](https://maxgav13.github.io/GMisc/reference/sc_layers.md)

## Examples

``` r
T_stat(DPM_data, k = 6)
#> $GGPLOT

#> 
#> $PLOTLY
#> 
#> $Bounds.95
#>  [1] 0.6 1.4 1.8 2.7 3.3 3.8 4.4 5.7 7.5 7.9 8.7 9.4 9.7
#> 
#> $Bounds.99
#> [1] 0.6 3.8 5.7 7.5 7.9 8.7 9.4 9.7
#> 
#> $Bounds.999
#> [1] 3.8 7.9 9.4 9.7
#> 
```
