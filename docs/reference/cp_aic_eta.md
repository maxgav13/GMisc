# Changepoint analysis of different layer models

Calculates and plots the AIC and eta-squared statistics for diferent
layer models based on a changepoint analysis using the mean and
variance.

## Usage

``` r
cp_aic_eta(data, m = 10, nl = 3)
```

## Arguments

- data:

  A data frame containing the location variable (depth or distance) in
  the first column, and the value of interest in the second column

- m:

  The maximum number of breakpoints (# layers - 1) to look for

- nl:

  The minimum number of points per layer to be considered

## Value

A ggplot and plotly objects showing the AIC and eta-squared statistics,
and a data frame with all the data and possible layer models

## Details

The example data given is intended to show the structure needed for
input data. The user should follow this structure, which in general
corresponds with a data frame with a sequence in the first column and
the observed/measured values in the second column

## See also

Other layer detection:
[`RI()`](https://maxgav13.github.io/GMisc/reference/RI.md),
[`T2_stat()`](https://maxgav13.github.io/GMisc/reference/T2_stat.md),
[`T_stat()`](https://maxgav13.github.io/GMisc/reference/T_stat.md),
[`cp_layers()`](https://maxgav13.github.io/GMisc/reference/cp_layers.md),
[`layers_window()`](https://maxgav13.github.io/GMisc/reference/layers_window.md),
[`sc_bic()`](https://maxgav13.github.io/GMisc/reference/sc_bic.md),
[`sc_layers()`](https://maxgav13.github.io/GMisc/reference/sc_layers.md)

## Examples

``` r
cp_aic_eta(DPM_data, m = 10, nl = 3)
#> $GGPLOT

#> 
#> $PLOTLY
#> 
#> $Data
#> # A tibble: 101 × 12
#>     Prof Blows B1      B2      B3      B4    B5    B6    B7    B8    B9    B10  
#>    <dbl> <int> <fct>   <fct>   <fct>   <fct> <fct> <fct> <fct> <fct> <fct> <fct>
#>  1   0       0 [0,2.7] [0,2.7] [0,2.7] [0,2… [0,2… [0,2… [0,2… [0,2… [0,2… [0,2…
#>  2   0.1    10 [0,2.7] [0,2.7] [0,2.7] [0,2… [0,2… [0,2… [0,2… [0,2… [0,2… [0,2…
#>  3   0.2     8 [0,2.7] [0,2.7] [0,2.7] [0,2… [0,2… [0,2… [0,2… [0,2… [0,2… [0,2…
#>  4   0.3     5 [0,2.7] [0,2.7] [0,2.7] [0,2… [0,2… [0,2… [0,2… [0,2… [0,2… [0,2…
#>  5   0.4     5 [0,2.7] [0,2.7] [0,2.7] [0,2… [0,2… [0,2… [0,2… [0,2… [0,2… [0,2…
#>  6   0.5     9 [0,2.7] [0,2.7] [0,2.7] [0,2… [0,2… [0,2… [0,2… [0,2… [0,2… [0,2…
#>  7   0.6     6 [0,2.7] [0,2.7] [0,2.7] [0,2… [0,2… [0,2… [0,2… [0,2… [0,2… [0,2…
#>  8   0.7     3 [0,2.7] [0,2.7] [0,2.7] [0,2… [0,2… [0,2… [0,2… [0,2… [0,2… [0,2…
#>  9   0.8     3 [0,2.7] [0,2.7] [0,2.7] [0,2… [0,2… [0,2… [0,2… [0,2… [0,2… [0,2…
#> 10   0.9     3 [0,2.7] [0,2.7] [0,2.7] [0,2… [0,2… [0,2… [0,2… [0,2… [0,2… [0,2…
#> # ℹ 91 more rows
#> 
```
