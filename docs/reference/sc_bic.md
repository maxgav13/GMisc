# Structural change analysis of different layer models

Performs a structural change analysis on diferent layer models to find
points where a change in the sequence might be occuring.

## Usage

``` r
sc_bic(x, h = 0.1)
```

## Arguments

- x:

  A data frame containing the location variable (depth or distance) in
  the first column, and the value of interest in the second column

- h:

  Percent of data points per layer

## Value

A ggplot and plotly objects showing the BIC statistic

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
[`cp_aic_eta()`](https://maxgav13.github.io/GMisc/reference/cp_aic_eta.md),
[`cp_layers()`](https://maxgav13.github.io/GMisc/reference/cp_layers.md),
[`layers_window()`](https://maxgav13.github.io/GMisc/reference/layers_window.md),
[`sc_layers()`](https://maxgav13.github.io/GMisc/reference/sc_layers.md)

## Examples

``` r
sc_bic(DPM_data, h = 0.1)
#> $GGPLOT

#> 
#> $PLOTLY
#> 
```
