# Plots a layered model and summarizes the statistics for each layer from a Changepoint result

Given a number of breakpoints, plots a layered model of one variable
against distance, plots the confidence intervals for each layer, and
gives a summary table.

## Usage

``` r
cp_layers(x, breaks, conf.level = 0.95)
```

## Arguments

- x:

  The resulting object from
  [`cp_aic_eta()`](https://maxgav13.github.io/GMisc/reference/cp_aic_eta.md)

- breaks:

  An integer giving the number of breakpoints to use (from
  'Changepoint')

- conf.level:

  Confidence level to use for plot and summary statistics (Default is
  0.95)

## Value

A ggplot and plotly objects showing the layered model, another showing
the confidence intervals, and a summary table

## See also

Other layer detection:
[`RI()`](https://maxgav13.github.io/GMisc/reference/RI.md),
[`T2_stat()`](https://maxgav13.github.io/GMisc/reference/T2_stat.md),
[`T_stat()`](https://maxgav13.github.io/GMisc/reference/T_stat.md),
[`cp_aic_eta()`](https://maxgav13.github.io/GMisc/reference/cp_aic_eta.md),
[`layers_window()`](https://maxgav13.github.io/GMisc/reference/layers_window.md),
[`sc_bic()`](https://maxgav13.github.io/GMisc/reference/sc_bic.md),
[`sc_layers()`](https://maxgav13.github.io/GMisc/reference/sc_layers.md)

## Examples

``` r
cp = cp_aic_eta(DPM_data, m = 10, nl = 3)
cp_layers(cp, breaks = 2)
#> $LayersGG

#> 
#> $LayersLY
#> 
#> $StatsGG

#> 
#> $StatsLY
#> 
#> $Summary
#>      Layers Obs  Mean    SD Min Max CI.lwr CI.upr   MoE
#> 1   [0,2.7]  28  4.96 2.190   0  10   4.12   5.81 0.849
#> 2 (2.7,8.7]  60  8.08 1.150   6  11   7.79   8.38 0.297
#> 3  (8.7,10]  13 10.70 0.855  10  12  10.20  11.20 0.517
#> 
#> $ES
#> [1] 0.606
#> 
```
