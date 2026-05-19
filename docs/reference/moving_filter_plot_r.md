# Moving (weighted) average filter plot (residual)

Plots the residual results of a moving (weighted) average filter
routine.

## Usage

``` r
moving_filter_plot_r(
  x,
  xlab = "X",
  ylab = "Residual data",
  filterlab = "Filter",
  plotk = "all"
)
```

## Arguments

- x:

  The result of using the
  [`moving_avg_filter()`](https://maxgav13.github.io/GMisc/reference/moving_avg_filter.md)
  or
  [`moving_wt_filter()`](https://maxgav13.github.io/GMisc/reference/moving_wt_filter.md)
  functions

- xlab:

  The label for the x-axis

- ylab:

  The label for the y-axis

- filterlab:

  The label for the legend

- plotk:

  A string for choosing what to plot (Default is "all")

## Value

ggplot and plotly objects with the residual data

## Examples

``` r
data(nautilus)
k = c(3, 5, 7, 9)
filt = moving_avg_filter(nautilus$x, nautilus$y, k)
moving_filter_plot_r(filt)
#> $GGPLOT

#> 
#> $PLOTLY
#> 
moving_filter_plot_r(filt, plotk = c("k_3", "k_9")) # plots residuals for filters of window 3 and 9
#> $GGPLOT

#> 
#> $PLOTLY
#> 
```
