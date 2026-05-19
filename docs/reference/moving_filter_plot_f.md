# Moving (weighted) average filter plot (filtered)

Plots the filtered results of a moving (weighted) average filter
routine.

## Usage

``` r
moving_filter_plot_f(
  x,
  xlab = "X",
  ylab = "Data",
  filterlab = "Filter & data",
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

ggplot and plotly objects with the filtered data and original data

## Examples

``` r
data(nautilus)
k = c(3, 5, 7, 9)
filt = moving_avg_filter(nautilus$x, nautilus$y, k)
moving_filter_plot_f(filt)
#> $GGPLOT

#> 
#> $PLOTLY
#> 
moving_filter_plot_f(filt, plotk = c("y", "k_9")) # plots data and filter of window 9
#> $GGPLOT

#> 
#> $PLOTLY
#> 
moving_filter_plot_f(filt, plotk = c("k_3", "k_9")) # plots filters of window 3 and 9
#> $GGPLOT

#> 
#> $PLOTLY
#> 
```
