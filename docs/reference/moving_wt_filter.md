# Moving weighted average filter

Filters data using moving weighted averages of diferent window sizes.

## Usage

``` r
moving_wt_filter(x, y, k)
```

## Arguments

- x:

  A numeric vector with the position values of the data to filter (a
  sequence)

- y:

  A numeric vector with the data to filter

- k:

  A numeric vector with the widths of the windows to use for the filter
  (must be an odd number)

## Value

A list with two tibbles: one with the filtered results plus the original
data, and one with the residuals

## Details

The weigths for the window lengths are calculated in a way that farther
observations from the central (estimated) value are given a lesser
weight than observations near de central value

## References

Swan, A. R. H. & Sandilands, M. (1995). Introduction to Geological Data
Analysis. Blackwell Science.

## Examples

``` r
data(nautilus)
head(nautilus)
#>   x     y
#> 1 1  4.89
#> 2 2 -4.11
#> 3 3 -4.12
#> 4 4  0.86
#> 5 5  1.86
#> 6 6 -2.14
k = c(5, 7, 9)
moving_wt_filter(nautilus$x, nautilus$y, k)
#> $Filtered
#> # A tibble: 129 × 5
#>        x     y    k_5    k_7    k_9
#>    <dbl> <dbl>  <dbl>  <dbl>  <dbl>
#>  1     1  4.89  0.177 -0.332 -0.383
#>  2     2 -4.11 -1.12  -0.739 -0.565
#>  3     3 -4.12 -1.44  -0.881 -0.732
#>  4     4  0.86 -0.893 -0.965 -0.871
#>  5     5  1.86 -0.340 -0.846 -1.01 
#>  6     6 -2.14 -0.673 -0.841 -1.17 
#>  7     7 -2.15 -1.46  -1.38  -1.34 
#>  8     8 -0.16 -2.12  -1.87  -1.65 
#>  9     9 -4.16 -2.16  -2.04  -1.97 
#> 10    10 -3.17 -2.09  -2.13  -2.27 
#> # ℹ 119 more rows
#> 
#> $Residual
#> # A tibble: 129 × 4
#>        x    k_5    k_7    k_9
#>    <dbl>  <dbl>  <dbl>  <dbl>
#>  1     1  4.71   5.22   5.27 
#>  2     2 -2.99  -3.37  -3.55 
#>  3     3 -2.68  -3.24  -3.39 
#>  4     4  1.75   1.83   1.73 
#>  5     5  2.20   2.71   2.87 
#>  6     6 -1.47  -1.30  -0.968
#>  7     7 -0.687 -0.765 -0.811
#>  8     8  1.96   1.71   1.49 
#>  9     9 -2.00  -2.12  -2.19 
#> 10    10 -1.08  -1.04  -0.901
#> # ℹ 119 more rows
#> 
```
