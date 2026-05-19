# Moving average filter

Filters data using moving averages of diferent window sizes.

## Usage

``` r
moving_avg_filter(x, y, k)
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
k = c(3, 5, 11, 25)
moving_avg_filter(nautilus$x, nautilus$y, k)
#> $Filtered
#> # A tibble: 129 × 6
#>        x     y    k_3    k_5   k_11   k_25
#>    <dbl> <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#>  1     1  4.89  0.390 -1.11  -0.46  -1.38 
#>  2     2 -4.11 -1.11  -0.620 -0.701 -1.80 
#>  3     3 -4.12 -2.46  -0.124 -0.634 -1.49 
#>  4     4  0.86 -0.467 -1.53  -1.03  -1.23 
#>  5     5  1.86  0.193 -1.14  -1.24  -0.992
#>  6     6 -2.14 -0.81  -0.346 -0.963 -0.949
#>  7     7 -2.15 -1.48  -1.35  -1.88  -0.912
#>  8     8 -0.16 -2.16  -2.36  -1.70  -0.927
#>  9     9 -4.16 -2.50  -1.57  -1.98  -1.04 
#> 10    10 -3.17 -1.84  -2.17  -1.81  -1.14 
#> # ℹ 119 more rows
#> 
#> $Residual
#> # A tibble: 129 × 5
#>        x    k_3    k_5   k_11   k_25
#>    <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#>  1     1  4.5    6.00   5.35   6.27 
#>  2     2 -3.00  -3.49  -3.41  -2.31 
#>  3     3 -1.66  -4.00  -3.49  -2.63 
#>  4     4  1.33   2.39   1.89   2.09 
#>  5     5  1.67   3.00   3.1    2.85 
#>  6     6 -1.33  -1.79  -1.18  -1.19 
#>  7     7 -0.667 -0.8   -0.271 -1.24 
#>  8     8  2.00   2.20   1.54   0.767
#>  9     9 -1.66  -2.59  -2.18  -3.12 
#> 10    10 -1.33  -0.996 -1.36  -2.03 
#> # ℹ 119 more rows
#> 
```
