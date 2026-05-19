# Similarity Coefficient

Calculates the similarity coeficient between observations based on their
features (columns). Coefficient ranges between 0 and 1, with values
close to 1 indicating more similarity.

## Usage

``` r
similarity_coef(x, samp.names = NULL, digits = 3)
```

## Arguments

- x:

  A data frame of observations in rows and features (variables) in
  columns. Features must be non-zero and positive.

- samp.names:

  A character vector with the names of the observations (samples)

- digits:

  Number of digits to use

## Value

A matrix of similarity coefficients

## References

Borchardt, G.A. (1974). The SIMAN Coefficient for Similarity Analysis.
Classification Society Bulletin, 3(2), 2–8.

## Examples

``` r
dat = iris[c(1,2,51,52,101,102),]
similarity_coef(dat)
#>         1     2    51    52   101   102
#> 1   1.000 0.954 0.521 0.539 0.516 0.508
#> 2   0.954 1.000 0.520 0.537 0.500 0.531
#> 51  0.521 0.520 1.000 0.951 0.803 0.833
#> 52  0.539 0.537 0.951 1.000 0.826 0.855
#> 101 0.516 0.500 0.803 0.826 1.000 0.837
#> 102 0.508 0.531 0.833 0.855 0.837 1.000
```
