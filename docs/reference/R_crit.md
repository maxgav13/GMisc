# Mean resultant length critical value

Calculates the critical value for the mean resultant length for
directional statistics.

## Usage

``` r
R_crit(n, case = "2D", conf.level = 0.95)
```

## Arguments

- n:

  Sample size of the correlated values

- case:

  String idicating for which case to calculate the critical value, 2D or
  3D

- conf.level:

  Confidence level to use for the critical value (Default is 0.95, and
  see details section below)

## Value

Mean resultant length's critical value for a given sample size

## Details

The result of this function is mostly to be used with
`dir_unif_test_xD()`, but can be used as standalone. The options for
confidence level are 0.99, 0.975, 0.95, or 0.90

## References

Davis, J. C. (2002). Statistical and Data Analysis in Geology. 3rd ed.
John Wiley & Sons.

## See also

Other statistics:
[`NewStats_2samples()`](https://maxgav13.github.io/GMisc/reference/NewStats_2samples.md),
[`NewStats_contrasts()`](https://maxgav13.github.io/GMisc/reference/NewStats_contrasts.md),
[`Spearman_crit()`](https://maxgav13.github.io/GMisc/reference/Spearman_crit.md),
[`ci_chisq()`](https://maxgav13.github.io/GMisc/reference/conf_int_point.md),
[`pval_chisq()`](https://maxgav13.github.io/GMisc/reference/pvalue_point.md)

## Examples

``` r
R_crit(10)
#> [1] 0.539
R_crit(10, conf.level = 0.9)
#> [1] 0.479
R_crit(10, case = "3D", conf.level = 0.9)
#> [1] 0.454
```
