# Spearman critical value

Calculates the critical value for Spearman's correlation coeficient.

## Usage

``` r
Spearman_crit(n, conf.level = 0.95)
```

## Arguments

- n:

  Sample size of the correlated values

- conf.level:

  Confidence level to use for the critical value (Default is 0.95, and
  see details section below)

## Value

Spearman's critical value for a given sample size

## Details

The result of this function can be used to estimate the statistical
signficance of Spearman's correlation coeficient. The options for
confidence level are 0.99, 0.95, or 0.90

## References

Swan, A. R. H. & Sandilands, M. (1995). Introduction to Geological Data
Analysis. Blackwell Science.

## See also

Other statistics:
[`NewStats_2samples()`](https://maxgav13.github.io/GMisc/reference/NewStats_2samples.md),
[`NewStats_contrasts()`](https://maxgav13.github.io/GMisc/reference/NewStats_contrasts.md),
[`R_crit()`](https://maxgav13.github.io/GMisc/reference/R_crit.md),
[`ci_chisq()`](https://maxgav13.github.io/GMisc/reference/conf_int_point.md),
[`pval_chisq()`](https://maxgav13.github.io/GMisc/reference/pvalue_point.md)

## Examples

``` r
Spearman_crit(10)
#> [1] 0.695
Spearman_crit(10, conf.level = 0.9)
#> [1] 0.611
```
