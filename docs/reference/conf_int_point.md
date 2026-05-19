# Confidence interval from point estimate

Calculates the confidence interval for a population parameter (mean,
variance, standard deviation) based on a point estimate.

## Usage

``` r
ci_chisq(s, n, conf.level = 0.95, digits = 2)

ci_F(s1, n1, s2, n2, conf.level = 0.95, digits = 2)

ci_t(x, s, n, conf.level = 0.95, digits = 2)

ci_t2(x1, s1, n1, x2, s2, n2, var.equal = FALSE, conf.level = 0.95, digits = 2)

ci_z(x, sig, n, conf.level = 0.95, digits = 2)

ci_z2(x1, sig1, n1, x2, sig2, n2, conf.level = 0.95, digits = 2)
```

## Arguments

- s:

  Sample standard deviation

- n:

  Sample size

- conf.level:

  Confidence level to use for the confidence interval (Default is 0.95)

- digits:

  Number of digits to round to (Default is 2)

- s1:

  Sample's 1 standard deviation

- n1:

  Sample size of sample 1

- s2:

  Sample's 2 standard deviation

- n2:

  Sample size of sample 2

- x:

  Sample mean

- x1:

  Samples's 1 mean

- x2:

  Samples's 2 mean

- var.equal:

  Logical indicating if the variances are equal or not (for `ci_t2`)

- sig:

  Population standard deviation

- sig1:

  Population's 1 standard deviation

- sig2:

  Population's 2 standard deviation

## Value

A tibble with the estimate, degrees of freedom, and lower and upper ends
of the confidence interval

## Details

`ci_chisq` for variance, `ci_F` for ratio of variances, `ci_t` for mean
with unknown sigma (can work for dependent (paired) samples using the
mean and standard deviation of differences) and `ci_t2` for the
difference of two means with unknown and potentially unequal variances,
`ci_z` for mean with known sigma, and `ci_z2` for the difference of two
means with known sigma.

## See also

Other statistics:
[`NewStats_2samples()`](https://maxgav13.github.io/GMisc/reference/NewStats_2samples.md),
[`NewStats_contrasts()`](https://maxgav13.github.io/GMisc/reference/NewStats_contrasts.md),
[`R_crit()`](https://maxgav13.github.io/GMisc/reference/R_crit.md),
[`Spearman_crit()`](https://maxgav13.github.io/GMisc/reference/Spearman_crit.md),
[`pval_chisq()`](https://maxgav13.github.io/GMisc/reference/pvalue_point.md)

## Examples

``` r
s <- 0.535
n <- 10
ci_chisq(s, n)
#> # A tibble: 2 × 5
#>   stat  value    df lower upper
#>   <chr> <dbl> <dbl> <dbl> <dbl>
#> 1 sd     0.54     9  0.37  0.98
#> 2 var    0.29     9  0.14  0.95

s1 <- 3.1
n1 <- 15
s2 <- 0.8
n2 <- 12
ci_F(s1, n1, s2, n2)
#> # A tibble: 2 × 6
#>   stat  ratio   df1   df2 lower upper
#>   <chr> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 sd     3.88    14    11  2.11  6.82
#> 2 var   15.0     14    11  4.47 46.5 

x <- 80
s <- 15
n <- 20
ci_t(x, s, n)
#> # A tibble: 1 × 4
#>    mean    df lower upper
#>   <dbl> <dbl> <dbl> <dbl>
#> 1    80    19  73.0  87.0

x1 <- 90
s1 <- 15
n1 <- 20
x2 <- 80
s2 <- 8
n2 <- 30
ci_t2(x1, s1, n1, x2, s2, n2)
#> # A tibble: 1 × 4
#>   mean_diff    df lower upper
#>       <dbl> <dbl> <dbl> <dbl>
#> 1        10  26.3  2.48  17.5

x <- 80
sig <- 15
n <- 20
ci_z(x, sig, n)
#> # A tibble: 1 × 3
#>    mean lower upper
#>   <dbl> <dbl> <dbl>
#> 1    80  73.4  86.6

x1 <- 42
sig1 <- 8
n1 <- 75
x2 <- 36
sig2 <- 6
n2 <- 50
ci_z2(x1, sig1, n1, x2, sig2, n2)
#> # A tibble: 1 × 3
#>   mean_diff lower upper
#>       <dbl> <dbl> <dbl>
#> 1         6  3.54  8.46
```
