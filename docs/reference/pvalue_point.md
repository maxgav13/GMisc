# P-value for proposed population parameter

Calculates the p-value for a proposed parameter.

## Usage

``` r
pval_chisq(s, n, sigma, side = c("two", "one"))

pval_F(s1, n1, s2, n2, side = c("two", "one"))

pval_t(x, s, n, mu, side = c("two", "one"))

pval_t2(x1, s1, n1, x2, s2, n2, dmu = 0, side = c("two", "one"))

pval_z(x, sig, n, mu = 0, side = c("two", "one"))

pval_corr(r, n, side = c("two", "one"))
```

## Arguments

- s:

  Sample standard deviation

- n:

  Sample size

- sigma:

  Population standard deviation

- side:

  To either get a two-tail or one-tail p-value (Default is "two")

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

- mu:

  Population mean

- x1:

  Sample's 1 mean

- x2:

  Sample's 2 mean

- dmu:

  Proposed difference between sample means (Default is 0)

- sig:

  Population standard deviation

- r:

  The coefficient of correlation to test

## Value

P-value

## See also

Other statistics:
[`NewStats_2samples()`](https://maxgav13.github.io/GMisc/reference/NewStats_2samples.md),
[`NewStats_contrasts()`](https://maxgav13.github.io/GMisc/reference/NewStats_contrasts.md),
[`R_crit()`](https://maxgav13.github.io/GMisc/reference/R_crit.md),
[`Spearman_crit()`](https://maxgav13.github.io/GMisc/reference/Spearman_crit.md),
[`ci_chisq()`](https://maxgav13.github.io/GMisc/reference/conf_int_point.md)

## Examples

``` r
s <- 0.535
n <- 10
sig <- 1
pval_chisq(s, n, sig)
#> [1] 0.0425

s1 <- 3.1
n1 <- 15
s2 <- 0.8
n2 <- 12
pval_F(s1, n1, s2, n2)
#> [1] 6.92e-05

x <- 6.14
s <- 0.803
n <- 9
mu <- 7
pval_t(x, s, n, mu)
#> [1] 0.0124

x1 <- 34.67
s1 <- 4.97
n1 <- 15
x2 <- 40.87
s2 <- 5.36
n2 <- 15
pval_t2(x1, s1, n1, x2, s2, n2)
#> [1] 0.00274

x <- 2.6
sig <- 0.3
n <- 36
mu <- 2.5
pval_z(x, sig, n, mu)
#> [1] 0.0455

r <- 0.597
n <- 10
pval_corr(r, n)
#> [1] 0.0684
```
