# Negative Exponential Model

These functions provide Negative Exponential Model equation
(`expneg.fun()`), as well as the self-starters for the
[`nls()`](https://rdrr.io/r/stats/nls.html) (`SSexpneg()`) and
[`drc::drm()`](https://rdrr.io/pkg/drc/man/drm.html) functions
(`DRCexpneg()`).

## Usage

``` r
expneg.fun(x, a, b, c)

SSexpneg(x, a, b, c)

DRCexpneg()
```

## Arguments

- x:

  A numeric vector of values (e.g., time, dose).

- a:

  The horizontal asymptote of the response as `x` tends to infinity.

- b:

  The initial offset from the asymptote at `x = 0`, i.e. \\y(0) = a +
  b\\.

- c:

  The rate parameter in the exponent. Negative values yield decay toward
  `a`; positive values yield growth away from `a`.

## Value

`expneg.fun()` and `SSexpneg()` return a numeric value, while
`DRCexpneg()` returns a list containing the nonlinear function and the
self starter function.

## Details

The model is defined as:

\$\$y = a + b \\ e^{cx}\$\$

where `a` is the horizontal asymptote as `x` tends to infinity, `b` is
the initial offset from the asymptote at `x = 0` (i.e. \\y(0) = a +
b\\), and `c` is the rate parameter. Negative values of `c` yield decay
toward `a`; positive values yield growth away from `a`.

## See also

Other non-linear functions, self-starters:
[`chaprich.fun()`](https://maxgav13.github.io/GMisc/reference/chaprich.md),
[`exp3.fun()`](https://maxgav13.github.io/GMisc/reference/exp3.md),
[`expneg2.fun()`](https://maxgav13.github.io/GMisc/reference/expneg2.md),
[`invpoly1.fun()`](https://maxgav13.github.io/GMisc/reference/invpoly1.md),
[`invpoly2.fun()`](https://maxgav13.github.io/GMisc/reference/invpoly2.md),
[`invpoly3.fun()`](https://maxgav13.github.io/GMisc/reference/invpoly3.md),
[`kostmod.fun()`](https://maxgav13.github.io/GMisc/reference/kostmod.md),
[`varexp.fun()`](https://maxgav13.github.io/GMisc/reference/varexp.md),
[`vargauss.fun()`](https://maxgav13.github.io/GMisc/reference/vargauss.md),
[`varsph.fun()`](https://maxgav13.github.io/GMisc/reference/varsph.md)

## Examples

``` r
x <- seq(0, 10, length.out = 50)
y <- expneg.fun(x, a = 5, b = -4, c = -0.4) + rnorm(50, sd = 0.1)
df <- data.frame(x = x, y = y)
mod = nls(y ~ SSexpneg(x, a, b, c), data = df)
summary(mod)
#> 
#> Formula: y ~ SSexpneg(x, a, b, c)
#> 
#> Parameters:
#>   Estimate Std. Error t value Pr(>|t|)    
#> a  4.97445    0.03475  143.14   <2e-16 ***
#> b -4.04913    0.05613  -72.14   <2e-16 ***
#> c -0.40735    0.01427  -28.54   <2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 0.1026 on 47 degrees of freedom
#> 
#> Number of iterations to convergence: 3 
#> Achieved convergence tolerance: 4.572e-07
#> 
plot(x, y, cex = 0.8)
lines(x, predict(mod), col = 'blue')


if (FALSE) { # \dontrun{
mod = drc::drm(y ~ x, data = df, fct = DRCexpneg())
summary(mod)
plot(mod, log = "")
} # }
```
