# First-Order Inverse Polynomial Model

These functions provide First-Order Inverse Polynomial Model equation
(`invpoly1.fun()`), as well as the self-starters for the
[`nls()`](https://rdrr.io/r/stats/nls.html) (`SSinvpoly1()`) and
[`drc::drm()`](https://rdrr.io/pkg/drc/man/drm.html) functions
(`DRCinvpoly1()`).

## Usage

``` r
invpoly1.fun(x, a, b)

SSinvpoly1(x, a, b)

DRCinvpoly1()
```

## Arguments

- x:

  A numeric vector of non-zero positive values (e.g., time, distance).

- a:

  The coefficient of the \\1/x\\ term in the linearised form. Controls
  the initial rise of the curve near the origin.

- b:

  The coefficient of the \\x\\ term in the denominator. The reciprocal
  \\1/b\\ gives the asymptote of the response as \\x \to \infty\\.

## Value

`invpoly1.fun()` and `SSinvpoly1()` return a numeric value, while
`DRCinvpoly1()` returns a list containing the nonlinear function and the
self starter function.

## Details

The model is defined as:

\$\$y = \frac{x}{a + b x}\$\$

where `a` is the coefficient of the \\1/x\\ term in the linearised form
(controls the initial rise near the origin) and `b` is the coefficient
of the linear term in the denominator. The reciprocal \\1/b\\ gives the
asymptote as \\x \to \infty\\.

The curve passes through the origin and approaches the asymptote \\1/b\\
as \\x \to \infty\\.

## See also

Other non-linear functions, self-starters:
[`chaprich.fun()`](https://maxgav13.github.io/GMisc/reference/chaprich.md),
[`exp3.fun()`](https://maxgav13.github.io/GMisc/reference/exp3.md),
[`expneg.fun()`](https://maxgav13.github.io/GMisc/reference/expneg.md),
[`expneg2.fun()`](https://maxgav13.github.io/GMisc/reference/expneg2.md),
[`invpoly2.fun()`](https://maxgav13.github.io/GMisc/reference/invpoly2.md),
[`invpoly3.fun()`](https://maxgav13.github.io/GMisc/reference/invpoly3.md),
[`kostmod.fun()`](https://maxgav13.github.io/GMisc/reference/kostmod.md),
[`varexp.fun()`](https://maxgav13.github.io/GMisc/reference/varexp.md),
[`vargauss.fun()`](https://maxgav13.github.io/GMisc/reference/vargauss.md),
[`varsph.fun()`](https://maxgav13.github.io/GMisc/reference/varsph.md)

## Examples

``` r
x <- seq(1, 20, length.out = 50)
y <- invpoly1.fun(x, a = 2, b = 0.5) + rnorm(50, sd = 0.05)
df <- data.frame(x = x, y = y)
mod = nls(y ~ SSinvpoly1(x, a, b), data = df)
summary(mod)
#> 
#> Formula: y ~ SSinvpoly1(x, a, b)
#> 
#> Parameters:
#>   Estimate Std. Error t value Pr(>|t|)    
#> a 2.018758   0.065033   31.04   <2e-16 ***
#> b 0.500286   0.006379   78.43   <2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 0.04561 on 48 degrees of freedom
#> 
#> Number of iterations to convergence: 3 
#> Achieved convergence tolerance: 5.63e-08
#> 
plot(x, y, cex = 0.8)
lines(x, predict(mod), col = 'blue')


if (FALSE) { # \dontrun{
mod = drc::drm(y ~ x, data = df, fct = DRCinvpoly1())
summary(mod)
plot(mod, log = "")
} # }
```
