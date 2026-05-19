# Modified Kostiakov Infiltration Model

These functions provide the Modified Kostiakov Infiltration Model
equation (`kostmod.fun()`), as well as the self-starters for the
[`nls()`](https://rdrr.io/r/stats/nls.html) (`SSkostmod()`) and
[`drc::drm()`](https://rdrr.io/pkg/drc/man/drm.html) functions
(`DRCkostmod()`).

## Usage

``` r
kostmod.fun(x, alfa, beta, fc)

SSkostmod(x, alfa, beta, fc)

DRCkostmod()
```

## Arguments

- x:

  A numeric vector (e.g., time, distance).

- alfa:

  The parameter controlling the shape of the curve. Must be non-zero.

- beta:

  The exponent applied to `x`. Affects the curvature of the model.

- fc:

  The value of the curve at the final observation.

## Value

`kostmod.fun()` and `SSkostmod()` return a numeric value, while
`DRCkostmod()` returns a list containing the nonlinear function and the
self starter function.

## Details

The model is defined as:

\$\$y = \alpha x^{\beta} + f_c\$\$

where `alfa` (\\\alpha\\) controls the shape of the curve, `beta`
(\\\beta\\) is the exponent applied to `x` and affects curvature, and
`fc` (\\f_c\\) is the value of the curve at the final observation (an
additive constant).

## See also

Other non-linear functions, self-starters:
[`chaprich.fun()`](https://maxgav13.github.io/GMisc/reference/chaprich.md),
[`exp3.fun()`](https://maxgav13.github.io/GMisc/reference/exp3.md),
[`expneg.fun()`](https://maxgav13.github.io/GMisc/reference/expneg.md),
[`expneg2.fun()`](https://maxgav13.github.io/GMisc/reference/expneg2.md),
[`invpoly1.fun()`](https://maxgav13.github.io/GMisc/reference/invpoly1.md),
[`invpoly2.fun()`](https://maxgav13.github.io/GMisc/reference/invpoly2.md),
[`invpoly3.fun()`](https://maxgav13.github.io/GMisc/reference/invpoly3.md),
[`varexp.fun()`](https://maxgav13.github.io/GMisc/reference/varexp.md),
[`vargauss.fun()`](https://maxgav13.github.io/GMisc/reference/vargauss.md),
[`varsph.fun()`](https://maxgav13.github.io/GMisc/reference/varsph.md)

## Examples

``` r
x <- seq(0.5, 40)
y <- kostmod.fun(x, alfa = 0.3, beta = -0.8, fc = 0.1) + rnorm(40, sd = 0.01)
df <- data.frame(x = x, y = y)
mod = nls(y ~ SSkostmod(x, alfa, beta, fc), data = df)
summary(mod)
#> 
#> Formula: y ~ SSkostmod(x, alfa, beta, fc)
#> 
#> Parameters:
#>       Estimate Std. Error t value Pr(>|t|)    
#> alfa  0.307981   0.008137   37.85   <2e-16 ***
#> beta -0.791292   0.030040  -26.34   <2e-16 ***
#> fc    0.096380   0.003942   24.45   <2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 0.009563 on 37 degrees of freedom
#> 
#> Number of iterations to convergence: 4 
#> Achieved convergence tolerance: 4.869e-07
#> 
plot(x, y, cex = 0.8)
lines(x, predict(mod), col = 'blue')


if (FALSE) { # \dontrun{
mod = drc::drm(y ~ x, data = df, fct = DRCkostmod())
summary(mod)
plot(mod, log = "")
} # }
```
