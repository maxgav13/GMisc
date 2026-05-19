# Gaussian Variogram Model

These functions provide Gaussian Variogram Model equation
(`vargauss.fun()`), as well as the self-starters for the
[`nls()`](https://rdrr.io/r/stats/nls.html) (`SSvargauss()`) and
[`drc::drm()`](https://rdrr.io/pkg/drc/man/drm.html) functions
(`DRCvargauss()`).

## Usage

``` r
vargauss.fun(x, nug, psill, range)

SSvargauss(x, nug, psill, range)

DRCvargauss()
```

## Arguments

- x:

  A numeric vector of non-negative lag distances.

- nug:

  The nugget: the semivariance at zero lag distance (intercept).

- psill:

  The partial sill: the difference between the sill (total variance) and
  the nugget.

- range:

  The range parameter controlling the rate of increase. The practical
  range at which ~95 percent of the sill is reached is approximately
  \\\sqrt{3} \times \text{range}\\.

## Value

`vargauss.fun()` and `SSvargauss()` return a numeric value, while
`DRCvargauss()` returns a list containing the nonlinear function and the
self starter function.

## Details

The model is defined as:

\$\$y = \text{nug} + \text{psill} \left(1 - e^{-(x /
\text{range})^2}\right)\$\$

where `nug` is the nugget (semivariance at zero lag), `psill` is the
partial sill, and `range` controls the rate of increase. The practical
range at which ~95 percent of the sill is reached is approximately
\\\sqrt{3} \times \text{range}\\. The Gaussian model has a smooth,
parabolic behaviour near the origin compared to the concave-up
exponential.

The Gaussian variogram model is commonly used in geostatistics to
describe spatial autocorrelation with a smooth, parabolic behaviour near
the origin. Like the exponential model, it approaches the sill
asymptotically, but has an S-shaped rise compared to the concave-up
exponential curve.

## See also

Other non-linear functions, self-starters:
[`chaprich.fun()`](https://maxgav13.github.io/GMisc/reference/chaprich.md),
[`exp3.fun()`](https://maxgav13.github.io/GMisc/reference/exp3.md),
[`expneg.fun()`](https://maxgav13.github.io/GMisc/reference/expneg.md),
[`expneg2.fun()`](https://maxgav13.github.io/GMisc/reference/expneg2.md),
[`invpoly1.fun()`](https://maxgav13.github.io/GMisc/reference/invpoly1.md),
[`invpoly2.fun()`](https://maxgav13.github.io/GMisc/reference/invpoly2.md),
[`invpoly3.fun()`](https://maxgav13.github.io/GMisc/reference/invpoly3.md),
[`kostmod.fun()`](https://maxgav13.github.io/GMisc/reference/kostmod.md),
[`varexp.fun()`](https://maxgav13.github.io/GMisc/reference/varexp.md),
[`varsph.fun()`](https://maxgav13.github.io/GMisc/reference/varsph.md)

## Examples

``` r
x <- seq(0, 100, length.out = 50)
y <- vargauss.fun(x, nug = 0.2, psill = 0.8, range = 20) + rnorm(50, sd = 0.02)
df <- data.frame(x = x, y = y)
mod = nls(y ~ SSvargauss(x, nug, psill, range), data = df)
summary(mod)
#> 
#> Formula: y ~ SSvargauss(x, nug, psill, range)
#> 
#> Parameters:
#>        Estimate Std. Error t value Pr(>|t|)    
#> nug    0.197555   0.007649   25.83   <2e-16 ***
#> psill  0.802862   0.007915  101.43   <2e-16 ***
#> range 20.187738   0.251812   80.17   <2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 0.01638 on 47 degrees of freedom
#> 
#> Number of iterations to convergence: 5 
#> Achieved convergence tolerance: 2.056e-06
#> 
plot(x, y, cex = 0.8)
lines(x, predict(mod), col = 'blue')


if (FALSE) { # \dontrun{
mod = drc::drm(y ~ x, data = df, fct = DRCvargauss())
summary(mod)
plot(mod, log = "")
} # }
```
