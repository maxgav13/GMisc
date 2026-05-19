# Exponential Model with Inverse Predictor (Type 3)

These functions provide Exponential Model with Inverse Predictor (Type
3) equation (`exp3.fun()`), as well as the self-starters for the
[`nls()`](https://rdrr.io/r/stats/nls.html) (`SSexp3()`) and
[`drc::drm()`](https://rdrr.io/pkg/drc/man/drm.html) functions
(`DRCexp3()`).

## Usage

``` r
exp3.fun(x, a, b)

SSexp3(x, a, b)

DRCexp3()
```

## Arguments

- x:

  A numeric vector of non-zero positive values (e.g., time, distance).

- a:

  A scaling parameter representing the value of the response when `1/x`
  approaches zero (i.e., as `x` tends to infinity). Must be positive.

- b:

  The rate parameter in the exponent. Controls the curvature of the
  response; negative values yield a decreasing curve, positive values an
  increasing curve.

## Value

`exp3.fun()` and `SSexp3()` return a numeric value, while `DRCexp3()`
returns a list containing the nonlinear function and the self starter
function.

## Details

The model is defined as:

\$\$y = a \\ e^{b / x}\$\$

where `a` is a scaling parameter representing the response as `x` tends
to infinity, and `b` is the rate parameter in the exponent. Negative
values of `b` yield a decreasing curve; positive values yield an
increasing curve.

## See also

Other non-linear functions, self-starters:
[`chaprich.fun()`](https://maxgav13.github.io/GMisc/reference/chaprich.md),
[`expneg.fun()`](https://maxgav13.github.io/GMisc/reference/expneg.md),
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
x <- seq(1, 20, length.out = 50)
y <- exp3.fun(x, a = 5, b = -3) + rnorm(50, sd = 0.1)
df <- data.frame(x = x, y = y)
mod = nls(y ~ SSexp3(x, a, b), data = df)
summary(mod)
#> 
#> Formula: y ~ SSexp3(x, a, b)
#> 
#> Parameters:
#>   Estimate Std. Error t value Pr(>|t|)    
#> a  5.03009    0.03456  145.54   <2e-16 ***
#> b -3.02623    0.06047  -50.05   <2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 0.09231 on 48 degrees of freedom
#> 
#> Number of iterations to convergence: 3 
#> Achieved convergence tolerance: 6.714e-06
#> 
plot(x, y, cex = 0.8)
lines(x, predict(mod), col = 'blue')


if (FALSE) { # \dontrun{
mod = drc::drm(y ~ x, data = df, fct = DRCexp3())
summary(mod)
plot(mod, log = "")
} # }
```
