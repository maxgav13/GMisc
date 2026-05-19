# Compute stereonet coordinates for a line (degrees)

Converts the trend and plunge of one or more lines to their \\(x, y)\\
positions on an equal-area or equal-angle stereonet of unit radius.
Vectorised over `trd` and `plg`.

## Usage

``` r
StCoordLineD(trd, plg, wulff = F)
```

## Arguments

- trd:

  Numeric vector. Trend of the line(s) in degrees.

- plg:

  Numeric vector. Plunge of the line(s) in degrees. Lines plunging into
  the upper hemisphere (negative plunge) are automatically reflected.

- wulff:

  Logical. If `TRUE` uses an equal-angle (Wulff) projection; if `FALSE`
  (default) uses an equal-area (Schmidt) projection.

## Value

A tibble with columns `xp` and `yp` giving the stereonet coordinates for
each input line.

## References

Allmendinger, R. W., Cardozo, N., & Fisher, D. M. (2012). Structural
geology algorithms: Vectors and tensors. Cambridge university press.

## See also

Other structural algorithms:
[`Angles()`](https://maxgav13.github.io/GMisc/reference/Angles.md),
[`CartToSphD()`](https://maxgav13.github.io/GMisc/reference/CartToSphD.md),
[`GreatCircleD()`](https://maxgav13.github.io/GMisc/reference/GreatCircleD.md),
[`Pole()`](https://maxgav13.github.io/GMisc/reference/Pole.md),
[`Rotate()`](https://maxgav13.github.io/GMisc/reference/Rotate.md),
[`SmallCircleD()`](https://maxgav13.github.io/GMisc/reference/SmallCircleD.md),
[`SphToCartD()`](https://maxgav13.github.io/GMisc/reference/SphToCartD.md),
[`StPointD()`](https://maxgav13.github.io/GMisc/reference/StPointD.md)

## Examples

``` r
StCoordLineD(trd = c(30, 90, 180), plg = c(10, 45, 60))
#> # A tibble: 3 × 2
#>         xp        yp
#>      <dbl>     <dbl>
#> 1 4.55e- 1  7.87e- 1
#> 2 5.41e- 1  3.31e-17
#> 3 4.48e-17 -3.66e- 1
StCoordLineD(trd = 45, plg = 30, wulff = TRUE)
#> # A tibble: 1 × 2
#>      xp    yp
#>   <dbl> <dbl>
#> 1 0.408 0.408
```
