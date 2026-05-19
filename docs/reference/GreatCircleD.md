# Compute a great-circle path for a stereonet (degrees)

Computes the path of the great circle of a plane on an equal-area or
equal-angle stereonet of unit radius.

## Usage

``` r
GreatCircleD(strike, dip, wulff = F)
```

## Arguments

- strike:

  Numeric. Strike of the plane in degrees (right-hand rule).

- dip:

  Numeric. Dip of the plane in degrees.

- wulff:

  Logical. If `TRUE` uses an equal-angle (Wulff) net; if `FALSE`
  (default) uses an equal-area (Schmidt) net.

## Value

A tibble with columns `xp` and `yp` giving the stereonet coordinates of
the great-circle path.

## References

Allmendinger, R. W., Cardozo, N., & Fisher, D. M. (2012). Structural
geology algorithms: Vectors and tensors. Cambridge university press.

## See also

Other structural algorithms:
[`Angles()`](https://maxgav13.github.io/GMisc/reference/Angles.md),
[`CartToSphD()`](https://maxgav13.github.io/GMisc/reference/CartToSphD.md),
[`Pole()`](https://maxgav13.github.io/GMisc/reference/Pole.md),
[`Rotate()`](https://maxgav13.github.io/GMisc/reference/Rotate.md),
[`SmallCircleD()`](https://maxgav13.github.io/GMisc/reference/SmallCircleD.md),
[`SphToCartD()`](https://maxgav13.github.io/GMisc/reference/SphToCartD.md),
[`StCoordLineD()`](https://maxgav13.github.io/GMisc/reference/StCoordLineD.md),
[`StPointD()`](https://maxgav13.github.io/GMisc/reference/StPointD.md)

## Examples

``` r
GreatCircleD(strike = 45, dip = 30)
#> Warning: The `x` argument of `as_tibble.matrix()` must have unique column names if
#> `.name_repair` is omitted as of tibble 2.0.0.
#> ℹ Using compatibility `.name_repair`.
#> ℹ The deprecated feature was likely used in the GMisc package.
#>   Please report the issue at <https://github.com/maxgav13/GMisc/issues>.
#> # A tibble: 181 × 2
#>       xp    yp
#>    <dbl> <dbl>
#>  1 0.707 0.707
#>  2 0.715 0.693
#>  3 0.722 0.679
#>  4 0.729 0.665
#>  5 0.735 0.651
#>  6 0.742 0.637
#>  7 0.748 0.623
#>  8 0.754 0.609
#>  9 0.759 0.595
#> 10 0.765 0.580
#> # ℹ 171 more rows
GreatCircleD(strike = 60, dip = 45, wulff = TRUE)
#> # A tibble: 181 × 2
#>       xp    yp
#>    <dbl> <dbl>
#>  1 0.866 0.5  
#>  2 0.861 0.483
#>  3 0.857 0.467
#>  4 0.852 0.451
#>  5 0.847 0.435
#>  6 0.842 0.419
#>  7 0.836 0.403
#>  8 0.831 0.388
#>  9 0.826 0.373
#> 10 0.820 0.358
#> # ℹ 171 more rows
```
