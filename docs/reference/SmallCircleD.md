# Compute a small-circle path for a stereonet (degrees)

Computes the path(s) of a small circle defined by its rotation axis and
cone angle on an equal-area or equal-angle stereonet of unit radius.

## Usage

``` r
SmallCircleD(trda, plga, coneAngle, wulff = F, closed = F)
```

## Arguments

- trda:

  Numeric. Trend of the rotation axis in degrees.

- plga:

  Numeric. Plunge of the rotation axis in degrees.

- coneAngle:

  Numeric. Half-apex (cone) angle in degrees.

- wulff:

  Logical. If `TRUE` uses an equal-angle (Wulff) net; if `FALSE`
  (default) uses an equal-area (Schmidt) net.

- closed:

  Logical. If `TRUE` the path segments are closed by appending the first
  point to the end. Default `FALSE`.

## Value

A named list with components:

- `path1`:

  Tibble of stereonet coordinates for the main segment.

- `path2`:

  Tibble of stereonet coordinates for the secondary segment.

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
[`SphToCartD()`](https://maxgav13.github.io/GMisc/reference/SphToCartD.md),
[`StCoordLineD()`](https://maxgav13.github.io/GMisc/reference/StCoordLineD.md),
[`StPointD()`](https://maxgav13.github.io/GMisc/reference/StPointD.md)

## Examples

``` r
SmallCircleD(trda = 45, plga = 30, coneAngle = 20)
#> $path1
#> # A tibble: 360 × 2
#>       xp    yp
#>    <dbl> <dbl>
#>  1 0.643 0.643
#>  2 0.647 0.639
#>  3 0.650 0.635
#>  4 0.654 0.631
#>  5 0.658 0.627
#>  6 0.662 0.623
#>  7 0.665 0.618
#>  8 0.669 0.614
#>  9 0.672 0.610
#> 10 0.675 0.606
#> # ℹ 350 more rows
#> 
#> $path2
#> # A tibble: 0 × 0
#> 
```
