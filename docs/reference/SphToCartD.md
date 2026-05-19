# Convert spherical to Cartesian coordinates (degrees)

Converts a line or plane orientation in spherical coordinates to
direction cosines in a North–East–Down system.

## Usage

``` r
SphToCartD(trd, plg, k = 0)
```

## Arguments

- trd:

  Numeric. Trend of the line (degrees, `k = 0`) or strike of the plane
  (degrees, right-hand rule, `k = 1`).

- plg:

  Numeric. Plunge of the line (degrees, `k = 0`) or dip of the plane
  (degrees, `k = 1`).

- k:

  Integer. `0` for a line (trend/plunge); `1` for a plane (strike/dip,
  right-hand rule). Default `0`.

## Value

A named list with components `cn`, `ce`, and `cd` (north, east, and down
direction cosines).

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
[`StCoordLineD()`](https://maxgav13.github.io/GMisc/reference/StCoordLineD.md),
[`StPointD()`](https://maxgav13.github.io/GMisc/reference/StPointD.md)

## Examples

``` r
SphToCartD(trd = 45, plg = 30)
#> $cn
#> [1] 0.6123724
#> 
#> $ce
#> [1] 0.6123724
#> 
#> $cd
#> [1] 0.5
#> 
SphToCartD(trd = 60, plg = 36, k = 1)
#> $cn
#> [1] 0.509037
#> 
#> $ce
#> [1] -0.2938926
#> 
#> $cd
#> [1] 0.809017
#> 
```
