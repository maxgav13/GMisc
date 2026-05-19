# Rotate a line about an axis

Rotates a line (or axis) by applying a coordinate transformation matrix
computed from the rotation axis and rotation magnitude. Based on the
algorithm originally written by Randall A. Marrett.

## Usage

``` r
Rotate(raz, rdip, rot, trd, plg, ans0)
```

## Arguments

- raz:

  Numeric. Trend of the rotation axis in radians.

- rdip:

  Numeric. Plunge of the rotation axis in radians.

- rot:

  Numeric. Rotation magnitude in radians (positive = right-hand rule
  about the axis).

- trd:

  Numeric. Trend of the line to rotate in radians.

- plg:

  Numeric. Plunge of the line to rotate in radians.

- ans0:

  Character. `"a"` if the line to rotate is an axis (result forced into
  lower hemisphere); `"v"` if it is a vector.

## Value

A named numeric vector `c(trd, plg)` with the trend and plunge of the
rotated line in radians.

## References

Allmendinger, R. W., Cardozo, N., & Fisher, D. M. (2012). Structural
geology algorithms: Vectors and tensors. Cambridge university press.

## See also

Other structural algorithms:
[`Angles()`](https://maxgav13.github.io/GMisc/reference/Angles.md),
[`CartToSphD()`](https://maxgav13.github.io/GMisc/reference/CartToSphD.md),
[`GreatCircleD()`](https://maxgav13.github.io/GMisc/reference/GreatCircleD.md),
[`Pole()`](https://maxgav13.github.io/GMisc/reference/Pole.md),
[`SmallCircleD()`](https://maxgav13.github.io/GMisc/reference/SmallCircleD.md),
[`SphToCartD()`](https://maxgav13.github.io/GMisc/reference/SphToCartD.md),
[`StCoordLineD()`](https://maxgav13.github.io/GMisc/reference/StCoordLineD.md),
[`StPointD()`](https://maxgav13.github.io/GMisc/reference/StPointD.md)

## Examples

``` r
Rotate(raz = pi / 2, rdip = 0, rot = pi / 4,
       trd = pi / 4, plg = pi / 6, ans0 = "a")
#>        trd        plg 
#> 3.80310999 0.07954316 
```
