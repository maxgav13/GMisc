# Convert between a plane and its pole

Returns either the strike and dip of a plane given its pole, or the
trend and plunge of a pole given its plane (right-hand rule convention).

## Usage

``` r
Pole(trd, plg, k)
```

## Arguments

- trd:

  Numeric. Trend of the pole (radians) when `k = 0`, or strike of the
  plane (radians, right-hand rule) when `k = 1`.

- plg:

  Numeric. Plunge of the pole (radians) when `k = 0`, or dip of the
  plane (radians) when `k = 1`.

- k:

  Integer. Direction of conversion: `0` returns the plane from its pole;
  `1` returns the pole from its plane.

## Value

A named numeric vector. When `k = 0`: `c(strike, dip)` in radians. When
`k = 1`: `c(trd, plg)` in radians.

## References

Allmendinger, R. W., Cardozo, N., & Fisher, D. M. (2012). Structural
geology algorithms: Vectors and tensors. Cambridge university press.

## See also

Other structural algorithms:
[`Angles()`](https://maxgav13.github.io/GMisc/reference/Angles.md),
[`CartToSphD()`](https://maxgav13.github.io/GMisc/reference/CartToSphD.md),
[`GreatCircleD()`](https://maxgav13.github.io/GMisc/reference/GreatCircleD.md),
[`Rotate()`](https://maxgav13.github.io/GMisc/reference/Rotate.md),
[`SmallCircleD()`](https://maxgav13.github.io/GMisc/reference/SmallCircleD.md),
[`SphToCartD()`](https://maxgav13.github.io/GMisc/reference/SphToCartD.md),
[`StCoordLineD()`](https://maxgav13.github.io/GMisc/reference/StCoordLineD.md),
[`StPointD()`](https://maxgav13.github.io/GMisc/reference/StPointD.md)

## Examples

``` r
# Pole to plane (k = 1)
Pole(trd = pi / 4, plg = pi / 6, k = 1)
#>      trd      plg 
#> 5.497787 1.047198 

# Plane from its pole (k = 0)
Pole(trd = pi / 3, plg = pi / 8, k = 0)
#>   strike      dip 
#> 2.617994 1.178097 
```
