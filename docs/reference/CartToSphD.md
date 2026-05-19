# Convert Cartesian to spherical coordinates (degrees)

Converts North–East–Down direction cosines to the trend and plunge of
the corresponding line in degrees.

## Usage

``` r
CartToSphD(cn, ce, cd)
```

## Arguments

- cn:

  Numeric. North direction cosine.

- ce:

  Numeric. East direction cosine.

- cd:

  Numeric. Down direction cosine.

## Value

A named numeric vector `c(trd, plg)` with trend and plunge in degrees.

## References

Allmendinger, R. W., Cardozo, N., & Fisher, D. M. (2012). Structural
geology algorithms: Vectors and tensors. Cambridge university press.

## See also

Other structural algorithms:
[`Angles()`](https://maxgav13.github.io/GMisc/reference/Angles.md),
[`GreatCircleD()`](https://maxgav13.github.io/GMisc/reference/GreatCircleD.md),
[`Pole()`](https://maxgav13.github.io/GMisc/reference/Pole.md),
[`Rotate()`](https://maxgav13.github.io/GMisc/reference/Rotate.md),
[`SmallCircleD()`](https://maxgav13.github.io/GMisc/reference/SmallCircleD.md),
[`SphToCartD()`](https://maxgav13.github.io/GMisc/reference/SphToCartD.md),
[`StCoordLineD()`](https://maxgav13.github.io/GMisc/reference/StCoordLineD.md),
[`StPointD()`](https://maxgav13.github.io/GMisc/reference/StPointD.md)

## Examples

``` r
CartToSphD(cn = 0.707, ce = 0.5, cd = 0.5)
#>      trd      plg 
#> 35.26847 30.00000 
```
