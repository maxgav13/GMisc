# Compute angles between lines or planes

Calculates the angle between two lines, the angle between two planes,
the line of intersection of two planes, or the plane containing two
apparent dips, depending on the value of `ans0`.

## Usage

``` r
Angles(trd1, plg1, trd2, plg2, ans0)
```

## Arguments

- trd1:

  Numeric. Trend of the first line, or strike of the first plane
  (radians, right-hand rule for planes).

- plg1:

  Numeric. Plunge of the first line, or dip of the first plane
  (radians).

- trd2:

  Numeric. Trend of the second line, or strike of the second plane
  (radians).

- plg2:

  Numeric. Plunge of the second line, or dip of the second plane
  (radians).

- ans0:

  Character. Operation selector:

  `"l"`

  :   Angle between two lines (trend/plunge input).

  `"p"`

  :   Angle between two planes (strike/dip input, right-hand rule).

  `"i"`

  :   Trend and plunge of the intersection line of two planes.

  `"a"`

  :   Strike and dip of the plane containing two apparent dips.

## Value

A named list with components `ans1` and `ans2`. For `"l"` and `"p"`,
these are the angle and its supplement (degrees). For `"i"` and `"a"`,
these are the trend/strike and plunge/dip of the resulting line or plane
(degrees).

## References

Allmendinger, R. W., Cardozo, N., & Fisher, D. M. (2012). Structural
geology algorithms: Vectors and tensors. Cambridge university press.

## See also

Other structural algorithms:
[`CartToSphD()`](https://maxgav13.github.io/GMisc/reference/CartToSphD.md),
[`GreatCircleD()`](https://maxgav13.github.io/GMisc/reference/GreatCircleD.md),
[`Pole()`](https://maxgav13.github.io/GMisc/reference/Pole.md),
[`Rotate()`](https://maxgav13.github.io/GMisc/reference/Rotate.md),
[`SmallCircleD()`](https://maxgav13.github.io/GMisc/reference/SmallCircleD.md),
[`SphToCartD()`](https://maxgav13.github.io/GMisc/reference/SphToCartD.md),
[`StCoordLineD()`](https://maxgav13.github.io/GMisc/reference/StCoordLineD.md),
[`StPointD()`](https://maxgav13.github.io/GMisc/reference/StPointD.md)

## Examples

``` r
# Angle between two lines
Angles(trd1 = 3 * pi / 180, plg1 = 20 * pi / 180,
       trd2 = 80 * pi / 180, plg2 = 40 * pi / 180,
       ans0 = "l")
#> $ans1
#> [1] 1.388441
#> 
#> $ans2
#> [1] 178.6116
#> 

# Intersection of two planes
Angles(trd1 = 40 * pi / 180, plg1 = 30 * pi / 180,
       trd2 = 120 * pi / 180, plg2 = 50 * pi / 180,
       ans0 = "i")
#> $ans1
#>      trd 
#> 4.186462 
#> 
#> $ans2
#>        plg 
#> 0.03185937 
#> 
```
