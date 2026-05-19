# Compute equal-area stereonet coordinates from azimuth and inclination (degrees)

Converts azimuth and inclination angle pairs to \\(x, y)\\ positions on
an equal-area (Schmidt) stereonet of unit radius. Handles dips that
exceed 90° by reflecting the point through the centre of the net.

## Usage

``` r
StPointD(az, iang)
```

## Arguments

- az:

  Numeric vector. Azimuth (trend) of the line(s) in degrees.

- iang:

  Numeric vector. Inclination angle of the line(s) in degrees (positive
  downward; values \> 90° are handled via internal quadrant correction).

## Value

A tibble with columns `x` and `y` giving the stereonet coordinates for
each input line.

## See also

Other structural algorithms:
[`Angles()`](https://maxgav13.github.io/GMisc/reference/Angles.md),
[`CartToSphD()`](https://maxgav13.github.io/GMisc/reference/CartToSphD.md),
[`GreatCircleD()`](https://maxgav13.github.io/GMisc/reference/GreatCircleD.md),
[`Pole()`](https://maxgav13.github.io/GMisc/reference/Pole.md),
[`Rotate()`](https://maxgav13.github.io/GMisc/reference/Rotate.md),
[`SmallCircleD()`](https://maxgav13.github.io/GMisc/reference/SmallCircleD.md),
[`SphToCartD()`](https://maxgav13.github.io/GMisc/reference/SphToCartD.md),
[`StCoordLineD()`](https://maxgav13.github.io/GMisc/reference/StCoordLineD.md)

## Examples

``` r
StPointD(az = c(0, 90, 180, 270), iang = c(10, 30, 60, 45))
#> # A tibble: 4 × 2
#>           x         y
#>       <dbl>     <dbl>
#> 1  0         1.23e- 1
#> 2  3.66e- 1  2.24e-17
#> 3  8.66e-17 -7.07e- 1
#> 4 -5.41e- 1 -9.94e-17
```
