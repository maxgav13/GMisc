# Conversion from dip directions to strike

Calculates strike directions from dip directions.

## Usage

``` r
d2s(x)
```

## Arguments

- x:

  A vector of directions in degrees

## Value

A vector or directions

## See also

Other directional analysis:
[`s2d()`](https://maxgav13.github.io/GMisc/reference/s2d.md)

## Examples

``` r
x <- runif(min = 30, max = 80, n = 20)
strike <- d2s(x)
dip.dir <- s2d(strike)
```
