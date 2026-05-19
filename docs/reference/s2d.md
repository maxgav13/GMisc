# Conversion from strike to dip directions

Calculates dip directions from strike directions .

## Usage

``` r
s2d(x)
```

## Arguments

- x:

  A vector of directions in degrees

## Value

A vector or directions

## See also

Other directional analysis:
[`d2s()`](https://maxgav13.github.io/GMisc/reference/d2s.md)

## Examples

``` r
x <- runif(min = 120, max = 170, n = 20)
dip.dir <- s2d(x)
strike <- d2s(dip.dir)
```
