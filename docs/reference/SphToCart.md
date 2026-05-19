# Convert spherical to Cartesian coordinates (radians)

Converts a line or plane orientation given in spherical coordinates
(trend/plunge or strike/dip) to direction cosines in a North–East–Down
coordinate system.

## Usage

``` r
SphToCart(trd, plg, k = 0)
```

## Arguments

- trd:

  Numeric. Trend of the line (radians, `k = 0`) or strike of the plane
  (radians, right-hand rule, `k = 1`).

- plg:

  Numeric. Plunge of the line (radians, `k = 0`) or dip of the plane
  (radians, `k = 1`).

- k:

  Integer. `0` for a line (trend/plunge); `1` for a plane (strike/dip,
  right-hand rule). Default `0`.

## Value

A named numeric vector `c(cn, ce, cd)` with the north, east, and down
direction cosines.

## References

Allmendinger, R. W., Cardozo, N., & Fisher, D. M. (2012). Structural
geology algorithms: Vectors and tensors. Cambridge university press.

## Examples

``` r
if (FALSE) { # \dontrun{
# Triple colon required because it's not exported
GMisc:::SphToCart(trd = pi / 4, plg = pi / 6)
GMisc:::SphToCart(trd = pi / 3, plg = pi / 5, k = 1)
} # }
```
