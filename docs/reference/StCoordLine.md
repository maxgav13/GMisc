# Compute stereonet coordinates for a line (radians)

Converts the trend and plunge of a line to its \\(x, y)\\ position on an
equal-area or equal-angle stereonet of unit radius.

## Usage

``` r
StCoordLine(trd, plg, wulff = F)
```

## Arguments

- trd:

  Numeric. Trend of the line in radians.

- plg:

  Numeric. Plunge of the line in radians. Lines plunging into the upper
  hemisphere (negative plunge) are automatically reflected.

- wulff:

  Logical. If `TRUE` uses an equal-angle (Wulff) projection; if `FALSE`
  (default) uses an equal-area (Schmidt) projection.

## Value

A one-row matrix with columns `xp` and `yp` giving the stereonet
coordinates.

## References

Allmendinger, R. W., Cardozo, N., & Fisher, D. M. (2012). Structural
geology algorithms: Vectors and tensors. Cambridge university press.

## Examples

``` r
if (FALSE) { # \dontrun{
# Triple colon required because it's not exported
GMisc:::StCoordLine(trd = pi / 4, plg = pi / 6)
GMisc:::StCoordLine(trd = pi / 3, plg = pi / 4, wulff = TRUE)
} # }
```
