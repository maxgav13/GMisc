# Compute a great-circle path for a stereonet (radians)

Computes the path of the great circle (trace) of a plane on an
equal-area or equal-angle stereonet of unit radius.

## Usage

``` r
GreatCircle(strike, dip, wulff = F)
```

## Arguments

- strike:

  Numeric. Strike of the plane in radians (right-hand rule).

- dip:

  Numeric. Dip of the plane in radians.

- wulff:

  Logical. If `TRUE` uses an equal-angle (Wulff) net; if `FALSE`
  (default) uses an equal-area (Schmidt) net.

## Value

Numeric matrix with columns `xp` and `yp` giving the stereonet
coordinates of the great-circle path.

## References

Allmendinger, R. W., Cardozo, N., & Fisher, D. M. (2012). Structural
geology algorithms: Vectors and tensors. Cambridge university press.

## Examples

``` r
if (FALSE) { # \dontrun{
# Triple colon required because it's not exported
GMisc:::GreatCircle(strike = pi / 4, dip = pi / 6)
GMisc:::GreatCircle(strike = pi / 3, dip = pi / 4, wulff = TRUE)
} # }
```
