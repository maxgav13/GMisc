# Convert Cartesian to spherical coordinates (radians)

Converts North–East–Down direction cosines to the trend and plunge of
the corresponding line in radians.

## Usage

``` r
CartToSph(cn, ce, cd)
```

## Arguments

- cn:

  Numeric. North direction cosine.

- ce:

  Numeric. East direction cosine.

- cd:

  Numeric. Down direction cosine.

## Value

A named numeric vector `c(trd, plg)` with trend and plunge in radians.

## References

Allmendinger, R. W., Cardozo, N., & Fisher, D. M. (2012). Structural
geology algorithms: Vectors and tensors. Cambridge university press.

## Examples

``` r
if (FALSE) { # \dontrun{
# Triple colon required because it's not exported
GMisc:::CartToSph(cn = 0.707, ce = 0.5, cd = 0.5)
} # }
```
