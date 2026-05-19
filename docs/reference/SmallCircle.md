# Compute a small-circle path for a stereonet (radians)

Computes the path(s) of a small circle defined by its rotation axis and
cone angle on an equal-area or equal-angle stereonet of unit radius.
When the small circle crosses the primitive, the path is split into two
segments.

## Usage

``` r
SmallCircle(trda, plga, coneAngle, wulff = F, closed = F)
```

## Arguments

- trda:

  Numeric. Trend of the rotation axis in radians.

- plga:

  Numeric. Plunge of the rotation axis in radians.

- coneAngle:

  Numeric. Half-apex (cone) angle in radians.

- wulff:

  Logical. If `TRUE` uses an equal-angle (Wulff) net; if `FALSE`
  (default) uses an equal-area (Schmidt) net.

- closed:

  Logical. If `TRUE` the path segments are closed by appending the first
  point to the end. Default `FALSE`.

## Value

A named list with components:

- `path1`:

  Matrix of stereonet coordinates for the main segment.

- `path2`:

  Matrix of stereonet coordinates for the secondary segment (empty when
  the circle does not cross the primitive).

## References

Allmendinger, R. W., Cardozo, N., & Fisher, D. M. (2012). Structural
geology algorithms: Vectors and tensors. Cambridge university press.

## Examples

``` r
if (FALSE) { # \dontrun{
# Triple colon required because it's not exported
GMisc:::SmallCircle(trda = pi / 4, plga = pi / 6, coneAngle = pi / 8)
} # }
```
