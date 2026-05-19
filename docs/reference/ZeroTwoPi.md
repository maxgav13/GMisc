# Constrain an azimuth to the range \[0, 2π\\

Wraps an azimuth value in radians so that it always lies in the interval
\[0, 2π).

## Usage

``` r
ZeroTwoPi(a)
```

## Arguments

- a:

  Numeric. Input azimuth in radians (may be outside \[0, 2π)).

## Value

A numeric value: the equivalent azimuth in \[0, 2π) (radians).

## References

Allmendinger, R. W., Cardozo, N., & Fisher, D. M. (2012). Structural
geology algorithms: Vectors and tensors. Cambridge university press.

## Examples

``` r
if (FALSE) { # \dontrun{
# Triple colon required because it's not exported
GMisc:::ZeroTwoPi(-0.5)
GMisc:::ZeroTwoPi(7)
} # }
```
