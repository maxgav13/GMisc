# Interpolate a characteristic diameter from a grain-size distribution

Returns the diameter at which `N` percent of the sample (by mass) is
finer, using log-linear interpolation. Extrapolation is used when `N`
falls outside the range of `percent`, with a warning.

## Usage

``` r
Dsize(N, sieve = NA, size = NA, percent, metric = T)
```

## Arguments

- N:

  Numeric. The desired percent-finer value (e.g. 10, 30, or 60).

- sieve:

  Numeric vector of ASTM sieve numbers. Used to derive `size` when
  `size` is `NA`. Default `NA`.

- size:

  Numeric vector of particle sizes corresponding to `percent`. Default
  `NA`.

- percent:

  Numeric vector of percent passing values (0–100).

- metric:

  Logical. If `TRUE` (default) `size` is in millimetres.

## Value

A single numeric value: the particle diameter at `N` percent passing.

## Details

This function is taken from the "geotech" package from James Kaklamanos

## See also

Other soil mechanics, classification:
[`USCS()`](https://maxgav13.github.io/GMisc/reference/USCS.md),
[`USCS.coarse.symbol()`](https://maxgav13.github.io/GMisc/reference/USCS.coarse.symbol.md),
[`USCS.fine.symbol()`](https://maxgav13.github.io/GMisc/reference/USCS.fine.symbol.md),
[`grainSize.coefs()`](https://maxgav13.github.io/GMisc/reference/grainSize.coefs.md),
[`percentComponents()`](https://maxgav13.github.io/GMisc/reference/percentComponents.md),
[`size.from.sieve()`](https://maxgav13.github.io/GMisc/reference/size.from.sieve.md)

## Examples

``` r
Dsize(
  N       = 60,
  percent = c(0, 10, 30, 60, 90, 100),
  size    = c(0.075, 0.15, 0.3, 0.6, 1.18, 2.36)
)
#> [1] 0.6
```
