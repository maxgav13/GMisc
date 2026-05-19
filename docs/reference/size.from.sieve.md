# Get particle size from sieve number

Converts ASTM sieve numbers to particle sizes in millimetres or inches.

## Usage

``` r
size.from.sieve(sieve, metric = T)
```

## Arguments

- sieve:

  A numeric vector of ASTM sieve numbers (e.g. 4, 10, 200).

- metric:

  Logical. If `TRUE` (default), returns sizes in millimetres; if
  `FALSE`, returns sizes in inches.

## Value

A numeric vector of particle sizes corresponding to each sieve number.

## Details

This function is taken from the "geotech" package from James Kaklamanos

## See also

Other soil mechanics, classification:
[`Dsize()`](https://maxgav13.github.io/GMisc/reference/Dsize.md),
[`USCS()`](https://maxgav13.github.io/GMisc/reference/USCS.md),
[`USCS.coarse.symbol()`](https://maxgav13.github.io/GMisc/reference/USCS.coarse.symbol.md),
[`USCS.fine.symbol()`](https://maxgav13.github.io/GMisc/reference/USCS.fine.symbol.md),
[`grainSize.coefs()`](https://maxgav13.github.io/GMisc/reference/grainSize.coefs.md),
[`percentComponents()`](https://maxgav13.github.io/GMisc/reference/percentComponents.md)

## Examples

``` r
size.from.sieve(c(4, 10, 40, 200))
#> [1] 4.750 2.000 0.425 0.075
size.from.sieve(c(4, 10, 200), metric = FALSE)
#> [1] 0.18700 0.07870 0.00295
```
