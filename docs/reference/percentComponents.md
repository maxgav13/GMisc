# Estimate gravel, sand, and fines content

Computes the percentages of gravel (greater than 4.75 mm), sand, and
fines (smaller than 0.075 mm) from a grain-size distribution curve.

## Usage

``` r
percentComponents(sieve = NA, size = NA, percent, metric = T)
```

## Arguments

- sieve:

  Numeric vector of ASTM sieve numbers. Used to derive `size` when
  `size` is `NA`. Default `NA`.

- size:

  Numeric vector of particle sizes. Default `NA`.

- percent:

  Numeric vector of percent passing values (0–100).

- metric:

  Logical. If `TRUE` (default) `size` is in millimetres; if `FALSE` it
  is in inches.

## Value

A named list with components:

- pg:

  Percent gravel.

- ps:

  Percent sand.

- pf:

  Percent fines.

## Details

This function is taken from the "geotech" package from James Kaklamanos

## See also

Other soil mechanics, classification:
[`Dsize()`](https://maxgav13.github.io/GMisc/reference/Dsize.md),
[`USCS()`](https://maxgav13.github.io/GMisc/reference/USCS.md),
[`USCS.coarse.symbol()`](https://maxgav13.github.io/GMisc/reference/USCS.coarse.symbol.md),
[`USCS.fine.symbol()`](https://maxgav13.github.io/GMisc/reference/USCS.fine.symbol.md),
[`grainSize.coefs()`](https://maxgav13.github.io/GMisc/reference/grainSize.coefs.md),
[`size.from.sieve()`](https://maxgav13.github.io/GMisc/reference/size.from.sieve.md)

## Examples

``` r
percentComponents(
  percent = c(0, 10, 40, 70, 90, 100),
  size    = c(0.075, 0.25, 0.6, 2, 4.75, 19)
)
#> $pg
#> [1] 10
#> 
#> $ps
#> [1] 90
#> 
#> $pf
#> [1] 0
#> 
```
