# Compute uniformity and curvature coefficients from a grain-size distribution

Calculates the uniformity coefficient and the curvature coefficient from
percent passing data. The characteristic diameters can be supplied
directly or derived from sieve numbers or particle sizes.

## Usage

``` r
grainSize.coefs(
  percent,
  sieve = NA,
  size = NA,
  D10 = NA,
  D30 = NA,
  D60 = NA,
  metric = T
)
```

## Arguments

- percent:

  Numeric vector of percent passing values (0–100).

- sieve:

  Numeric vector of ASTM sieve numbers. Used when `size` is not
  provided. Default `NA`.

- size:

  Numeric vector of particle sizes (mm if `metric = TRUE`, inches
  otherwise). Default `NA`.

- D10:

  Diameter at 10 percent passing. If `NA` (default) it is interpolated
  from the data.

- D30:

  Diameter at 30 percent passing. If `NA` (default) it is interpolated
  from the data.

- D60:

  Diameter at 60 percent passing. If `NA` (default) it is interpolated
  from the data.

- metric:

  Logical. If `TRUE` (default) sizes are in millimetres.

## Value

A named list with components `Cu` (uniformity coefficient) and `Cc`

## Details

This function is taken from the "geotech" package from James Kaklamanos
(curvature coefficient).

## See also

Other soil mechanics, classification:
[`Dsize()`](https://maxgav13.github.io/GMisc/reference/Dsize.md),
[`USCS()`](https://maxgav13.github.io/GMisc/reference/USCS.md),
[`USCS.coarse.symbol()`](https://maxgav13.github.io/GMisc/reference/USCS.coarse.symbol.md),
[`USCS.fine.symbol()`](https://maxgav13.github.io/GMisc/reference/USCS.fine.symbol.md),
[`percentComponents()`](https://maxgav13.github.io/GMisc/reference/percentComponents.md),
[`size.from.sieve()`](https://maxgav13.github.io/GMisc/reference/size.from.sieve.md)

## Examples

``` r
grainSize.coefs(
  percent = c(0, 10, 30, 60, 90, 100),
  size    = c(0.075, 0.15, 0.3, 0.6, 1.18, 2.36)
)
#> $Cu
#> [1] 4
#> 
#> $Cc
#> [1] 1
#> 
```
