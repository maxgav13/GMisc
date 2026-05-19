# Classify a soil using the Unified Soil Classification System (USCS)

Determines the USCS group symbol and descriptive name (ASTM D2487) for a
soil sample. Inputs can be provided as percent components and gradation
coefficients, or derived automatically from a grain-size distribution
and Atterberg limits.

## Usage

``` r
USCS(
  pg = NA,
  ps = NA,
  pf = NA,
  Cc = NA,
  Cu = NA,
  LL = NA,
  PL = NA,
  PI = NA,
  sieve = NA,
  size = NA,
  percent = NA,
  metric = T,
  spanish = F
)
```

## Arguments

- pg:

  Numeric. Percent gravel. Default `NA`.

- ps:

  Numeric. Percent sand. Default `NA`.

- pf:

  Numeric. Percent fines. Default `NA`.

- Cc:

  Numeric. Curvature coefficient. Default `NA`.

- Cu:

  Numeric. Uniformity coefficient. Default `NA`.

- LL:

  Numeric. Liquid limit (percent). Default `NA`.

- PL:

  Numeric. Plastic limit (percent). Default `NA`.

- PI:

  Numeric. Plasticity index (percent). Default `NA`; computed from `LL`
  and `PL` when missing.

- sieve:

  Numeric vector of ASTM sieve numbers. Used to derive `size` when
  `size` is `NA`. Default `NA`.

- size:

  Numeric vector of particle sizes. Default `NA`.

- percent:

  Numeric vector of percent passing values (0–100). Default `NA`.

- metric:

  Logical. If `TRUE` (default) `size` is in millimetres.

- spanish:

  Logical. If `TRUE` the group name is returned in Spanish; default
  `FALSE` (English).

## Value

A named list with:

- symbol:

  USCS group symbol (character).

- name:

  Descriptive group name (character).

- pg:

  Percent gravel.

- ps:

  Percent sand.

- pf:

  Percent fines.

- Cu:

  Uniformity coefficient (only for coarse soils with \\pf \le 12\\).

- Cc:

  Curvature coefficient (only for coarse soils with \\pf \le 12\\).

## Details

This function is taken from the "geotech" package from James Kaklamanos

## See also

Other soil mechanics, classification:
[`Dsize()`](https://maxgav13.github.io/GMisc/reference/Dsize.md),
[`USCS.coarse.symbol()`](https://maxgav13.github.io/GMisc/reference/USCS.coarse.symbol.md),
[`USCS.fine.symbol()`](https://maxgav13.github.io/GMisc/reference/USCS.fine.symbol.md),
[`grainSize.coefs()`](https://maxgav13.github.io/GMisc/reference/grainSize.coefs.md),
[`percentComponents()`](https://maxgav13.github.io/GMisc/reference/percentComponents.md),
[`size.from.sieve()`](https://maxgav13.github.io/GMisc/reference/size.from.sieve.md)

## Examples

``` r
USCS(pg = 15, ps = 34, pf = 51, Cc = 1, Cu = 4, LL = 40, PL = 10)
#> $symbol
#> [1] "CL"
#> 
#> $name
#> [1] "Sandy lean clay with gravel"
#> 
#> $pg
#> [1] 15
#> 
#> $ps
#> [1] 34
#> 
#> $pf
#> [1] 51
#> 
```
