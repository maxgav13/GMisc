# Determine the USCS symbol for fine-grained soils

Classifies silt and clay soils according to the Unified Soil
Classification System (USCS / ASTM D2487) using the Atterberg limits and
the A-line.

## Usage

``` r
USCS.fine.symbol(LL, PL = NA, PI = NA)
```

## Arguments

- LL:

  Numeric. Liquid limit (percent).

- PL:

  Numeric. Plastic limit (percent). Default `NA`; used to compute `PI`
  when `PI` is not provided.

- PI:

  Numeric. Plasticity index (percent). Default `NA`; computed as
  `LL - PL` when not supplied.

## Value

A character string with the USCS group symbol (e.g. `"CL"`, `"MH"`,
`"CL-ML"`).

## Details

This function is taken from the "geotech" package from James Kaklamanos

## See also

Other soil mechanics, classification:
[`Dsize()`](https://maxgav13.github.io/GMisc/reference/Dsize.md),
[`USCS()`](https://maxgav13.github.io/GMisc/reference/USCS.md),
[`USCS.coarse.symbol()`](https://maxgav13.github.io/GMisc/reference/USCS.coarse.symbol.md),
[`grainSize.coefs()`](https://maxgav13.github.io/GMisc/reference/grainSize.coefs.md),
[`percentComponents()`](https://maxgav13.github.io/GMisc/reference/percentComponents.md),
[`size.from.sieve()`](https://maxgav13.github.io/GMisc/reference/size.from.sieve.md)

## Examples

``` r
USCS.fine.symbol(LL = 40, PL = 20)
#> [1] "CL"
USCS.fine.symbol(LL = 60, PI = 25)
#> [1] "MH"
```
