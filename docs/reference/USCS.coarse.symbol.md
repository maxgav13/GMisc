# Determine the USCS symbol for coarse-grained soils

Classifies gravel and sand soils according to the Unified Soil
Classification System (USCS / ASTM D2487) from gradation characteristics
and, when fines exceed 5 percent, Atterberg limits.

## Usage

``` r
USCS.coarse.symbol(pg, ps, pf, Cc, Cu, LL, PL, PI = NA)
```

## Arguments

- pg:

  Numeric. Percent gravel (retained on the No. 4 / 4.75 mm sieve).

- ps:

  Numeric. Percent sand.

- pf:

  Numeric. Percent fines (passing No. 200 / 0.075 mm sieve).

- Cc:

  Numeric. Curvature coefficient.

- Cu:

  Numeric. Uniformity coefficient.

- LL:

  Numeric. Liquid limit (percent). Required when `pf` is between 5 and
  12, or greater than 12.

- PL:

  Numeric. Plastic limit (percent). Used together with `LL` to derive
  `PI`.

- PI:

  Numeric. Plasticity index (percent). Default `NA`; computed from `LL`
  and `PL` if not provided.

## Value

A character string with the USCS group symbol (e.g. `"GW"`, `"SP-SM"`).

## Details

This function is taken from the "geotech" package from James Kaklamanos

## See also

Other soil mechanics, classification:
[`Dsize()`](https://maxgav13.github.io/GMisc/reference/Dsize.md),
[`USCS()`](https://maxgav13.github.io/GMisc/reference/USCS.md),
[`USCS.fine.symbol()`](https://maxgav13.github.io/GMisc/reference/USCS.fine.symbol.md),
[`grainSize.coefs()`](https://maxgav13.github.io/GMisc/reference/grainSize.coefs.md),
[`percentComponents()`](https://maxgav13.github.io/GMisc/reference/percentComponents.md),
[`size.from.sieve()`](https://maxgav13.github.io/GMisc/reference/size.from.sieve.md)

## Examples

``` r
USCS.coarse.symbol(pg = 60, ps = 35, pf = 5, Cc = 1.5, Cu = 5,
                   LL = 0, PL = 0, PI = 0)
#> [1] "GW-GC"
```
