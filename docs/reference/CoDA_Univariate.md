# CoDA Univariate Statistics

Calculates univariate statistics for the parts of a given composition,
using Compositional Data Analysis (CoDA) principles.

## Usage

``` r
CoDA_Univariate(data, ppm_vars = NULL)
```

## Arguments

- data:

  A dataframe of observations for a given composition. Entries must be
  non-zero and positive.

- ppm_vars:

  A character vector with the names of the parts of the data that are in
  parts-per-million (ppm).

## Value

A tibble with univariate statistics (mean, median, sd-ilr, mad-ilr) for
the given composition, in their original scale (percent and/or ppm). For
a more detailed explanation check the reference.

## References

Filzmoser, P., Hron, K. & Reimann, C. (2009). Univariate statistical
analysis of environmental (compositional) data: Problems and
possibilities. Science of The Total Environment, 407(23), 6100-6108.
10.1016/j.scitotenv.2009.08.008.

## See also

Other compositional data analysis:
[`CoDA_2Group_H1()`](https://maxgav13.github.io/GMisc/reference/CoDA_2Groups.md),
[`CoDA_Atyp_Idx()`](https://maxgav13.github.io/GMisc/reference/CoDA_Atyp_Idx.md)

## Examples

``` r

data("Aar", package = 'compositions')
d1 = Aar %>% dplyr::select(SiO2,Al2O3,MnO)
d2 = Aar %>% dplyr::select(SiO2,Al2O3,Ba,Pb)
CoDA_Univariate(d1) # with no variables in ppm
#> # A tibble: 3 × 5
#>   variable    Mean  Median SD_ilr MAD_ilr
#>   <chr>      <dbl>   <dbl>  <dbl>   <dbl>
#> 1 SiO2     71.4    72.8     0.279   0.293
#> 2 Al2O3    14.1    14.5     0.186   0.234
#> 3 MnO       0.0443  0.0380  0.443   0.439
CoDA_Univariate(d2, ppm_vars = c('Ba','Pb')) # with Ba and Pb variables in ppm
#> # A tibble: 4 × 5
#>   variable  Mean Median SD_ilr MAD_ilr
#>   <chr>    <dbl>  <dbl>  <dbl>   <dbl>
#> 1 SiO2      71.4   72.8  0.279   0.293
#> 2 Al2O3     14.1   14.5  0.186   0.234
#> 3 Ba       653.   653.   0.272   0.296
#> 4 Pb        16.1   14.0  0.357   0.253
```
