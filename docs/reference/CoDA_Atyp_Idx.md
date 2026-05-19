# CoDA Atypicality Index

Calculates the atypicality index (ranges between 0 and 1, 1 being more
atypical/different) for all the observations of a composition or
compares one sample to the given composition, using Compositional Data
Analysis (CoDA) principles.

## Usage

``` r
CoDA_Atyp_Idx(comp, sample = NULL, thres = 0.95)
```

## Arguments

- comp:

  A matrix or data frame of observations for a given composition.
  Entries must be non-zero and positive.

- sample:

  A one sample vector to compare against the given composition.

- thres:

  Threshold to classify observation as atypical, from 0 to 1 (Default is
  0.95)

## Value

A matrix with the atypicality index or indices.

## References

Aitchison, J. (1986). The statistical analysis of compositional data.
Chapman and Hall.

## See also

Other compositional data analysis:
[`CoDA_2Group_H1()`](https://maxgav13.github.io/GMisc/reference/CoDA_2Groups.md),
[`CoDA_Univariate()`](https://maxgav13.github.io/GMisc/reference/CoDA_Univariate.md)

## Examples

``` r
data("Hongite", package = 'compositions')
samp = c(44, 20.4, 13.9, 9.1, 12.6)
CoDA_Atyp_Idx(Hongite)
#> New names:
#> • `` -> `...1`
#> # A tibble: 25 × 2
#>       idx Atypical
#>     <dbl> <chr>   
#>  1 0.712  No      
#>  2 0.0171 No      
#>  3 0.199  No      
#>  4 0.0318 No      
#>  5 0.709  No      
#>  6 0.828  No      
#>  7 0.882  No      
#>  8 0.969  Yes     
#>  9 0.1    No      
#> 10 0.987  Yes     
#> # ℹ 15 more rows
CoDA_Atyp_Idx(Hongite, samp)
#> New names:
#> • `` -> `...1`
#> # A tibble: 1 × 2
#>     idx Atypical
#>   <dbl> <chr>   
#> 1 0.998 Yes     
```
