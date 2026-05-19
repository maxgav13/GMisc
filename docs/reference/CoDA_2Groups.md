# CoDA Two Group Hypotheses

Performs the calculations for the hypothesis of equal centers and
covariance (`H1`), of equal covariance with different centers (`H2`), of
equal centers with different covariance (`H3`), and all at once `All`,
for two diferent samples, using Compositional Data Analysis (CoDA)
principles.

## Usage

``` r
CoDA_2Group_H1(comp1, comp2)

CoDA_2Group_H2(comp1, comp2)

CoDA_2Group_H3(comp1, comp2)

CoDA_2Group_All(comp1, comp2)
```

## Arguments

- comp1:

  A matrix or data frame of observations for composition 1. Entries must
  be non-zero and positive.

- comp2:

  A matrix or data frame of observations for composition 2. Entries must
  be non-zero and positive.

## Value

A tibble with the statistic (Q), degrees of freedom (nu), p-value, and
null hypothesis (H0)

## References

Pawlowsky-Hlahn, V., Egozcue, J.J & Tolosna-Delgado, R. (2015). Modeling
and analysis of compositional data. John Wiley & Sons.

Aitchison, J. (1986). The statistical analysis of compositional data.
Chapman and Hall.

## See also

Other compositional data analysis:
[`CoDA_Atyp_Idx()`](https://maxgav13.github.io/GMisc/reference/CoDA_Atyp_Idx.md),
[`CoDA_Univariate()`](https://maxgav13.github.io/GMisc/reference/CoDA_Univariate.md)

## Examples

``` r
data("Hongite", package = 'compositions')
data("Kongite", package = 'compositions')
CoDA_2Group_H1(Hongite,Kongite)
#> # A tibble: 1 × 4
#>       Q    nu      pval H0                          
#>   <dbl> <dbl>     <dbl> <chr>                       
#> 1  45.7    14 0.0000316 equal centers and covariance

data("Hongite", package = 'compositions')
data("Kongite", package = 'compositions')
CoDA_2Group_H2(Hongite,Kongite)
#> # A tibble: 1 × 4
#>       Q    nu  pval H0                                     
#>   <dbl> <dbl> <dbl> <chr>                                  
#> 1  10.7    10 0.384 equal covariance with different centers

data("Hongite", package = 'compositions')
data("Kongite", package = 'compositions')
CoDA_2Group_H3(Hongite,Kongite)
#> # A tibble: 1 × 4
#>       Q    nu        pval H0                                     
#>   <dbl> <int>       <dbl> <chr>                                  
#> 1  33.6     4 0.000000893 equal centers with different covariance

data("Hongite", package = 'compositions')
data("Kongite", package = 'compositions')
CoDA_2Group_All(Hongite,Kongite)
#> # A tibble: 3 × 4
#>       Q    nu        pval H0                                     
#>   <dbl> <dbl>       <dbl> <chr>                                  
#> 1  45.7    14 0.0000316   equal centers and covariance           
#> 2  10.7    10 0.384       equal covariance with different centers
#> 3  33.6     4 0.000000893 equal centers with different covariance
```
