# CIPW Norm

`CIPW()` calculates the mineral composition of an igneous rock sample,
based on ther major oxides and weight percent

## Usage

``` r
CIPW(data)
```

## Arguments

- data:

  A dataframe (tibble) with the major oxides in the first column and
  their percent weight in the second column

## Value

Summary table for the CIPW norm, with all the minerals and their percent
weight, as well as separate tables for the ferromagentic minerals and
its components

## Details

The names of the oxides must be: SiO2, TiO2, Al2O3, Fe2O3, FeO, MnO,
MgO, CaO, Na2O, K2O, P2O5. If any of the major oxides is not present
just use 0 for their percent weight

## Examples

``` r
d = data.frame(O = c('SiO2', 'TiO2', 'Al2O3', 'Fe2O3', 'FeO',
                     'MnO', 'MgO', 'CaO', 'Na2O', 'K2O', 'P2O5'),
               W = c(35.6,5.98,12.9,7.68,9.28,.05,
                     5.4,8.46,8.35,2.78,2.13))
CIPW(d)
#> $CIPW
#> # A tibble: 9 × 5
#>   Mineral          Abbr   Result    MW Perc_W
#>   <chr>            <chr>   <dbl> <dbl>  <dbl>
#> 1 Leucite          lc     0.0245  436.  10.7 
#> 2 Nepheline        ne     0.097   284.  27.6 
#> 3 Kaliophilite     kp     0.0051  316.   1.6 
#> 4 Acmite           ac     0.0377  462.  17.4 
#> 5 Olivine          ol    14.0       1   14.0 
#> 6 Calcium-silicate cs     0.0504  172.   8.68
#> 7 Magnetite        mt     0.0104  232.   2.4 
#> 8 Ilmenite         il     0.0748  152.  11.4 
#> 9 Apatite          ap     0.015   336.   5.05
#> 
#> $parts
#> # A tibble: 6 × 8
#>   Mineral     Abbr  Part  MF    MF_prop    MW Result Perc_W
#>   <chr>       <chr> <chr> <chr>   <dbl> <dbl>  <dbl>  <dbl>
#> 1 Diopside    di    mgdi  M       0.750  217. 0        0   
#> 2 Diopside    di    fedi  F       0.250  248. 0        0   
#> 3 Hypersthene hy    en    M       0.750  100. 0        0   
#> 4 Hypersthene hy    fs    F       0.250  132. 0        0   
#> 5 Olivine     ol    fo    M       0.750  141. 0.0745  10.5 
#> 6 Olivine     ol    fa    F       0.250  204. 0.0171   3.49
#> 
#> $parts.summary
#> # A tibble: 3 × 4
#>   Mineral        MW Result Perc_W
#>   <chr>       <dbl>  <dbl>  <dbl>
#> 1 Diopside     224. 0         0  
#> 2 Hypersthene  108. 0         0  
#> 3 Olivine      156. 0.0916   14.0
#> 
```
