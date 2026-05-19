# Piper Data Preparation

It transforms hydrochemical data (cations and anions) so they can be
plotted on
[`piper_diagram()`](https://maxgav13.github.io/GMisc/reference/piper_diagram.md).

## Usage

``` r
piper_data_prep(data)
```

## Arguments

- data:

  A dataframe with cations and anions, with the following names: Ca, Mg,
  Na, K, Cl, SO4, CO3, HCO3

## Value

A dataframe with `x` and `y` coordinates and the rest of the original
data

## Examples

``` r

d = data.frame(Group = c('A','A','B','B'),
               Ca = c(120,150,110,52.6),
               Mg = c(78,160,110,28),
               Na = c(210,590,340,51.6),
               K = c(4.2,2,3.6,2.3),
               HCO3 = c(181,181,189,151),
               CO3 = 0,
               Cl = c(220,744,476,72.2),
               SO4 = c(560,1020,584,126))

piper_data_prep(d)
#> # A tibble: 12 × 12
#>    ID        x     y Group    Ca    Mg    Na     K  HCO3   CO3    Cl   SO4
#>    <fct> <dbl> <dbl> <fct> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 1      57.4  25.9 A     120      78 210     4.2   181     0 220     560
#>  2 2      69.6  24.8 A     150     160 590     2     181     0 744    1020
#>  3 3      65.9  26.9 B     110     110 340     3.6   189     0 476     584
#>  4 4      47.7  27.8 B      52.6    28  51.6   2.3   151     0  72.2   126
#>  5 1     178.   48.3 A     120      78 210     4.2   181     0 220     560
#>  6 2     190.   40.5 A     150     160 590     2     181     0 744    1020
#>  7 3     188.   36.5 B     110     110 340     3.6   189     0 476     584
#>  8 4     167.   31.7 B      52.6    28  51.6   2.3   151     0  72.2   126
#>  9 1     124.  141.  A     120      78 210     4.2   181     0 220     560
#> 10 2     134.  137.  A     150     160 590     2     181     0 744    1020
#> 11 3     130.  138.  B     110     110 340     3.6   189     0 476     584
#> 12 4     109.  133.  B      52.6    28  51.6   2.3   151     0  72.2   126
```
