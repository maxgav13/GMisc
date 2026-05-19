# Mohr circle solution and plot

Solves a Mohr circle problem for the given normal stresses, shear
stress, and rotation angle, and plots the results.

## Usage

``` r
Mohr_Circle(sigx, sigy, tauxy, theta = 0, digits = 5, units = "kPa")
```

## Arguments

- sigx:

  A number corresponding with the mayor normal stress

- sigy:

  A number corresponding with the minor normal stress

- tauxy:

  A number corresponding with the shear stress

- theta:

  Angle of rotation for which to find the state of stresses

- digits:

  Number of significant digits to us

- units:

  A string with the units of the measurements

## Value

A list containing a data frame with the results, and a ggplot object
with the Mohr circle

## References

Coduto, D. P. (1999). Geotechnical Engineering - Principles and
Practices. Prentice Hall.

Holtz, R. D., Kovacs, W. D. & Sheahan, T. C. (2011). An Introduction to
Geotechnical Engineering. Prentice Hall.

Gonzalez de Vallejo, L. I. (2004). Ingenieria Geologica. Prentice Hall.

## See also

Other soil mechanics:
[`Casagrande_LL()`](https://maxgav13.github.io/GMisc/reference/Casagrande_LL.md),
[`DS_plot()`](https://maxgav13.github.io/GMisc/reference/direct_shear_plots.md),
[`bearing_capacity()`](https://maxgav13.github.io/GMisc/reference/bearing_capacity.md),
[`compaction_test()`](https://maxgav13.github.io/GMisc/reference/compaction_test.md),
[`fallcone_limits()`](https://maxgav13.github.io/GMisc/reference/fallcone_limits.md),
[`induced_stress()`](https://maxgav13.github.io/GMisc/reference/induced_stress.md),
[`insitu_stress()`](https://maxgav13.github.io/GMisc/reference/insitu_stress.md),
[`particle_size_graph()`](https://maxgav13.github.io/GMisc/reference/particle_size_graph.md)

## Examples

``` r
sigx = 143.6
sigy = 100.5
tauxy = -14.4
Mohr_Circle(sigx, sigy, tauxy)
#> $results
#>     sig1   sig2 sig_theta tau_theta tau_max
#> 1 147.97 96.132     143.6     -14.4  25.918
#> 
#> $figure

#> 
Mohr_Circle(sigx, sigy, tauxy, theta = -35)
#> $results
#>     sig1   sig2 sig_theta tau_theta tau_max
#> 1 147.97 96.132    115.89   -25.175  25.918
#> 
#> $figure

#> 
```
