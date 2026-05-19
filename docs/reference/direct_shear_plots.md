# Draws the results for a direct shear test

Draws the sigma-tau pairs along with the Mohr-Coulomb failure envelope
for a set of direct shear lab test results, optionally with Mohr
circles, and annotates the graph with the values of cohesion and
friction angle.

## Usage

``` r
DS_plot(sig.n, tau, units = "kPa")

DS_Mohr_plot(sig.n, tau, units = "kPa")
```

## Arguments

- sig.n:

  A numeric vector of the normal stress values

- tau:

  A numeric vector of the shear stress values

- units:

  A string with the units of the measurements

## Value

A ggplot of the direct shear test results (`DS_plot`) or with the Mohr
circles (`DS_Mohr_plot`)

## References

Coduto, D. P. (1999). Geotechnical Engineering - Principles and
Practices. Prentice Hall.

Holtz, R. D., Kovacs, W. D. & Sheahan, T. C. (2011). An Introduction to
Geotechnical Engineering. Prentice Hall.

Gonzalez de Vallejo, L. I. (2004). Ingenieria Geologica. Prentice Hall.

## See also

Other soil mechanics:
[`Casagrande_LL()`](https://maxgav13.github.io/GMisc/reference/Casagrande_LL.md),
[`Mohr_Circle()`](https://maxgav13.github.io/GMisc/reference/Mohr_Circle.md),
[`bearing_capacity()`](https://maxgav13.github.io/GMisc/reference/bearing_capacity.md),
[`compaction_test()`](https://maxgav13.github.io/GMisc/reference/compaction_test.md),
[`fallcone_limits()`](https://maxgav13.github.io/GMisc/reference/fallcone_limits.md),
[`induced_stress()`](https://maxgav13.github.io/GMisc/reference/induced_stress.md),
[`insitu_stress()`](https://maxgav13.github.io/GMisc/reference/insitu_stress.md),
[`particle_size_graph()`](https://maxgav13.github.io/GMisc/reference/particle_size_graph.md)

## Examples

``` r
sig.n = c(80,237,395)
tau = c(127,345,475)
DS_plot(sig.n, tau)

DS_Mohr_plot(sig.n, tau)

```
