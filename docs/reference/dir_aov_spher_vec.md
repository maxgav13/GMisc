# Test for common mean direction (vectorial)

Tests whether two or more groups of 3-D orientations share a common mean
direction applicable to spherical data with multiple groups. The test
compares the within-group mean vectors via a weighted sum of squared
discrepancies and returns a conclusion statement, per-group vector
statistics, the pooled test result, and an equal-area stereonet.

## Usage

``` r
dir_aov_spher_vec(
  trd,
  plg,
  grp,
  data = NULL,
  conf.level = 0.95,
  type = c("line", "dir", "strike")
)
```

## Arguments

- trd:

  Numeric vector of trend values in degrees, or an unquoted column name
  when `data` is supplied. For lines: trend of the lineation. For
  planes: dip direction or strike depending on `type`.

- plg:

  Numeric vector of plunge values in degrees, or an unquoted column name
  when `data` is supplied. For lines: plunge. For planes: dip angle.

- grp:

  A vector (factor or character) of group labels, or an unquoted column
  name when `data` is supplied. Must be the same length as `trd` and
  `plg`.

- data:

  A data frame (or tibble) containing the columns referenced by `trd`,
  `plg`, and `grp`. Default `NULL` (use vectors directly).

- conf.level:

  Numeric. Confidence level for per-group and pooled mean direction
  confidence cones. Default `0.95`.

- type:

  Character. How to interpret the input orientations:

  `"line"`

  :   (default) Trend and plunge of a lineation.

  `"dir"`

  :   Dip direction and dip angle of a plane (pole used internally).

  `"strike"`

  :   Strike (right-hand rule) and dip angle of a plane (pole used
      internally).

## Value

A named list with four components:

- `statement`:

  Character string summarising the test conclusion.

- `vector_g`:

  Tibble with per-group statistics: group label, sample size `N`,
  resultant length `R`, mean resultant length `Rbar`, concentration
  parameter `kappa`, mean trend `trd`, mean plunge `plg`, and confidence
  cone half-width `cone` (degrees).

- `vector_res`:

  Tibble with the test statistic `Nr`, degrees of freedom `df`, and
  p-value `p.value`. When the null hypothesis is not rejected, the
  pooled mean direction statistics are appended as additional columns.

- `stereoplot`:

  A ggplot equal-area stereonet showing the data points, per-group mean
  directions (coloured triangles) with confidence cones, and—when the
  null is not rejected—the pooled mean direction (gold triangle) with
  its confidence cone.

## Details

When the null hypothesis is not rejected (samples share a common mean),
the pooled mean direction and its confidence cone are overlaid in gold
on the stereonet. Per-group mean directions and their confidence cones
are always shown in distinct colours.

## References

Fisher, N. I., Lewis, T., & Embleton, B. J. J. (1987). Statistical
analysis of spherical data. Cambridge University Press.

## See also

Other directional statistics:
[`dir_aov_spher_ax()`](https://maxgav13.github.io/GMisc/reference/dir_aov_spher_ax.md),
[`dir_stats_circ()`](https://maxgav13.github.io/GMisc/reference/dir_stats_circ.md),
[`dir_stats_spher()`](https://maxgav13.github.io/GMisc/reference/dir_stats_spher.md),
[`rose_diag_circ()`](https://maxgav13.github.io/GMisc/reference/rose_diag_circ.md),
[`rose_diag_spher()`](https://maxgav13.github.io/GMisc/reference/rose_diag_spher.md)

## Examples

``` r
set.seed(123)
trds <- c(runif(10, 50, 70), runif(10, 70, 90))
plgs <- c(runif(10, 20, 40), runif(10, 30, 50))
grps <- rep(c("A","B"), each=10)
res <- dir_aov_spher_vec(trds, plgs, grps)
res$statement
#> [1] "Mean direction differs between samples"
res$vector_res
#> # A tibble: 1 × 3
#>      Nr    df  p.value
#>   <dbl> <dbl>    <dbl>
#> 1  54.0     2 1.87e-12
res$stereoplot


# From a data frame
csv_path <- system.file("spherical_data_vec_aov.csv", package = "GMisc")
df <- utils::read.csv(csv_path)
dir_aov_spher_vec(trend, plunge, type, data = df)
#> $statement
#> [1] "Samples come from a distribution with common mean direction"
#> 
#> $vector_g
#> # A tibble: 4 × 9
#>   grp       N     R  Rbar kappa   sigc   trd   plg  cone
#>   <chr> <int> <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl>
#> 1 A        36  35.7 0.992 115.  0.0218 17.3  -76.3  2.16
#> 2 B        39  38.8 0.994 162.  0.0176 10.4  -77.2  1.75
#> 3 C        16  15.7 0.981  48.7 0.0494 11.0  -75.1  4.91
#> 4 D        16  15.8 0.989  86.0 0.0371  7.62 -80.9  3.69
#> 
#> $vector_res
#> # A tibble: 1 × 16
#>      Nr    df p.value   trd   plg strike   dir   dip     R  Rbar kappa  cone
#>   <dbl> <dbl>   <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  10.8     6  0.0956  12.7 -77.2   103.  193.  167.  106. 0.990  97.9  1.39
#> # ℹ 4 more variables: N <int>, x <dbl>, y <dbl>, z <dbl>
#> 
#> $stereoplot

#> 
```
