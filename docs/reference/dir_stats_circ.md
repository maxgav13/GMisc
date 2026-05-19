# Circular (directional) descriptive statistics

Computes descriptive statistics for a sample of angular measurements
(azimuths or orientations) using circular statistics. Optionally pulls
the data column from a data frame. Handles both vectorial data
(azimuths, 0°–360°) and axial data (strikes or lineations, 0°–180°) via
the `dir` argument, and returns the mean direction, resultant length,
circular variance, circular standard deviation, circular dispersion,
concentration parameter, and a confidence cone for the mean direction.

## Usage

``` r
dir_stats_circ(x, data = NULL, dir = 1, conf.level = 0.95)
```

## Arguments

- x:

  Numeric vector of angles in degrees, or an unquoted column name when
  `data` is supplied.

- data:

  A data frame (or tibble) containing the column referenced by `x`.
  Default `NULL` (use `x` directly as a vector).

- dir:

  Integer. Data type: `1` for vectorial data (full 360° range, default);
  `0` for axial data (180° range, e.g. strike or foliation— angles are
  doubled internally and the mean is halved back).

- conf.level:

  Numeric. Confidence level for the mean-direction cone of confidence.
  Default `0.95`.

## Value

A one-row data frame with the following columns:

- `theta.hat`:

  Mean direction in degrees (0°–360°).

- `N`:

  Sample size.

- `R`:

  Length of the resultant vector.

- `Rbar`:

  Mean resultant length (R / N); ranges from 0 (uniform) to 1 (perfectly
  concentrated).

- `circ.var`:

  Circular variance (1 − Rbar).

- `circ.sd`:

  Circular standard deviation.

- `circ.disp`:

  Circular dispersion.

- `kappa`:

  Estimated concentration parameter of the von Mises distribution.

- `cone`:

  Half-width of the `conf.level` confidence cone around the mean
  direction (degrees).

## References

Fisher, N. I. (1993). Statistical analysis of circular data. Cambridge
University Press.

Davis, J. C. (2002). Statistics and data analysis in geology (3rd ed).
J. Wiley.

Swan, A. R. H., & Sandilands, M. (1995). Introduction to geological data
analysis. Blackwell Science.

## See also

Other directional statistics:
[`dir_aov_spher_ax()`](https://maxgav13.github.io/GMisc/reference/dir_aov_spher_ax.md),
[`dir_aov_spher_vec()`](https://maxgav13.github.io/GMisc/reference/dir_aov_spher_vec.md),
[`dir_stats_spher()`](https://maxgav13.github.io/GMisc/reference/dir_stats_spher.md),
[`rose_diag_circ()`](https://maxgav13.github.io/GMisc/reference/rose_diag_circ.md),
[`rose_diag_spher()`](https://maxgav13.github.io/GMisc/reference/rose_diag_spher.md)

## Examples

``` r
angles = c(255, 239, 222, 231, 199, 271, 222, 274, 228, 246,
           177, 199, 257, 201, 237, 209, 216, 180, 182, 250,
           219, 196, 197, 246, 218, 235, 232, 243, 232, 180,
           231, 254, 242, 149, 212, 210, 230, 205, 220, 268)
# Vectorial data supplied as a plain vector
dir_stats_circ(angles)
#>   theta.hat  N     R   Rbar circ.var circ.sd circ.disp kappa cone
#> 1     223.2 40 35.61 0.8901   0.1099  0.4825    0.2355 4.847 8.58

# Axial data (strikes)
dir_stats_circ(angles, dir = 0)
#>   theta.hat  N     R   Rbar circ.var circ.sd circ.disp kappa cone
#> 1      44.5 40 25.07 0.6268   0.3732  0.9666     1.066 1.623 8.84

# Using tidy column selection from a data frame
df <- data.frame(azi = angles)
dir_stats_circ(azi, data = df, conf.level = 0.90)
#>   theta.hat  N     R   Rbar circ.var circ.sd circ.disp kappa cone
#> 1     223.2 40 35.61 0.8901   0.1099  0.4825    0.2355 4.847 7.19
```
