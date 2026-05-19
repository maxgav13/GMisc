# Directional statistics two-sample test (2D)

Performes the two-sample test, for the 2D case, for a pair of angle
measurements to determine if they could belong to the same population
(event).

## Usage

``` r
dir_2sample_test_circ(x, y, dir = 1, conf.level = 0.95)
```

## Arguments

- x:

  A vector of angular measurements in degrees for sample 1

- y:

  A vector of angular measurements in degrees for sample 2

- dir:

  A logical indicating if the data are directional (1) or
  non-directional (0)

- conf.level:

  Confidence level for the F-statistic calculation and p-value
  interpretation (Default is 0.95)

## Value

A list with the F-statistic, the critical value of F, the degrees of
freedom, the p-value, and the interpretation of these values

## Details

Directional data refers to dipping planes or lines, data expressed as
dip direction (trend). Non-directional data refers to strike or
lineations expressed as azimuths that can take two angles (i.e. 45 or
225)

## References

Swan, A. R. H. & Sandilands, M. (1995). Introduction to Geological Data
Analysis. Blackwell Science.

## Examples

``` r
x = c(255, 239, 222, 231, 199, 271, 222, 274, 228, 246, 177, 199,
      257, 201, 237, 209, 216, 180, 182, 250, 219, 196, 197, 246,
      218, 235, 232, 243, 232, 180, 231, 254, 242, 149, 212, 210,
      230, 205, 220, 268)
y = c(225, 208, 172, 198, 204, 183, 190, 212, 247, 127, 167, 234,
      217, 192, 212, 171, 169, 210, 245, 222, 185, 227, 193, 178,
      187, 182, 194, 217, 168, 211, 234, 204, 221, 198, 261, 228,
      146, 201, 146, 231)
dir_2sample_test_circ(x, y)
#> $f
#> [1] 12.6
#> 
#> $fcrit
#> [1] 3.96
#> 
#> $df1
#> [1] 1
#> 
#> $df2
#> [1] 78
#> 
#> $p_value
#> [1] 0.000658
#> 
#> $interpretation
#> [1] "Reject H0 and conclude that the two samples could not come from the same population with the same mean direction"
#> 
```
