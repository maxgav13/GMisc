# Splits a vector into two classes

Splits (dichotomizes) a numeric vector into two classes using the
"up-down" or "median" criteria. The results is intendend to be used in a
runs test for tendency.

## Usage

``` r
dichotomy(x, criteria = c("up-down", "median"))
```

## Arguments

- x:

  A numeric vector of values to split

- criteria:

  Splitting criteria to use ("up-down" or "median")

## Value

A vector with the splitted classes

## References

Davis, J. C. (2002). Statistical and Data Analysis in Geology. 3rd ed.
John Wiley & Sons.

## Examples

``` r
x1 = c(17, 14, 13, 17, 9, 15, 36, 13, 26, 10, 25, 13, 25, 8, 28, 15, 19, 32, 22)
x2 = c(17, 14, 14, 17, 9, 15, 36, 13, 26, 10, 25, 13, 25, 8, 28, 15, 19, 32, 22)
dichotomy(x1)
#>  [1] - - + - + + - + - + - + - + - + + -
#> Levels: + -
dichotomy(x2)
#>  [1] - + + - + + - + - + - + - + - + + -
#> Levels: + -
dichotomy(x2, criteria = "median")
#>  [1] + - - + - - + - + - + - + - + - + + +
#> Levels: + -
```
