# Gini index for a given split of a categorical variable

Calculates the Gini index of a categorical variable for a given split
(threshold) value of a numeric variable.

## Usage

``` r
Gini(x, group, thres)
```

## Arguments

- x:

  A numeric vector

- group:

  Categorical variable

- thres:

  Threshold value to use for the split

## Value

A data frame with Gini index before and after the split, as well as the
gain and gain percent

## References

Kuhn, M. & Johnson, K. (2013). Applied Predictive Modeling. Springer.

Witten, I. H., Frank, E. & Hall, M. A. (2011). Data Mining: Practical
Machine Learning Tools and Techniques. Elsevier.

## Examples

``` r
Gini(mtcars$mpg, mtcars$cyl, 15)
#>  gini.init gini.split gini.gain gini.gain.perc
#>      0.643      0.553      0.09             14
Gini(mtcars$mpg, mtcars$cyl, 21)
#>  gini.init gini.split gini.gain gini.gain.perc
#>      0.643      0.342     0.301           46.8
Gini(mtcars$mpg, mtcars$cyl, 25)
#>  gini.init gini.split gini.gain gini.gain.perc
#>      0.643      0.488     0.155           24.1
```
