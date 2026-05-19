# Entropy for a given split of a categorical variable

Calculates the entropy of a categorical variable for a given split
(threshold) value of a numeric variable.

## Usage

``` r
entropy(x, group, thres)
```

## Arguments

- x:

  A numeric vector

- group:

  Categorical variable

- thres:

  Threshold value to use for the split

## Value

A data frame with entropy before and after the split, as well as the
gain and gain percent

## References

Kuhn, M. & Johnson, K. (2013). Applied Predictive Modeling. Springer.

Witten, I. H., Frank, E. & Hall, M. A. (2011). Data Mining: Practical
Machine Learning Tools and Techniques. Elsevier.

## Examples

``` r
entropy(mtcars$mpg, mtcars$cyl, 15)
#>  ent.init ent.split ent.gain ent.gain.perc
#>      1.53      1.32     0.21          13.7
entropy(mtcars$mpg, mtcars$cyl, 21)
#>  ent.init ent.split ent.gain ent.gain.perc
#>      1.53     0.758    0.772          50.5
entropy(mtcars$mpg, mtcars$cyl, 25)
#>  ent.init ent.split ent.gain ent.gain.perc
#>      1.53      1.18     0.35          22.9
```
