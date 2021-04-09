#' @title Gini index for a given split of a categorical variable
#' @description Calculates the Gini index of a categorical variable for a given split (threshold) value of a numeric variable.
#' @param x A numeric vector
#' @param group Categorical variable
#' @param thres Threshold value to use for the split
#' @export
#' @return A data frame with Gini index before and after the split, as well as the gain and gain percent
#' @references Kuhn, M. & Johnson, K. (2013). Applied Predictive Modeling. Springer.
#' @references  Witten, I. H., Frank, E. & Hall, M. A. (2011). Data Mining: Practical Machine Learning Tools and Techniques. Elsevier.
#' @examples
#' Gini(mtcars$mpg, mtcars$cyl, 15)
#' Gini(mtcars$mpg, mtcars$cyl, 21)
#' Gini(mtcars$mpg, mtcars$cyl, 25)
#'
Gini = function(x, group, thres) {
  df = data.frame(x, group, right = ifelse(x >= thres, "right", "left"))
  A = as.matrix(table(df$right, df$group))
  B = prop.table(A,1) # row-wise proportions (splits)
  a = colSums(A) # classes totals
  b = rowSums(A) # split totals
  n = sum(A) # total samples
  gini.init = signif(sum(a/n * (1 - a/n)), 3) # initial Gini index
  x = B[1,] # right split
  y = B[2,] # left split
  gini.x = sum(x * (1 - x)) # right split Gini index
  gini.y = sum(y * (1 - y)) # left split Gini index
  gini.split = signif(gini.x * b[1]/n + gini.y * b[2]/n, 3) # Gini index after split
  gini.gain = signif(gini.init - gini.split, 3) # Gini index information gain
  gini.gain.perc = signif(gini.gain / gini.init * 100, 3)
  DF = data.frame(gini.init, gini.split, gini.gain, gini.gain.perc)
  row.names(DF) = ""
  return(DF)
}
