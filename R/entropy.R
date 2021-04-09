#' @title Entropy for a given split of a categorical variable
#' @description Calculates the entropy of a categorical variable for a given split (threshold) value of a numeric variable.
#' @param x A numeric vector
#' @param group Categorical variable
#' @param thres Threshold value to use for the split
#' @export
#' @return A data frame with entropy before and after the split, as well as the gain and gain percent
#' @references Kuhn, M. & Johnson, K. (2013). Applied Predictive Modeling. Springer.
#' @references Witten, I. H., Frank, E. & Hall, M. A. (2011). Data Mining: Practical Machine Learning Tools and Techniques. Elsevier.
#' @examples
#' entropy(mtcars$mpg, mtcars$cyl, 15)
#' entropy(mtcars$mpg, mtcars$cyl, 21)
#' entropy(mtcars$mpg, mtcars$cyl, 25)
#'
entropy = function(x, group, thres) {
  df = data.frame(x, group, right = ifelse(x >= thres, "right", "left"))
  A = as.matrix(table(df$right, df$group))
  B = prop.table(A,1) # row-wise proportions (splits)
  a = colSums(A) # classes totals
  b = rowSums(A) # split totals
  n = sum(A) # total samples
  ent.init = signif(-sum(a/n * log2(a/n)), 3) # initial entropy (information)
  x = B[1,] # right split
  y = B[2,] # left split
  ent.x = -sum(x * log2(x), na.rm = T) # right split entropy (information)
  ent.y = -sum(y * log2(y), na.rm = T) # left split entropy (information)
  ent.split = signif(ent.x * b[1]/n + ent.y * b[2]/n, 3) # entropy after split
  ent.gain = signif(ent.init - ent.split, 3) # information gain
  ent.gain.perc = signif(ent.gain / ent.init * 100, 3)
  DF = data.frame(ent.init, ent.split, ent.gain, ent.gain.perc)
  row.names(DF) = ""
  return(DF)
}
