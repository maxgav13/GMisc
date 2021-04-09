#' @title P-value for coefficient of correlation between two variables
#' @description Calculates the p-value for a given coefficient of correlation between two variables, using the t-statistic (student-t distribution).
#' @param r The coefficient of correlation to test
#' @param n Sample size from the sample
#' @param side To either get a two-tail or one-tail p-value (Default is "two")
#' @export
#' @return P-value
#' @examples
#' r <- 0.597
#' n <- 10
#' pval_corr(r, n)
#'
pval_corr = function(r, n, side = c("two", "one")) {
  t = (r * sqrt(n-2)) / sqrt(1-r^2)
  df = n - 2
  pval = stats::pt(abs(t), df, lower.tail = F) * 2
  if (any(side == "two")) {
    pval = (signif(pval, 3))
  } else {
    pval = signif(pval/2, 3)
  }
  return(pval)
}
