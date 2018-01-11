#' @title P-value for proposed difference of the mean between 2 paired samples
#' @description Calculates the p-value for the proposed difference of the mean between 2 paired samples (student-t distribution).
#' @param x A point estimate of the mean of the difference between observed values
#' @param s A point estimate of the standar deviaton of the difference between observed values
#' @param n Sample size from the population
#' @param dmu Proposed difference between sample means (Default is 0)
#' @param side To either get a two-tail or one-tail p-value (Default is "two")
#' @export
#' @return P-value
#' @import stats
#' @examples
#' x <- 0.1
#' s <- 0.125
#' n <- 10
#' pval_t2_pair(x, s, n)
#'
pval_t2_pair = function(x, s, n, dmu = 0, side = c("two", "one")) {
  df = n - 1
  t = (x - dmu) / (s/sqrt(n))
  pval = pt(abs(t), df, lower.tail = F) * 2
  if (any(side == "two")) {
    pval = (signif(pval, 3))
  } else {
    pval = signif(pval/2, 3)
  }
  return(pval)
}
