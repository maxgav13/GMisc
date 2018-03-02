#' @title P-value for proposed difference of the mean between 2 samples
#' @description Calculates the p-value for the proposed difference of the mean between 2 samples (t distribution).
#' @param x1 Sample's 1 mean
#' @param s1 Sample's 1 standard deviation
#' @param n1 Sample size of sample 1
#' @param x2 Sample's 2 mean
#' @param s2 Sample's 2 standard deviation
#' @param n2 Sample size of sample 2
#' @param dmu Proposed difference between sample means (Default is 0)
#' @param side To either get a two-tail or one-tail p-value (Default is "two")
#' @export
#' @return P-value
#' @import stats
#' @examples
#' x1 <- 34.67
#' s1 <- 4.97
#' n1 <- 15
#' x2 <- 40.87
#' s2 <- 5.36
#' n2 <- 15
#' pval_t2(mu1, s1, n1, mu2, s2, n2)
#'
pval_t2 = function(x1, s1, n1, x2, s2, n2, dmu = 0, side = c("two", "one")) {
  dft = n1 + n2 - 2
  t = ((x1 - x2) - dmu) / sqrt(s1^2/n1 + s2^2/n2)
  pval = pt(abs(t), dft, lower.tail = F) * 2
  if (any(side == "two")) {
    pval = (signif(pval, 3))
  } else {
    pval = signif(pval/2, 3)
  }
  return(pval)
}
