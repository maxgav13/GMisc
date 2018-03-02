#' @title P-value for ratio of standard deviations of 2 samples
#' @description Calculates the p-value for the ratio of the standard deviation of 2 samples (F distribution).
#' @param s1 Sample's 1 standard deviation
#' @param n1 Sample size of sample 1
#' @param s2 Sample's 2 standard deviation
#' @param n2 Sample size of sample 2
#' @param side To either get a two-tail or one-tail p-value (Default is "two")
#' @export
#' @return P-value
#' @import stats
#' @examples
#' s1 <- 3.1
#' n1 <- 15
#' s2 <- 0.8
#' n2 <- 12
#' pval_F(s1, n1, s2, n2)
#'
pval_F <- function (s1, n1, s2, n2, side = c("two", "one")) {
  df1 = n1-1
  df2 = n2-1
  f = s1^2/s2^2
  if (f < 1) {
    pval = pf(f, df1, df2, lower.tail = T) * 2
  } else {
    pval = pf(f, df1, df2, lower.tail = F) * 2
  }
  if (any(side == "two")) {
    pval = (signif(pval, 3))
  } else {
    pval = signif(pval/2, 3)
  }
  return(pval)
}
