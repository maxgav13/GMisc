#' @title P-value for proposed mean of a population
#' @description Calculates the p-value for a proposed mean for a population with known standard deviation, using the Z-statistic (normal distribution).
#' @param x A point estimate of the population mean
#' @param sig A point estimate of the population standard deviation
#' @param n Sample size from the population
#' @param mu Proposed population mean to test for
#' @param side To either get a two-tail or one-tail p-value (Default is "two")
#' @export
#' @return P-value
#' @import stats
#' @examples
#' x <- 2.6
#' sig <- 0.3
#' n <- 36
#' mu <- 2.5
#' pval_z(x, sig, n, mu)
#'
pval_z = function(x, sig, n, mu = 0, side = c("two", "one")) {
  z = (x - mu) / (sig/sqrt(n))
  pval = pnorm(abs(z), lower.tail = F) * 2
  if (any(side == "two")) {
    pval = (signif(pval, 3))
  } else {
    pval = signif(pval/2, 3)
  }
  return(pval)
}
