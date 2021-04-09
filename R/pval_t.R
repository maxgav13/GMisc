#' @title P-value for proposed mean of a population (unknown sigma)
#' @description Calculates the p-value for a proposed mean for a population with unknown standard deviation, using the t-statistic (t distribution).
#' @param x Sample mean
#' @param s Sample standard deviation
#' @param n Sample size
#' @param mu Population mean
#' @param side To either get a two-tail or one-tail p-value (Default is "two")
#' @export
#' @return P-value
#' @examples
#' x <- 6.14
#' s <- 0.803
#' n <- 9
#' mu <- 7
#' pval_t(x, s, n, mu)
#'
pval_t = function(x, s, n, mu, side = c("two", "one")) {
  t = (x - mu) / (s/sqrt(n))
  df = n - 1
  pval = stats::pt(abs(t), df, lower.tail = F) * 2
  if (any(side == "two")) {
    pval = (signif(pval, 3))
  } else {
    pval = signif(pval/2, 3)
  }
  return(pval)
}
