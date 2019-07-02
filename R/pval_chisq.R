#' @title P-value for proposed standard deviation of a population
#' @description Calculates the p-value for a proposed standard deviation of a population (ChiSquare distribution).
#' @param s Sample standard deviation
#' @param n Sample size
#' @param sigma Population standard deviation
#' @param side To either get a two-tail or one-tail p-value (Default is "two")
#' @export
#' @return P-value
#' @import stats
#' @examples
#' s <- 0.535
#' n <- 10
#' sig <- 1
#' pval_chisq(s, n, sigma)
#'
pval_chisq <- function (s, n, sigma, side = c("two", "one")){
  v = n-1
  chi = (v * s^2) / sigma^2
  if (chi < v) {
    pval = pchisq(chi, v, lower.tail = T) * 2
  } else {
    pval = pchisq(chi, v, lower.tail = F) * 2
  }
  if (any(side == "two")) {
    pval = signif(pval, 3)
  } else {
    pval = signif(pval/2, 3)
  }
  return(pval)
}
