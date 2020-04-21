#' @title Confidence interval for mean (unknown sigma)
#' @description Calculates the confidence interval for the mean with unknown standard deviation, using the t-statistic (t distribution).
#' @param x Sample mean
#' @param s Sample standard deviation
#' @param n Sample size
#' @param conf.level Confidence level to use for the confidence interval (Default is 0.95)
#' @export
#' @return A data frame with the mean, degrees of freedom, and lower and upper ends of the confidence interval
#' @details It can work for dependent (paired) samples using the mean and standard deviation of differences
#' @import stats
#' @examples
#' x <- 80
#' s <- 15
#' n <- 20
#' ci_t(x, s, n)
#'
ci_t <- function (x, s, n, conf.level = .95){
  alpha <- 1 - conf.level
  stderr <- s/sqrt(n)
  upper <- x + qt(1-alpha/2, df = n-1) * stderr
  lower <- x + qt(alpha/2, df = n-1) * stderr
  DF <- data.frame(mean = x, df = n-1, lower = round(lower, 2), upper = round(upper, 2))
  DF
}