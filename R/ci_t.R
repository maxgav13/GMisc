#' @title Confidence interval for mean (unknown sigma)
#' @description Calculates the confidence interval for the mean with unknown standard deviation, using the t-statistic (t distribution).
#' @param x Sample mean
#' @param s Sample standard deviation
#' @param n Sample size
#' @param conf.level Confidence level to use for the confidence interval (Default is 0.95)
#' @param digits Number of digits to round to (Default is 2)
#' @export
#' @return A tibble with the mean, degrees of freedom, and lower and upper ends of the confidence interval
#' @details It can work for dependent (paired) samples using the mean and standard deviation of differences
#' @examples
#' x <- 80
#' s <- 15
#' n <- 20
#' ci_t(x, s, n)
#'
ci_t <- function (x, s, n, conf.level = .95, digits = 2){
  alpha <- 1 - conf.level
  stderr <- s/sqrt(n)
  upper <- x + stats::qt(1-alpha/2, df = n-1) * stderr
  lower <- x + stats::qt(alpha/2, df = n-1) * stderr
  DF <- tibble::tibble(mean = x,
                       df = n-1,
                       lower = round(lower, digits),
                       upper = round(upper, digits))
  DF
}
