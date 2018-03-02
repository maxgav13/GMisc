#' @title Confidence interval for mean (known sigma)
#' @description Calculates the confidence interval for the mean with known standard deviation, using the Z-statistic (normal distribution).
#' @param x Sample mean
#' @param sig Population standard deviation
#' @param n Sample size
#' @param conf.level Confidence level to use for the confidence interval (Default is 0.95)
#' @export
#' @return A data frame with the mean, and lower and upper ends of the confidence interval
#' @import stats
#' @examples
#' x <- 80
#' sig <- 15
#' n <- 20
#' ci_z(x, sig, n)
#'
ci_z <- function (x, sig, n, conf.level = .95){
  alpha <- 1 - conf.level
  stderr <- sig/sqrt(n)
  upper <- x + qnorm(1-alpha/2, 0, 1) * stderr
  lower <- x + qnorm(alpha/2, 0, 1) * stderr
  DF <- data.frame(mean = x, lower = round(lower, 2), upper = round(upper, 2))
  DF
}
