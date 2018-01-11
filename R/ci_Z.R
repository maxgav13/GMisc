#' @title Confidence interval for mean of a population
#' @description Calculates the confidence interval for a population mean with known mean and standard deviation, using the Z-statistic (normal distribution).
#' @param mu A point estimate of the population mean (a vector of values can also be used)
#' @param sig A point estimate of the population standard deviation (a vector of values can also be used)
#' @param n Sample size from the population (a vector of values can also be used)
#' @param conf.level Confidence level to use for the confidence interval (Default is 0.95)
#' @export
#' @return A data frame with the mean, and lower and upper ends of the confidence interval
#' @import stats
#' @examples
#' mu <- 80
#' sig <- 15
#' n <- 20
#' ci_Z(mu, sig, n)
#'
ci_Z <- function (mu, sig, n, conf.level = .95){
  alpha <- 1 - conf.level
  stderr <- sig/sqrt(n)
  upper <- mu + qnorm(1-alpha/2, 0, 1) * stderr
  lower <- mu + qnorm(alpha/2, 0, 1) * stderr
  DF <- data.frame(mean = mu, lower = round(lower, 2), upper = round(upper, 2))
  DF
}
