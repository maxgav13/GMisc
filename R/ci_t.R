#' @title Confidence interval for mean of a sample
#' @description Calculates the confidence interval for a population mean with known mean and unknown standard deviation, using the t-statistic (student-t distribution).
#' @param mu A point estimate of the population mean (a vector of values can also be used)
#' @param s A point estimate of the sample standard deviation (a vector of values can also be used)
#' @param n Sample size (a vector of values can also be used)
#' @param conf.level Confidence level to use for the confidence interval (Default is 0.95)
#' @export
#' @return A data frame with the mean, degrees of freedom, and lower and upper ends of the confidence interval
#' @import stats
#' @examples
#' mu <- 80
#' s <- 15
#' n <- 20
#' ci_t(mu, s, n)
#'
ci_t <- function (mu, s, n, conf.level = .95){
  alpha <- 1 - conf.level
  stderr <- s/sqrt(n)
  upper <- mu + qt(1-alpha/2, df = n-1) * stderr
  lower <- mu + qt(alpha/2, df = n-1) * stderr
  DF <- data.frame(mean = mu, df = n-1, lower = round(lower, 2), upper = round(upper, 2))
  DF
}
