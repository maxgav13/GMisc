#' @title Confidence interval for standard deviation of a sample
#' @description Calculates the confidence interval for the standard deviation of a sample (ChiSquare distribution).
#' @param s A point estimate of the sample standard deviation (a vector of values can also be used)
#' @param n Sample size (a vector of values can also be used)
#' @param conf.level Confidence level to use for the confidence interval (Default is 0.95)
#' @export
#' @return A data frame with the standard deviation and variance, degrees of freedom, and lower and upper ends of the confidence interval
#' @import stats
#' @examples
#' s <- 0.535
#' n <- 10
#' ci_chisq(s, n)
#'
ci_chisq <- function (s, n, conf.level = .95){
  alpha <- 1 - conf.level
  upper <- ((n-1) * s^2)/qchisq(alpha/2, n-1)
  lower <- ((n-1) * s^2)/qchisq(1-alpha, n-1)
  DF <- data.frame(stat = c("sd", "var"), value = round(c(s, s^2), 2) , df = n-1,
                   lower = c(round(sqrt(lower), 2), round(lower, 2)),
                   upper = c(round(sqrt(upper), 2), round(upper, 2)))
  DF
}
