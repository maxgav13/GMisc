#' @title Confidence interval for difference of the mean between 2 samples (known sigmas)
#' @description Calculates the confidence interval for the difference of the mean between 2 samples (Z distribution).
#' @param x1 Sample's 1 mean
#' @param sig1 Population's 1 standard deviation
#' @param n1 Sample size of sample 1
#' @param x2 Sample's 2 mean
#' @param sig2 Population's 2 standard deviation
#' @param n2 Sample size of sample 2
#' @param conf.level Confidence level to use for the confidence interval (Default is 0.95)
#' @param digits Number of digits to round to (Default is 2)
#' @export
#' @return A data frame with the difference of means, and lower and upper ends of the confidence interval
#' @import stats
#' @examples
#' x1 <- 42
#' sig1 <- 8
#' n1 <- 75
#' x2 <- 36
#' sig2 <- 6
#' n2 <- 50
#' ci_z2(x1, sig1, n1, x2, sig2, n2)
#'
ci_z2 <- function (x1, sig1, n1, x2, sig2, n2, conf.level = .95, digits = 2){
  alpha <- 1 - conf.level
  stderr <- sqrt(sig1^2/n1 + sig2^2/n2)
  upper <- (x1-x2) + qnorm(1-alpha/2) * stderr
  lower <- (x1-x2) + qnorm(alpha/2) * stderr
  DF <- data.frame(mean_diff = round(x1-x2, digits),
                   lower = round(lower, digits), upper = round(upper, digits))
  DF
}
