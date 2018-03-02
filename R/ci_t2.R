#' @title Confidence interval for difference of the mean between 2 samples
#' @description Calculates the confidence interval for the difference of the mean between 2 samples (t distribution).
#' @param x1 Sample's 1 mean
#' @param s1 Sample's 1 standard deviation
#' @param n1 Sample size of sample 1
#' @param x2 Sample's 2 mean
#' @param s2 Sample's 2 standard deviation
#' @param n2 Sample size of sample 2
#' @param conf.level Confidence level to use for the confidence interval (Default is 0.95)
#' @export
#' @return A data frame with the difference of means, degrees of freedom, and lower and upper ends of the confidence interval
#' @import stats
#' @examples
#' x1 <- 90
#' s1 <- 15
#' n1 <- 20
#' x2 <- 80
#' s2 <- 8
#' n2 <- 30
#' ci_t2(x1, s1, n1, x2, s2, n2)
#'
ci_t2 <- function (x1, s1, n1, x2, s2, n2, conf.level = .95){
  alpha <- 1 - conf.level
  stderr <- sqrt(s1^2/n1 + s2^2/n2)
  upper <- (x1-x2) + qt(1-alpha/2, df = n1+n2-2) * stderr
  lower <- (x1-x2) + qt(alpha/2, df = n1+n2-2) * stderr
  DF <- data.frame(mean_diff = round(x1-x2, 2), df = n1+n2-2,
                   lower = round(lower, 2), upper = round(upper, 2))
  DF
}
