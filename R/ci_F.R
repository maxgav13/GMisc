#' @title Confidence interval for ratio of standard deviations of 2 samples
#' @description Calculates the confidence interval for the ratio of the standard deviation of 2 sample (F distribution).
#' @param s1 A point estimate of sample's 1 standard deviation
#' @param n1 Sample size of sample 1
#' @param s2 A point estimate of sample's 2 standard deviation
#' @param n2 Sample size of sample 2
#' @param conf.level Confidence level to use for the confidence interval (Default is 0.98, and commonly another one is 0.90)
#' @export
#' @return A data frame with the ratio of standard deviations and variances, degrees of freedom for both samples, and lower and upper ends of the confidence interval
#' @import stats
#' @examples
#' s1 <- 3.1
#' n1 <- 15
#' s2 <- 0.8
#' n2 <- 12
#' ci_F(s1, n1, s2, n2)
#'
ci_F <- function (s1, n1, s2, n2, conf.level = .98){
  alpha <- 1 - conf.level
  upper <- (s1^2/s2^2) * qf(1-alpha/2, n2-1, n1-1)
  lower <- (s1^2/s2^2) * (1/qf(1-alpha/2, n1-1, n2-1))
  DF <- data.frame(stat = c("sd", "var"),
                   ratio = c(round(s1/s2, 2), round(s1^2/s2^2, 2)),
                   df1 = n1-1, df2 = n2-1,
                   lower = c(round(sqrt(lower), 2), round(lower, 2)),
                   upper = c(round(sqrt(upper), 2), round(upper, 2)))
  DF
}
