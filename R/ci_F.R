#' @title Confidence interval for ratio of variances (standard deviations) of 2 samples
#' @description Calculates the confidence interval for the ratio of the variance (standard deviation) of 2 samples (F distribution).
#' @param s1 Sample's 1 standard deviation
#' @param n1 Sample size of sample 1
#' @param s2 Sample's 2 standard deviation
#' @param n2 Sample size of sample 2
#' @param conf.level Confidence level to use for the confidence interval (Default is 0.95)
#' @param digits Number of digits to round to (Default is 2)
#' @export
#' @return A tibble with the ratio of standard deviations and variances, degrees of freedom for both samples, and lower and upper ends of the confidence interval
#' @examples
#' s1 <- 3.1
#' n1 <- 15
#' s2 <- 0.8
#' n2 <- 12
#' ci_F(s1, n1, s2, n2)
#'
ci_F <- function (s1, n1, s2, n2, conf.level = .95, digits =2){
  alpha <- 1 - conf.level
  upper <- (s1^2/s2^2) * stats::qf(1-alpha/2, n2-1, n1-1)
  lower <- (s1^2/s2^2) * (1/stats::qf(1-alpha/2, n1-1, n2-1))
  DF <- tibble::tibble(stat = c("sd", "var"),
                       ratio = c(round(s1/s2, digits),
                                 round(s1^2/s2^2, digits)),
                       df1 = n1-1, df2 = n2-1,
                       lower = c(round(sqrt(lower), digits),
                                 round(lower, digits)),
                       upper = c(round(sqrt(upper), digits),
                                 round(upper, digits)))
  DF
}
