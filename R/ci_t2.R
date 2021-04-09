#' @title Confidence interval for difference of the mean between 2 samples (unknown sigmas)
#' @description Calculates the confidence interval for the difference of the mean between 2 samples (t distribution).
#' @param x1 Sample's 1 mean
#' @param s1 Sample's 1 standard deviation
#' @param n1 Sample size of sample 1
#' @param x2 Sample's 2 mean
#' @param s2 Sample's 2 standard deviation
#' @param n2 Sample size of sample 2
#' @param var.equal Logical indicating if the variances are equal or not
#' @param conf.level Confidence level to use for the confidence interval (Default is 0.95)
#' @param digits Number of digits to round to (Default is 2)
#' @export
#' @return A tibble with the difference of means, degrees of freedom, and lower and upper ends of the confidence interval
#' @examples
#' x1 <- 90
#' s1 <- 15
#' n1 <- 20
#' x2 <- 80
#' s2 <- 8
#' n2 <- 30
#' ci_t2(x1, s1, n1, x2, s2, n2)
#'
ci_t2 <- function (x1, s1, n1, x2, s2, n2, var.equal = FALSE, conf.level = .95, digits = 2){
  alpha <- 1 - conf.level
  if (var.equal == T) {
    stderr <- sqrt(((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2)) * sqrt(1/n1 + 1/n2)
    v <- n1+n2-2
  } else {
    stderr <- sqrt(s1^2/n1 + s2^2/n2)
    v <- (s1^2/n1 + s2^2/n2)^2 / (((s1^2/n1)^2)/(n1-1) + ((s2^2/n2)^2)/(n2-1))
  }

  upper <- (x1-x2) + stats::qt(1-alpha/2, df = v) * stderr
  lower <- (x1-x2) + stats::qt(alpha/2, df = v) * stderr
  DF <- tibble::tibble(mean_diff = round(x1-x2, digits),
                       df = round(v, digits),
                       lower = round(lower, digits),
                       upper = round(upper, digits))
  DF
}
