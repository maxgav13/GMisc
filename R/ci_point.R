#' @title Confidence interval from point estimate
#' @description Calculates the confidence interval for a population parameter (mean, variance, standard deviation) based on a point estimate.
#' @param x Sample mean
#' @param s Sample standard deviation
#' @param sig Population standard deviation
#' @param n Sample size
#' @param x1 Samples's 1 mean
#' @param s1 Sample's 1 standard deviation
#' @param sig1 Population's 1 standard deviation
#' @param n1 Sample size of sample 1
#' @param x2 Samples's 2 mean
#' @param s2 Sample's 2 standard deviation
#' @param sig2 Population's 2 standard deviation
#' @param n2 Sample size of sample 2
#' @param var.equal Logical indicating if the variances are equal or not (for \code{ci_t2})
#' @param conf.level Confidence level to use for the confidence interval (Default is 0.95)
#' @param digits Number of digits to round to (Default is 2)
#' @details \code{ci_chisq} for variance, \code{ci_F} for ratio of variances, \code{ci_t} for mean with unknown sigma (can work for dependent (paired) samples using the mean and standard deviation of differences) and \code{ci_t2} for the difference of two means with unknown and potentially unequal variances, \code{ci_z} for mean with known sigma, and \code{ci_z2} for the difference of two means with known sigma.
#' @export
#' @return A tibble with the estimate, degrees of freedom, and lower and upper ends of the confidence interval
#' @family statistics
#' @rdname conf_int_point
#' @examples
#' s <- 0.535
#' n <- 10
#' ci_chisq(s, n)
#'
ci_chisq <- function(s, n, conf.level = .95, digits = 2) {
  alpha <- 1 - conf.level
  upper <- ((n - 1) * s^2) / stats::qchisq(alpha / 2, n - 1)
  lower <- ((n - 1) * s^2) / stats::qchisq(1 - alpha / 2, n - 1)
  DF <- tibble::tibble(
    stat = c("sd", "var"),
    value = round(c(s, s^2), digits),
    df = n - 1,
    lower = c(round(sqrt(lower), digits), round(lower, digits)),
    upper = c(round(sqrt(upper), digits), round(upper, digits))
  )
  DF
}

#' @export
#' @rdname conf_int_point
#' @examples
#' s1 <- 3.1
#' n1 <- 15
#' s2 <- 0.8
#' n2 <- 12
#' ci_F(s1, n1, s2, n2)
#'
ci_F <- function(s1, n1, s2, n2, conf.level = .95, digits = 2) {
  alpha <- 1 - conf.level
  upper <- (s1^2 / s2^2) * stats::qf(1 - alpha / 2, n2 - 1, n1 - 1)
  lower <- (s1^2 / s2^2) * (1 / stats::qf(1 - alpha / 2, n1 - 1, n2 - 1))
  DF <- tibble::tibble(
    stat = c("sd", "var"),
    ratio = c(round(s1 / s2, digits), round(s1^2 / s2^2, digits)),
    df1 = n1 - 1,
    df2 = n2 - 1,
    lower = c(round(sqrt(lower), digits), round(lower, digits)),
    upper = c(round(sqrt(upper), digits), round(upper, digits))
  )
  DF
}

#' @export
#' @rdname conf_int_point
#' @examples
#' x <- 80
#' s <- 15
#' n <- 20
#' ci_t(x, s, n)
#'
ci_t <- function(x, s, n, conf.level = .95, digits = 2) {
  alpha <- 1 - conf.level
  stderr <- s / sqrt(n)
  upper <- x + stats::qt(1 - alpha / 2, df = n - 1) * stderr
  lower <- x + stats::qt(alpha / 2, df = n - 1) * stderr
  DF <- tibble::tibble(
    mean = x,
    df = n - 1,
    lower = round(lower, digits),
    upper = round(upper, digits)
  )
  DF
}

#' @export
#' @rdname conf_int_point
#' @examples
#' x1 <- 90
#' s1 <- 15
#' n1 <- 20
#' x2 <- 80
#' s2 <- 8
#' n2 <- 30
#' ci_t2(x1, s1, n1, x2, s2, n2)
#'
ci_t2 <- function(
  x1,
  s1,
  n1,
  x2,
  s2,
  n2,
  var.equal = FALSE,
  conf.level = .95,
  digits = 2
) {
  alpha <- 1 - conf.level
  if (var.equal == T) {
    stderr <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2)) *
      sqrt(1 / n1 + 1 / n2)
    v <- n1 + n2 - 2
  } else {
    stderr <- sqrt(s1^2 / n1 + s2^2 / n2)
    v <- (s1^2 / n1 + s2^2 / n2)^2 /
      (((s1^2 / n1)^2) / (n1 - 1) + ((s2^2 / n2)^2) / (n2 - 1))
  }

  upper <- (x1 - x2) + stats::qt(1 - alpha / 2, df = v) * stderr
  lower <- (x1 - x2) + stats::qt(alpha / 2, df = v) * stderr
  DF <- tibble::tibble(
    mean_diff = round(x1 - x2, digits),
    df = round(v, digits),
    lower = round(lower, digits),
    upper = round(upper, digits)
  )
  DF
}

#' @export
#' @rdname conf_int_point
#' @examples
#' x <- 80
#' sig <- 15
#' n <- 20
#' ci_z(x, sig, n)
#'
ci_z <- function(x, sig, n, conf.level = .95, digits = 2) {
  alpha <- 1 - conf.level
  stderr <- sig / sqrt(n)
  upper <- x + stats::qnorm(1 - alpha / 2, 0, 1) * stderr
  lower <- x + stats::qnorm(alpha / 2, 0, 1) * stderr
  DF <- tibble::tibble(
    mean = x,
    lower = round(lower, digits),
    upper = round(upper, digits)
  )
  DF
}

#' @export
#' @rdname conf_int_point
#' @examples
#' x1 <- 42
#' sig1 <- 8
#' n1 <- 75
#' x2 <- 36
#' sig2 <- 6
#' n2 <- 50
#' ci_z2(x1, sig1, n1, x2, sig2, n2)
#'
ci_z2 <- function(x1, sig1, n1, x2, sig2, n2, conf.level = .95, digits = 2) {
  alpha <- 1 - conf.level
  stderr <- sqrt(sig1^2 / n1 + sig2^2 / n2)
  upper <- (x1 - x2) + stats::qnorm(1 - alpha / 2) * stderr
  lower <- (x1 - x2) + stats::qnorm(alpha / 2) * stderr
  DF <- tibble::tibble(
    mean_diff = round(x1 - x2, digits),
    lower = round(lower, digits),
    upper = round(upper, digits)
  )
  DF
}
