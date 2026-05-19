#' @title P-value for proposed population parameter
#' @description Calculates the p-value for a proposed parameter.
#' @param s Sample standard deviation
#' @param n Sample size
#' @param sigma Population standard deviation
#' @param s1 Sample's 1 standard deviation
#' @param n1 Sample size of sample 1
#' @param s2 Sample's 2 standard deviation
#' @param n2 Sample size of sample 2
#' @param x Sample mean
#' @param mu Population mean
#' @param x1 Sample's 1 mean
#' @param x2 Sample's 2 mean
#' @param dmu Proposed difference between sample means (Default is 0)
#' @param sig Population standard deviation
#' @param r The coefficient of correlation to test
#' @param side To either get a two-tail or one-tail p-value (Default is "two")
#' @export
#' @return P-value
#' @family statistics
#' @rdname pvalue_point
#' @examples
#' s <- 0.535
#' n <- 10
#' sig <- 1
#' pval_chisq(s, n, sig)
#'
pval_chisq <- function(s, n, sigma, side = c("two", "one")) {
  v = n - 1
  chi = (v * s^2) / sigma^2
  if (chi < v) {
    pval = stats::pchisq(chi, v, lower.tail = T) * 2
  } else {
    pval = stats::pchisq(chi, v, lower.tail = F) * 2
  }
  if (any(side == "two")) {
    pval = signif(pval, 3)
  } else {
    pval = signif(pval / 2, 3)
  }
  return(pval)
}

#' @export
#' @rdname pvalue_point
#' @examples
#' s1 <- 3.1
#' n1 <- 15
#' s2 <- 0.8
#' n2 <- 12
#' pval_F(s1, n1, s2, n2)
#'
pval_F <- function(s1, n1, s2, n2, side = c("two", "one")) {
  df1 = n1 - 1
  df2 = n2 - 1
  f = s1^2 / s2^2
  if (f < 1) {
    pval = stats::pf(f, df1, df2, lower.tail = T) * 2
  } else {
    pval = stats::pf(f, df1, df2, lower.tail = F) * 2
  }
  if (any(side == "two")) {
    pval = (signif(pval, 3))
  } else {
    pval = signif(pval / 2, 3)
  }
  return(pval)
}

#' @export
#' @rdname pvalue_point
#' @examples
#' x <- 6.14
#' s <- 0.803
#' n <- 9
#' mu <- 7
#' pval_t(x, s, n, mu)
#'
pval_t = function(x, s, n, mu, side = c("two", "one")) {
  t = (x - mu) / (s / sqrt(n))
  df = n - 1
  pval = stats::pt(abs(t), df, lower.tail = F) * 2
  if (any(side == "two")) {
    pval = (signif(pval, 3))
  } else {
    pval = signif(pval / 2, 3)
  }
  return(pval)
}

#' @export
#' @rdname pvalue_point
#' @examples
#' x1 <- 34.67
#' s1 <- 4.97
#' n1 <- 15
#' x2 <- 40.87
#' s2 <- 5.36
#' n2 <- 15
#' pval_t2(x1, s1, n1, x2, s2, n2)
#'
pval_t2 = function(x1, s1, n1, x2, s2, n2, dmu = 0, side = c("two", "one")) {
  dft = n1 + n2 - 2
  t = ((x1 - x2) - dmu) / sqrt(s1^2 / n1 + s2^2 / n2)
  pval = stats::pt(abs(t), dft, lower.tail = F) * 2
  if (any(side == "two")) {
    pval = (signif(pval, 3))
  } else {
    pval = signif(pval / 2, 3)
  }
  return(pval)
}

#' @export
#' @rdname pvalue_point
#' @examples
#' x <- 2.6
#' sig <- 0.3
#' n <- 36
#' mu <- 2.5
#' pval_z(x, sig, n, mu)
#'
pval_z = function(x, sig, n, mu = 0, side = c("two", "one")) {
  z = (x - mu) / (sig / sqrt(n))
  pval = stats::pnorm(abs(z), lower.tail = F) * 2
  if (any(side == "two")) {
    pval = (signif(pval, 3))
  } else {
    pval = signif(pval / 2, 3)
  }
  return(pval)
}

#' @export
#' @rdname pvalue_point
#' @examples
#' r <- 0.597
#' n <- 10
#' pval_corr(r, n)
#'
pval_corr = function(r, n, side = c("two", "one")) {
  t = (r * sqrt(n - 2)) / sqrt(1 - r^2)
  df = n - 2
  pval = stats::pt(abs(t), df, lower.tail = F) * 2
  if (any(side == "two")) {
    pval = (signif(pval, 3))
  } else {
    pval = signif(pval / 2, 3)
  }
  return(pval)
}
