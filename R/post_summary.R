#' @title Summary of vectors, matrices or data frames
#' @description Summarises vectors, matrices or data frames by showing their mean, gometric mean, median, standard deviation, median absolute deviation,inter quartile range, and desired quantiles.
#' @param x A vector, matrix or data frame
#' @param probs Desired quantiles to display
#' @param digits Number of significant digits to display
#' @export
#' @return A data frame with summary statistics
#' @import stats
#' @import dplyr
#' @import DescTools
#' @examples
#' x1 <- rnorm(100, 30, 6)
#' x2 <- matrix(x1, nrow = 10)
#' x3 <- data.frame(N = rnorm(100, 30, 6), T = rt(100, 10, 30))
#' post_summary(x1)
#' post_summary(x2)
#' post_summary(x3)
#'
post_summary <- function(x, probs = seq(0,1,.25), digits = 3){
  if (is.data.frame(x) == TRUE | is.matrix(x) == TRUE) {
    x = select_if(x, is.numeric)
    res = rbind.data.frame(mean = apply(x, 2, mean, na.rm = T),
                           Gmean = apply(x, 2, Gmean, na.rm = T),
                           median = apply(x, 2, median, na.rm = T),
                           sd = apply(x, 2, sd, na.rm = T),
                           mad = apply(x, 2, mad, na.rm = T),
                           IQR = apply(x, 2, IQR, na.rm = T),
                           cv = apply(x, 2, CoefVar, na.rm = T),
                           apply(x, 2, quantile, probs = probs, na.rm = T))
  } else if (is.vector(x) == TRUE) {
    res <- c(mean = mean(x, na.rm = T),
             Gmean = apply(x, 2, Gmean, na.rm = T),
             median = median(x, na.rm = T),
             sd = sd(x, na.rm = T),
             mad = mad(x, na.rm = T),
             IQR = IQR(x, na.rm = T),
             cv = CoefVar(x, na.rm = T),
             quantile(x, probs = probs, na.rm = T))
  }
  res = signif(res, digits)
  return(res)
}
