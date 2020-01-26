#' @title Summary of vectors, matrices or data frames
#' @description Summarises vectors or data frames by showing their mean, median, standard deviation, median absolute deviation,inter quartile range, coefficient of variation, and desired quantiles.
#' @param x A vector or data frame
#' @param probs Desired quantiles to display
#' @export
#' @return A tibble with summary statistics
#' @import stats
#' @import dplyr
#' @import tidyr
#' @import tibble
#' @import DescTools
#' @examples
#' x1 <- rnorm(100, 30, 6)
#' x2 <- data.frame(N = rnorm(100, 30, 6), T = rt(100, 10, 30))
#' post_summary(x1)
#' post_summary(x2)
#'
post_summary <- function(x, probs = c(.025,.5,.975)){
  if (is.data.frame(x) == TRUE) {
    x = select_if(x, is.numeric)
    res = rbind.data.frame(mean = apply(x, 2, mean, na.rm = T),
                           median = apply(x, 2, median, na.rm = T),
                           sd = apply(x, 2, sd, na.rm = T),
                           mad = apply(x, 2, mad, na.rm = T),
                           IQR = apply(x, 2, IQR, na.rm = T),
                           cv = apply(x, 2, CoefVar, na.rm = T),
                           apply(x, 2, quantile, probs = probs, na.rm = T))  %>%
      rownames_to_column('stat') %>%
      as_tibble()
  } else if (is.vector(x) == TRUE) {
    res <- c(mean = mean(x, na.rm = T),
             median = median(x, na.rm = T),
             sd = sd(x, na.rm = T),
             mad = mad(x, na.rm = T),
             IQR = IQR(x, na.rm = T),
             cv = CoefVar(x, na.rm = T),
             quantile(x, probs = probs, na.rm = T)) %>%
      enframe(name = 'stat')
  }

  return(res)
}
