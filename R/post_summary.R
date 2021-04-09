#' @title Summary of vectors, matrices or data frames
#' @description Summarises vectors or data frames by showing their mean, median, standard deviation, median absolute deviation,inter quartile range, coefficient of variation, and desired quantiles.
#' @param x A vector or data frame
#' @param probs Desired quantiles to display
#' @export
#' @return A tibble with summary statistics
#' @examples
#' x1 <- rnorm(100, 30, 6)
#' x2 <- data.frame(N = rnorm(100, 30, 6), T = rt(100, 10, 30))
#' post_summary(x1)
#' post_summary(x2)
#'
post_summary <- function(x, probs = c(.025,.5,.975)){
  if (is.data.frame(x) == TRUE) {
    x = dplyr::select_if(x, is.numeric)
    res = rbind.data.frame(mean = apply(x, 2, mean, na.rm = T),
                           median = apply(x, 2, stats::median, na.rm = T),
                           sd = apply(x, 2, stats::sd, na.rm = T),
                           mad = apply(x, 2, stats::mad, na.rm = T),
                           IQR = apply(x, 2, stats::IQR, na.rm = T),
                           cv = apply(x, 2, DescTools::CoefVar, na.rm = T),
                           apply(x, 2, stats::quantile, probs = probs, na.rm = T))  %>%
      tibble::rownames_to_column('stat') %>%
      tibble::as_tibble()
  } else if (is.vector(x) == TRUE) {
    res <- c(mean = mean(x, na.rm = T),
             median = stats::median(x, na.rm = T),
             sd = stats::sd(x, na.rm = T),
             mad = stats::mad(x, na.rm = T),
             IQR = stats::IQR(x, na.rm = T),
             cv = DescTools::CoefVar(x, na.rm = T),
             stats::quantile(x, probs = probs, na.rm = T)) %>%
      tibble::enframe(name = 'stat')
  }

  return(res)
}
