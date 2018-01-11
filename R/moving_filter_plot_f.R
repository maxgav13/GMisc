#' @title Moving (weighted) average filter plot (filtered)
#' @description Plots the filtered results of a moving (weighted) average filter routine.
#' @param x The result of using the \code{moving_avg_filter()} or \code{moving_wt_filter()} functions
#' @param xlab The label for the x-axis
#' @param ylab The label for the y-axis
#' @param filterlab The label for the legend
#' @param plotk A string for choosing what to plot (Default is "all")
#' @export
#' @return A ggplot object with the filtered data and original data
#' @import stats
#' @import ggplot2
#' @import tidyr
#' @examples
#' data(nautilus)
#' k = c(3, 5, 7, 9)
#' filt = moving_avg_filter(nautilus$x, nautilus$y, k)
#' moving_filter_plot_f(filt)
#' moving_filter_plot_f(filt, plotk = c("y", "k_9")) # plots data and filter of window 9
#' moving_filter_plot_f(filt, plotk = c("k_3", "k_9")) # plots filters of window 3 and 9
#'
moving_filter_plot_f = function(x, xlab = "X", ylab = "Data", filterlab = "Filter & data", plotk = "all") {

  if (any(plotk == "all")) {
    df = x$Filtered %>% select_at(names(.))
  } else {
    zz = plotk
    df = x$Filtered %>% select_at(c("x",zz))
  }

  f = df %>% gather(filtro, y_val, -x) %>%
    ggplot(aes(x = x, y = y_val, group = filtro, col = filtro)) +
    geom_line(size = 0.5) +
    labs(x = xlab, y = ylab, col = filterlab) +
    theme_bw()
  return(f)
}
