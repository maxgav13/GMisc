#' @title Moving (weighted) average filter plot (filtered)
#' @description Plots the filtered results of a moving (weighted) average filter routine.
#' @param x The result of using the \code{moving_avg_filter()} or \code{moving_wt_filter()} functions
#' @param xlab The label for the x-axis
#' @param ylab The label for the y-axis
#' @param filterlab The label for the legend
#' @param plotk A string for choosing what to plot (Default is "all")
#' @export
#' @return ggplot and plotly objects with the filtered data and original data
#' @import ggplot2
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
    df = x$Filtered
  } else {
    zz = plotk
    df = x$Filtered %>% dplyr::select(c("x",zz))
  }

  df = tidyr::pivot_longer(df, cols = -x, names_to = 'filtro', values_to = 'y_val')
  df$filtro = factor(df$filtro, levels = unique(df$filtro))

  f = ggplot(df, aes(x = x, y = .data$y_val,
                     group = .data$filtro, col = .data$filtro)) +
    geom_line(size = 0.5) +
    labs(x = xlab, y = ylab, col = filterlab) +
    theme_bw()

  p = plotly::ggplotly(f, dynamicTicks = T)

  return(list(GGPLOT=f, PLOTLY=p))
}
