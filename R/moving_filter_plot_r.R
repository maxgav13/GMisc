#' @title Moving (weighted) average filter plot (residual)
#' @description Plots the residual results of a moving (weighted) average filter routine.
#' @param x The result of using the \code{moving_avg_filter()} or \code{moving_wt_filter()} functions
#' @param xlab The label for the x-axis
#' @param ylab The label for the y-axis
#' @param filterlab The label for the legend
#' @param plotk A string for choosing what to plot (Default is "all")
#' @export
#' @return ggplot and plotly objects with the residual data
#' @import stats
#' @import ggplot2
#' @import tidyr
#' @examples
#' data(nautilus)
#' k = c(3, 5, 7, 9)
#' filt = moving_avg_filter(nautilus$x, nautilus$y, k)
#' moving_filter_plot_r(filt)
#' moving_filter_plot_r(filt, plotk = c("k_3", "k_9")) # plots residuals for filters of window 3 and 9
#'
moving_filter_plot_r = function(x, xlab = "X", ylab = "Residual data",filterlab = "Filter", plotk = "all") {

  if (any(plotk == "all")) {
    df = x$Residual %>% select_at(names(.))
  } else {
    zz = plotk
    df = x$Residual %>% select_at(c("x",zz))
  }

  df = df %>% gather(filtro, y_val, -x)
  df$filtro = factor(df$filtro, levels = unique(df$filtro))

  r = ggplot(df, aes(x = x, y = y_val, group = filtro, col = filtro)) +
    geom_line(size = 0.5) +
    labs(x = xlab, y = ylab, col = filterlab) +
    theme_bw()

  p = plotly::ggplotly(r, dynamicTicks = T)

  return(list(GGPLOT=r, PLOTLY=p))
}
