#' @title Structural change analysis of different layer models
#' @description Performs a structural change analysis on diferent layer models to find points where a change in the sequence might be occuring.
#' @param x A data frame containing the location variable (depth or distance) in the first column, and the value of interest in the second column
#' @param h Percent of data points per layer
#' @export
#' @return A ggplot and plotly objects showing the BIC statistic
#' @import ggplot2
#' @details The example data given is intended to show the structure needed for input data. The user should follow this structure, which in general corresponds with a data frame with a sequence in the first column and the observed/measured values in the second column
#' @examples
#' sc_bic(DPM_data, h = 0.1)
#'
sc_bic = function(x, h = 0.1) {

  dat = x
  nombres = names(dat)

  dat.ts = zoo::zoo(dat[[2]],dat[[1]])
  bp = strucchange::breakpoints(dat.ts ~ 1, h = h)

  rss = as.data.frame(t(summary(bp)$RSS))
  rss = cbind(Breakpoints=rownames(rss), rss)
  rss$Breakpoints=0:(nrow(rss)-1)

  q = ggplot(rss,aes(.data$Breakpoints,.data$BIC)) +
    geom_line(size = .5, col = 4) +
    geom_point(size = 2, shape = 19, col = 4) +
    labs(x = "Number of breakpoints", y = "BIC") +
    scale_x_continuous(breaks = 0:(nrow(rss)-1)) +
    theme_bw()

  p = plotly::ggplotly(q)

  return(list(GGPLOT=q, PLOTLY=p))
}
