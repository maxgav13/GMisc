#' @title Changepoint analysis of different layer models
#' @description Calculates and plots the AIC and eta-squared statistics for diferent layer models based on a changepoint analysis using the mean and variance.
#' @param x A data frame containing the location variable (depth or distance) in the first column, and the value of interest in the second column
#' @param m The maximum number of breakpoints (# layers - 1) to look for
#' @param nl The minimum number of points per layer to be considered
#' @export
#' @return A ggplot and plotly objects showing the AIC and eta-squared statistics, and a data frame with all the data and possible layer models
#' @import stats
#' @import ggplot2
#' @import changepoint
#' @import sjstats
#' @details The example data given is intended to show the structure needed for input data. The user should follow this structure, which in general corresponds with a data frame with a sequence in the first column and the observed/measured values in the second column
#' @examples
#' cp_aic_eta(DPM_data, m = 10, nl = 3)
#'
cp_aic_eta = function(x, m = 10, nl = 3) {

  datos = x
  nombres = names(datos)

  etas = NULL
  aic = NULL
  for (i in 1:m) {
    cpt = cpt.meanvar(datos[[2]], method = "BinSeg", penalty = "MBIC", Q = i, minseglen = nl)
    breaks = datos[[1]][cpts(cpt)]
    grouping = cut(datos[[1]],
                   breaks = c(min(datos[[1]]), breaks, max(datos[[1]])),
                   include.lowest = T)
    datos[[i+2]] = grouping
    etas[i] = round(eta_sq(aov(datos[[2]] ~ datos[[i+2]]))$etasq,3)
    aic[i] = round(AIC(aov(datos[[2]] ~ datos[[i+2]])),2)
    names(datos)[[i+2]] = paste("B", i, sep = "")
  }
  stats_df = data.frame(breaks = rep(1:m,2), stat = rep(c("eta","AIC"),each = m), stat_value = c(etas,aic))

  q = ggplot(stats_df, aes(breaks, stat_value)) +
    geom_line(size = .5, col = 4) +
    geom_point(size = 2, shape = 19, col = 4) +
    facet_wrap("stat", scales = "free") +
    labs(x = "Number of breakpoints", y = "Statistic Value") +
    scale_x_continuous(breaks = 1:m) +
    theme_bw()

  p = plotly::ggplotly(q)

  return(list(GGPLOT=q, PLOTLY=p, Data=datos))
}
