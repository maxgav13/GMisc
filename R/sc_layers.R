#' @title Plots a layered model and summarizes the statistics for each layer from a Strucchange result
#' @description Given a number of breakpoints, plots a layered model of one variable against distance, plots the confidence intervals for each layer, and gives a summary table.
#' @param x A data frame containing the location variable (depth or distance) in the first column, and the value of interest in the second column
#' @param h Percent of data points per layer
#' @param breaks An integer giving the number of breakpoints to use (from 'Strucchange')
#' @export
#' @return A ggplot and plotly objects showing the layered model, another showing the confidence intervals, and a summary table
#' @import stats
#' @import ggplot2
#' @import strucchange
#' @import dplyr
#' @import DescTools
#' @examples
#' sc_layers(DPM_data, h = 0.1, breaks = 2)
#'
sc_layers = function(x, h = 0.1, breaks) {

  dat = x
  nom = names(dat)

  dat.ts = zoo(dat[[2]],dat[[1]])
  bp = breakpoints(dat.ts ~ 1, h = h)
  breaks = dat[round(breakdates(bp, breaks = breaks)*nrow(dat)),1]

  grouping = cut(dat[[nom[1]]],
                 breaks = c(min(dat[1]), breaks, max(dat[1])),
                 include.lowest = T)
  dat$boundaries = grouping

  q = ggplot(dat, aes_string(nom[2], nom[1], col = "boundaries")) +
    geom_path(size = .75) +
    scale_y_reverse() +
    labs(x = nom[2], y = nom[1], col = 'Layers') +
    theme_bw()

  p = plotly::ggplotly(q)

  q2 = ggplot(dat, aes_string('boundaries', nom[2], col = 'boundaries')) +
    stat_summary(fun.data = mean_cl_normal,
                 geom = "pointrange",
                 color = "red",
                 size=.75) +
    labs(x = 'Layers', y = nom[2], col = '') +
    theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

  p2 = plotly::ggplotly(q2)

  Summary = dat %>%
    group_by(boundaries) %>%
    summarise_at(vars(nom[2]),
                 funs(Obs = n(),
                      Mean = signif(mean(.),3),
                      SD = signif(sd(.),3),
                      Min = signif(min(.),3),
                      Max = signif(max(.),3),
                      CI.lwr = signif(MeanCI(.)[[2]],3),
                      CI.upr = signif(MeanCI(.)[[3]],3)
                 )
    ) %>%
    mutate(MoE = signif(qt(.975,Obs-1)*SD/sqrt(Obs),3)) %>%
    as.data.frame()

  return(list(LayersGG=q, LayersLY=p, StatsGG=q2, StatsLY=p2, Summary=Summary))
}
