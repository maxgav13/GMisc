#' @title Plots a layered model and summarizes the statistics for each layer
#' @description Given a set of breakpoints, plots a layered model of one variable against distance, plots the confidence intervals for each layer, and gives a summary table.
#' @param x A data frame containing the location variable (depth or distance) in the first column, and the value of interest in the second column
#' @param breaks A vector containg the breakpoints (from 'RI', 'Cohen d' or 'Mahalanobis D2')
#' @export
#' @return A ggplot and plotly objects showing the layered model, another showing the confidence intervals, and a summary table
#' @import stats
#' @import ggplot2
#' @import dplyr
#' @import DescTools
#' @examples
#' layers_window(DPM_data, breaks = c(3.8,8.9))
#' sub = subset(CPTu_data,select = c(depth,qc))
#' layers_window(sub, breaks = c(1.2,3.8,5.1))
#'
layers_window = function(x, breaks) {

  dat = x
  nom = names(dat)

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
