#' @title Plots a layered model and summarizes the statistics for each layer
#' @description Given a set of breakpoints (depths/distances), plots a layered model of the data against distance, plots the confidence intervals for each layer, and gives a summary table.
#' @param x A data frame containing the location variable (depth or distance) in the first column, and the value(s) of interest in the rest of the columns
#' @param breaks A vector containg the breakpoints (from 'RI', 'Cohen d' or 'Mahalanobis D2')
#' @export
#' @return A ggplot and plotly objects showing the layered model, another showing the confidence intervals, and a summary table
#' @import stats
#' @import ggplot2
#' @import dplyr
#' @import forcats
#' @import tidyr
#' @import DescTools
#' @import heplots
#' @examples
#' layers_window(DPM_data, breaks = c(3.8,8.9))
#' layers_window(CPTu_data, breaks = c(1.2,3.8,5.1))
#'
layers_window = function(x, breaks) {

  dat = x
  nom = names(dat)

  grouping = cut(dat[[nom[1]]],
                 breaks = c(min(dat[1]), breaks, max(dat[1])),
                 include.lowest = T)

  dat$boundaries = grouping
  dat_tidy = gather(dat, Property, Value, -c(nom[1],'boundaries')) %>%
    mutate(Property = as_factor(Property))

  ydata = dat %>% select(-1,-boundaries) %>% as.matrix()
  xdata = dat %>% select(boundaries) %>% as.matrix()
  mod = lm(ydata ~ xdata)

  ES = round(etasq(mod)[[1]][1],3)

  q = ggplot(dat_tidy, aes_string('Value', nom[1], col = "boundaries")) +
    geom_path(aes(group=1),size = .75) +
    scale_y_reverse() +
    facet_grid(~Property,scales = 'free_x') +
    labs(x = '', y = nom[1], col = 'Layers') +
    theme_bw()

  p = plotly::ggplotly(q, dynamicTicks = T)

  q2 = ggplot(dat_tidy, aes_string('boundaries', 'Value', col = 'boundaries')) +
    stat_summary(fun.data = mean_cl_normal,
                 geom = "pointrange",
                 # color = "red",
                 size=.75) +
    facet_wrap(~Property,scales = 'free_y') +
    labs(x = 'Layers', y = '', col = 'Layers') +
    theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

  p2 = plotly::ggplotly(q2)

  Summary = dat_tidy %>%
    group_by(boundaries,Property) %>%
    summarise_at(vars(Value),
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

  return(list(LayersGG=q, LayersLY=p, StatsGG=q2, StatsLY=p2, Summary=Summary, ES=ES))
}
