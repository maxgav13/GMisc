#' @title Plots a layered model and summarizes the statistics for each layer
#' @description Given a set of breakpoints (depths/distances), plots a layered model of the data against distance, plots the confidence intervals for each layer, and gives a summary table.
#' @param x A data frame containing the location variable (depth or distance) in the first column, and the value(s) of interest in the rest of the columns
#' @param breaks A vector containing the breakpoints (from 'RI', 'Cohen d' or 'Mahalanobis D2')
#' @param conf.level Confidence level to use for plot and summary statistics (Default is 0.95)
#' @export
#' @return A ggplot and plotly objects showing the layered model, another showing the confidence intervals, and a summary table
#' @import ggplot2
#' @examples
#' layers_window(DPM_data, breaks = c(3.8,8.9))
#' layers_window(CPTu_data, breaks = c(1.2,3.8,5.1))
#'
layers_window = function(x, breaks, conf.level = 0.95) {

  alfa = 1 - conf.level
  dat = x
  nom = names(dat)

  grouping = cut(dat[[nom[1]]],
                 breaks = c(min(dat[1]), breaks, max(dat[1])),
                 include.lowest = T)

  dat$boundaries = grouping
  dat_tidy = tidyr::pivot_longer(dat, cols = -c(nom[1],'boundaries'),
                                 names_to = 'Property', values_to = 'Value') %>%
    dplyr::mutate(Property = forcats::as_factor(.data$Property))

  ydata = dat %>% dplyr::select(-1,-.data$boundaries) %>% as.matrix()
  xdata = dat %>% dplyr::select(.data$boundaries) %>% as.matrix()
  mod = stats::lm(ydata ~ xdata)

  ES = round(heplots::etasq(mod)[[1]][1],3)

  q = ggplot(dat_tidy, aes_string('Value', nom[1], col = "boundaries")) +
    geom_path(aes(group=1),size = .5) +
    scale_y_reverse() +
    facet_grid(~.data$Property,scales = 'free_x') +
    labs(x = '', y = nom[1], col = 'Layers') +
    theme_bw()

  p = plotly::ggplotly(q, dynamicTicks = T)

  q2 = ggplot(dat_tidy, aes(.data$Value, forcats::fct_rev(.data$boundaries),
                            col = .data$boundaries)) +
    stat_summary(fun.data = mean_cl_normal,
                 fun.args = list(conf.int = conf.level),
                 geom = "pointrange",
                 # color = "red",
                 size=.5,
                 fatten=1) +
    facet_wrap(~Property,scales = 'free_x') +
    labs(y = 'Layers', x = '', col = 'Layers') +
    theme_bw()
    # theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

  p2 = plotly::ggplotly(q2)

  Summary = dat_tidy %>%
    dplyr::group_by(.data$boundaries,.data$Property) %>%
    dplyr::summarise(dplyr::across(.data$Value,
                                   .fns = list(
                                     Obs = ~ dplyr::n(),
                                     Mean = ~ signif(mean(.),3),
                                     SD = ~ signif(stats::sd(.),3),
                                     Min = ~ signif(min(.),3),
                                     Max = ~ signif(max(.),3),
                                     CI.lwr = ~ signif(DescTools::MeanCI(., conf.level = conf.level)[[2]],3),
                                     CI.upr = ~ signif(DescTools::MeanCI(., conf.level = conf.level)[[3]],3)
                                   )
                                   ,.names = '{.fn}')) %>%
    dplyr::mutate(MoE = signif(stats::qt(1-alfa/2,.data$Obs-1)*.data$SD/sqrt(.data$Obs),3)) %>%
    as.data.frame()

  return(list(LayersGG=q, LayersLY=p, StatsGG=q2, StatsLY=p2, Summary=Summary, ES=ES))
}
