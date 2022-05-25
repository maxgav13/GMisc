#' @title Plots a layered model and summarizes the statistics for each layer from a Strucchange result
#' @description Given a number of breakpoints, plots a layered model of one variable against distance, plots the confidence intervals for each layer, and gives a summary table.
#' @param x A data frame containing the location variable (depth or distance) in the first column, and the value of interest in the second column
#' @param h Percent of data points per layer
#' @param breaks An integer giving the number of breakpoints to use (from 'Strucchange')
#' @export
#' @return A ggplot and plotly objects showing the layered model, another showing the confidence intervals, and a summary table
#' @import ggplot2
#' @examples
#' sc_layers(DPM_data, h = 0.1, breaks = 2)
#'
sc_layers = function(x, h = 0.1, breaks) {

  dat = x
  nom = names(dat)

  dat.ts = zoo::zoo(dat[[2]],dat[[1]])
  bp = strucchange::breakpoints(dat.ts ~ 1, h = h)
  breaks = dat[round(strucchange::breakdates(bp, breaks = breaks)*nrow(dat)),1]

  grouping = cut(dat[[nom[1]]],
                 breaks = c(min(dat[1]), breaks, max(dat[1])),
                 include.lowest = T)
  dat$boundaries = grouping

  ydata = dat %>% dplyr::select(-1,-.data$boundaries) %>% as.matrix()
  xdata = dat %>% dplyr::select(.data$boundaries) %>% as.matrix()
  mod = stats::lm(ydata ~ xdata)

  ES = round(DescTools::EtaSq(mod)[[1]],3)

  q = ggplot(dat, aes_string(nom[2], nom[1], col = "boundaries")) +
    geom_path(size = .75) +
    scale_y_reverse() +
    labs(x = nom[2], y = nom[1], col = 'Layers') +
    theme_bw()

  p = plotly::ggplotly(q, dynamicTicks = T)

  q2 = ggplot(dat, aes_string('boundaries', nom[2], col = 'boundaries')) +
    stat_summary(fun.data = mean_cl_normal,
                 geom = "pointrange",
                 color = "red",
                 size=.75) +
    labs(x = 'Layers', y = nom[2], col = '') +
    theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

  p2 = plotly::ggplotly(q2, dynamicTicks = T)

  Summary = dat %>%
    dplyr::group_by(.data$boundaries) %>%
    dplyr::summarise_at(dplyr::vars(nom[2]),
                        .funs = list(
                          Obs = ~ dplyr::n(),
                          Mean = ~ signif(mean(.),3),
                          SD = ~ signif(stats::sd(.),3),
                          Min = ~ signif(min(.),3),
                          Max = ~ signif(max(.),3),
                          CI.lwr = ~ signif(DescTools::MeanCI(.)[[2]],3),
                          CI.upr = ~ signif(DescTools::MeanCI(.)[[3]],3)
                        )
    ) %>%
    dplyr::mutate(MoE = signif(stats::qt(.975,.data$Obs-1)*.data$SD/sqrt(.data$Obs),3)) %>%
    as.data.frame()

  return(list(LayersGG=q, LayersLY=p, StatsGG=q2, StatsLY=p2, Summary=Summary, ES=ES))
}
