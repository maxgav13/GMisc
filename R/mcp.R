#' @title Multiple change-point analysis for layer boundary determination
#' @description Performs multiple change-point analysis in univariate or multivariate data, to find layer boundaries in a perforation log.
#' @param data A data frame containing the depth/distance in the first column, and the variables of interest in the rest of the columns, for a CPTu test: point resistance (qc), sleeve friction (fs), and pore-water pressure (u)
#' @param R The number of random permutations
#' @param alpha A parameter between 0 (exclusive) and 2 (inclusive). lower values make allow for more variation in the search for changepoints
#' @param sig.level Significance level to determine significance of the changepoints
#' @param min.perc Minimum percentage of data points per layer (between changepoints)
#' @param conf.level Confidence level to use for plot and summary statistics (Default is 0.95)
#' @export
#' @return ggplot and plotly objects showing the layer distinction, statistical summary of the layers, and a summary table
#' @references Nicholas A. James, David S. Matteson (2014). ecp: An R Package for Nonparametric Multiple Change Point Analysis of Multivariate Data, Journal of Statistical Software, 62(7), 1-25.
#' @import ggplot2
#' @details The example data given is intended to show the structure needed for input data. The user should follow this structure, which in general corresponds with a data frame with a sequence in the first column and the observed/measured values in the rest of the columns
#' @examples
#' mcp(CPTu_data, R = 199, alpha = 2, sig.level = .01, min.perc = 15) # multivariate example
#' mcp(DPM_data, R = 199, alpha = 2, sig.level = .01, min.perc = 15) # univariate example
#'
mcp = function(data, R = 199, alpha = 2, sig.level = .01, min.perc = 15, conf.level = 0.95) {

  alfa = 1 - conf.level
  dat_ecp0 = data
  dat_ecp = as.matrix(dat_ecp0[-1])
  noms.ecp = names(dat_ecp0)

  min.size = round(min.perc/100 * nrow(dat_ecp0))

  DivOutput <- ecp::e.divisive(dat_ecp, R = R, alpha = alpha, sig.lvl = sig.level, min.size = min.size)
  ks = length(DivOutput$estimates)
  brks = dat_ecp0[DivOutput$estimates[c(-1,-ks)],1]
  bounds = cut(dat_ecp0[[1]],
               breaks = c(min(dat_ecp0[[1]]), brks, max(dat_ecp0[[1]])),
               include.lowest = T)
  grps = DivOutput$cluster

  dat_ecp0$Layer = factor(grps)
  dat_ecp0$Bounds = bounds
  data_ecp0_tidy = tidyr::pivot_longer(dat_ecp0, cols = -c(noms.ecp[1],'Layer','Bounds'),
                                       names_to = 'Property', values_to = 'Value') %>%
    dplyr::mutate(Property = forcats::as_factor(.data$Property))

  ydata = dat_ecp0 %>% dplyr::select(-1,-.data$Layer,-.data$Bounds) %>% as.matrix()
  xdata = dat_ecp0 %>% dplyr::select(.data$Layer) %>% as.matrix()
  mod = stats::lm(ydata ~ xdata)

  ES = round(heplots::etasq(mod)[[1]][1],3)

  q = ggplot(data = data_ecp0_tidy, aes_string(x = 'Value', y = noms.ecp[1])) +
    geom_path(aes(group=1,col=.data$Layer),size=.5) +
    scale_y_reverse() +
    facet_grid(~.data$Property,scales = 'free_x') +
    theme_bw() +
    labs(x='', y='Depth [m]', col='Layer')

  p = plotly::ggplotly(q, dynamicTicks = TRUE)

  q2 = ggplot(data_ecp0_tidy, aes_string('Layer', 'Value',col='Layer')) +
    stat_summary(fun.data = mean_cl_normal,
                 fun.args = list(conf.int = conf.level),
                 geom = "pointrange",
                 #color = "red",
                 size=.5) +
    facet_wrap(~.data$Property,scales = 'free_y') +
    labs(x = 'Layer', y = '', col = 'Layer') +
    theme_bw()

  p2 = plotly::ggplotly(q2)

  Summary = data_ecp0_tidy %>%
    dplyr::group_by(.data$Layer,.data$Property) %>%
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
    dplyr::mutate(MoE = stats::qt(1-alfa/2,.data$Obs-1)*.data$SD/sqrt(.data$Obs)
                  ,Interval = levels(data_ecp0_tidy$Bounds)[levels(data_ecp0_tidy$Layer) == .data$Layer]
                  ) %>%
    dplyr::relocate(.data$Interval, .after = .data$Layer) %>%
    as.data.frame() %>%
    suppressWarnings()

  return(list(LayersGG=q, LayersLY=p, StatsGG=q2, StatsLY=p2, Summary=Summary, ES=ES))
}
