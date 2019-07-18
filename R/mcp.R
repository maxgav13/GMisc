#' @title Multiple change-point analysis for layer boundary determination
#' @description Performs multiple change-point analysis in univariate or multivariate data, to find layer boundaries in a perforation log.
#' @param data A data frame containing the depth/distance in the first column, and the variables of interest in the rest of the columns, for a CPTu test: point resistance (qc), sleeve friction (fs), and pore-water pressure (u)
#' @param R The number of random permutations
#' @param alpha A parameter between 0 (exclusive) and 2 (inclusive). lower values make allow for more variation in the search for changepoints
#' @param sig.level Significance level to determine significance of the changepoints
#' @param min.perc Minimum percentage of data points per layer (between changepoints)
#' @export
#' @return ggplot and plotly objects showing the layer distinction, statistical summary of the layers, and a summary table
#' @references Nicholas A. James, David S. Matteson (2014). ecp: An R Package for Nonparametric Multiple Change Point Analysis of Multivariate Data, Journal of Statistical Software, 62(7), 1-25.
#' @import stats
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import forcats
#' @import ecp
#' @import heplots
#' @details The example data given is intended to show the structure needed for input data. The user should follow this structure, which in general corresponds with a data frame with a sequence in the first column and the observed/measured values in the rest of the columns
#' @examples
#' mcp(CPTu_data, R = 199, alpha = 2, sig.level = .01, min.perc = 15) # multivariate example
#' mcp(DPM_data, R = 199, alpha = 2, sig.level = .01, min.perc = 15) # univariate example
#'
mcp = function(data, R = 199, alpha = 2, sig.level = .01, min.perc = 15) {

  dat_ecp0 = data
  dat_ecp = as.matrix(dat_ecp0[-1])
  noms.ecp = names(dat_ecp0)

  min.size = round(min.perc/100 * nrow(dat_ecp0))

  DivOutput <- e.divisive(dat_ecp, R = R, alpha = alpha, sig.lvl = sig.level, min.size = min.size)
  ks = length(DivOutput$estimates)
  brks = dat_ecp0[DivOutput$estimates[c(-1,-ks)],1]
  bounds = cut(dat_ecp0[[1]],
               breaks = c(min(dat_ecp0[[1]]), brks, max(dat_ecp0[[1]])),
               include.lowest = T)
  grps = DivOutput$cluster

  dat_ecp0$Layer = factor(grps)
  dat_ecp0$Bounds = bounds
  data_ecp0_tidy = gather(dat_ecp0, Property, Value, -c(noms.ecp[1],'Layer','Bounds')) %>%
    mutate(Property = as_factor(Property))

  ydata = dat_ecp0 %>% select(-1,-Layer,-Bounds) %>% as.matrix()
  xdata = dat_ecp0 %>% select(Layer) %>% as.matrix()
  mod = lm(ydata ~ xdata)

  ES = round(etasq(mod)[[1]][1],3)

  q = ggplot(data = data_ecp0_tidy, aes_string(x = 'Value', y = noms.ecp[1])) +
    geom_path(aes(group=1,col=Layer),size=.75) +
    scale_y_reverse() +
    facet_grid(~Property,scales = 'free_x') +
    theme_bw() +
    labs(x='', y='Depth [m]', col='Layer')

  p = plotly::ggplotly(q, dynamicTicks = TRUE)

  q2 = ggplot(data_ecp0_tidy, aes_string('Layer', 'Value',col='Layer')) +
    stat_summary(fun.data = mean_cl_normal,
                 geom = "pointrange",
                 #color = "red",
                 size=.75) +
    facet_wrap(~Property,scales = 'free_y') +
    labs(x = 'Layer', y = '', col = 'Layer') +
    theme_bw()

  p2 = plotly::ggplotly(q2)

  Summary = data_ecp0_tidy %>%
    group_by(Layer,Property) %>%
    summarise_at(vars(Value),
                 funs(Obs = n(),
                      Mean = mean(.),
                      SD = sd(.),
                      Min = min(.),
                      Max = max(.),
                      CI.lwr = MeanCI(.)[[2]],
                      CI.upr = MeanCI(.)[[3]]
                 )
    ) %>%
    mutate(MoE = qt(.975,Obs-1)*SD/sqrt(Obs),
           Interval = levels(data_ecp0_tidy$Bounds)[levels(data_ecp0_tidy$Layer) == Layer]) %>%
    as.data.frame()

  return(list(LayersGG=q, LayersLY=p, StatsGG=q2, StatsLY=p2, Summary=Summary, ES=ES))
}
