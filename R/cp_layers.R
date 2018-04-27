#' @title Plots a layered model and summarizes the statistics for each layer from a Changepoint result
#' @description Given a number of breakpoints, plots a layered model of one variable against distance, plots the confidence intervals for each layer, and gives a summary table.
#' @param x The resulting objecto from \code{cp_aic_eta()}
#' @param breaks An integer giving the number of breakpoints to use (from 'Changepoint')
#' @export
#' @return A ggplot and plotly objects showing the layered model, another showing the confidence intervals, and a summary table
#' @import stats
#' @import ggplot2
#' @import dplyr
#' @import DescTools
#' @examples
#' cp = cp_aic_eta(DPM_data, m = 10, nl = 3)
#' cp_layers(cp, breaks = 2)
#'
cp_layers = function(x, breaks) {

    datos = x[['Data']]
    datos = x[['Data']] %>% select(c(1:2,(breaks+2)))
    nombres = names(datos)
    nom = c(nombres[1:2],'Layers')
    names(datos) = nom

  q = ggplot(datos, aes_string(nom[2], nom[1], col = nom[3])) +
    geom_path(size = .75) +
    scale_y_reverse() +
    labs(x = nom[2], y = "Depth [m]", col = 'Layers') +
    theme_bw()

  p = plotly::ggplotly(q)

  q2 = ggplot(datos, aes_string(nom[3], nom[2])) +
    stat_summary(fun.data = mean_cl_normal,
                 geom = "pointrange",
                 color = "red",
                 size=.75) +
    theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

  p2 = plotly::ggplotly(q2)

  Summary = datos %>%
    group_by(Layers) %>%
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
