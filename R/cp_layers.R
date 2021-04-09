#' @title Plots a layered model and summarizes the statistics for each layer from a Changepoint result
#' @description Given a number of breakpoints, plots a layered model of one variable against distance, plots the confidence intervals for each layer, and gives a summary table.
#' @param x The resulting object from \code{cp_aic_eta()}
#' @param breaks An integer giving the number of breakpoints to use (from 'Changepoint')
#' @export
#' @return A ggplot and plotly objects showing the layered model, another showing the confidence intervals, and a summary table
#' @import ggplot2
#' @examples
#' cp = cp_aic_eta(DPM_data, m = 10, nl = 3)
#' cp_layers(cp, breaks = 2)
#'
cp_layers = function(x, breaks) {

    datos = x[['Data']]
    datos = x[['Data']] %>% dplyr::select(c(1:2,(breaks+2)))
    nombres = names(datos)
    nom = c(nombres[1:2],'Layers')
    names(datos) = nom

    ydata = datos %>% dplyr::select(-1,-Layers) %>% as.matrix()
    xdata = datos %>% dplyr::select(Layers) %>% as.matrix()
    mod = stats::lm(ydata ~ xdata)

    ES = round(DescTools::EtaSq(mod)[[1]],3)

  q = ggplot(datos, aes_string(nom[2], nom[1], col = nom[3])) +
    geom_path(size = .75) +
    scale_y_reverse() +
    labs(x = nom[2], y = "Depth [m]", col = 'Layers') +
    theme_bw()

  p = plotly::ggplotly(q, dynamicTicks = T)

  q2 = ggplot(datos, aes_string(nom[3], nom[2])) +
    stat_summary(fun.data = mean_cl_normal,
                 geom = "pointrange",
                 color = "red",
                 size=.75) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

  p2 = plotly::ggplotly(q2, dynamicTicks = T)

  Summary = datos %>%
    dplyr::group_by(Layers) %>%
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
    dplyr::mutate(MoE = signif(stats::qt(.975,Obs-1)*SD/sqrt(Obs),3)) %>%
    as.data.frame()

  return(list(LayersGG=q, LayersLY=p, StatsGG=q2, StatsLY=p2, Summary=Summary, ES=ES))
}
