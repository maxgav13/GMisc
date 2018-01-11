#' @title Plots the results of an RI or T_stat object
#' @description Plots the results of an intraclass correlation coefficient (RI) object or T_stat oject, to easily visualize the perforation log and the suggested layer boundaries.
#' @param x A list containing the results of the \code{RI()} or \code{T_stat()} functions
#' @param ylab The label to use for the y-axis
#' @export
#' @return A ggplot object based on the results from an RI object
#' @import ggplot2
#' @references Mora, R. (2013). Uso de metodos estadisticos para la determinacion de capas homogeneas de suelos volcanicos en un sitio de las laderas del Volcan Irazu, Cartago, Costa Rica. - Rev. Geol. Amer. Central, 49: 101-108.
#' @references Mora, R. (2013). Uso de metodos estadisticos para la identifacion de capas de suelos volcanicos con el ensayo del cono de pentracion en los terrenos de la Universidad de Costa Rica, Montes de Oca, San Jose, Costa Rica. - Rev. Geol. Amer. Central, 49: 109-120.
#' @examples
#' testRI = RI(DPM_data, k = 7)
#' testT = T_stat(DPM_data, k = 7)
#' RI_T_plot(testRI)
#' RI_T_plot(testT)
#'
RI_T_plot = function(x, ylab = "Depth [m]") {

  nombres = names(x$Data)

  if (any(nombres == "RI")) {
    stat = "RI"
    q = ggplot(x$Data, aes(Prof, RI)) +
      geom_line(na.rm = T) +
      geom_hline(yintercept = c(.7,.8), col = c("blue", "red")) +
      geom_point(data = x$RI_Over0.7, col = "blue", size = 2) +
      geom_point(data = x$RI_Over0.8, col = "red", size = 3) +
      coord_flip() + scale_x_reverse(name = ylab) +
      scale_y_continuous(name = stat) +
      theme_bw()
  } else if (any(nombres == "Tstat")) {
    stat = "T-statistic"
    q = ggplot(x$Data, aes(Prof, Tstat)) +
      geom_line(na.rm = T) +
      geom_hline(yintercept = c(x$Stats$t95, x$Stats$t99), col = c("blue", "red")) +
      geom_point(data = x$Over95, col = "blue", size = 2) +
      geom_point(data = x$Over99, col = "red", size = 3) +
      coord_flip() + scale_x_reverse(name = ylab) +
      scale_y_continuous(name = stat) +
      theme_bw()
  }
    return(q)
}
