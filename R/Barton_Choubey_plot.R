#' @title Barton-Choubey criterion plot
#' @description Draws the Barton-Choubey criterion for the given data and for the given level of stress (unit weight and depth).
#' @param BC The result from running \code{Barton_Choubey()}
#' @param units The units to use in the plot, either "kPa" or "MPa". Default is "kPa". If "MPa" is desired you can use units = "" or units = "MPa"
#' @export
#' @return A ggplot object for the Barton-Choubey criterion
#' @import stats
#' @import ggplot2
#' @references Barton, N. & Choubey, V. (1977). The shear strength of rock joints in theory and practice. Rock Mechanichs, 10: 1-54.
#' @examples
#' JRC = 10
#' JCS = 30
#' phi.r = 26
#' unit.weight = 18.6
#' depth = 40
#' BC = Barton_Choubey(JRC, JCS, phi.r, unit.weight, depth)
#' Barton_Choubey_plot(BC)
#'
Barton_Choubey_plot = function(BC, units = "kPa") {

  unit.factor = ifelse(units == "kPa", 1000, 1)

  c = BC$parameters$c
  phi = BC$parameters$phi

  if (units == "kPa") {
    shear = function(x) {c*unit.factor + tan(rads(phi)) * x}
  } else {
    shear = function(x) {c + tan(rads(phi)) * x}
  }

  labelphi = sprintf("phi*minute == %0.1f*degree", phi)
  if (units == "kPa") {
    labelc = sprintf("c*minute == %0.1f~kPa", c*unit.factor)
  } else {
    labelc = sprintf("c*minute == %0.4f~MPa", c)
  }

  if (units == "kPa") {
    q = ggplot(BC$dat, aes(sig_n*unit.factor, tau*unit.factor)) +
      geom_line(col = "red") +
      geom_point(data = BC$stress.level, aes(x*unit.factor, y*unit.factor), col = "blue", size = 2) +
      stat_function(fun = shear, col = "blue") +
      labs(x = expression(paste("Normal stress ", sigma, " [kPa]")),
           y = expression(paste("Shear stress ", tau, " [kPa]"))) +
      annotate("text", x = max(BC$dat$sig_n*unit.factor)/4, y = max(BC$dat$tau*unit.factor)/1.25,
               label = labelphi, parse = T)+
      annotate("text", x = max(BC$dat$sig_n*unit.factor)/4, y = 0.925*max(BC$dat$tau*unit.factor)/1.25,
               label = labelc, parse = T)+
      theme_bw()
  } else {
    q = ggplot(BC$dat, aes(sig_n, tau)) +
      geom_line(col = "red") +
      geom_point(data = BC$stress.level, aes(x, y), col = "blue", size = 2) +
      stat_function(fun = shear, col = "blue") +
      labs(x = expression(paste("Normal stress ", sigma, " [MPa]")),
           y = expression(paste("Shear stress ", tau, " [MPa]"))) +
      annotate("text", x = max(BC$dat$sig_n)/4, y = max(BC$dat$tau)/1.25,
               label = labelphi, parse = T)+
      annotate("text", x = max(BC$dat$sig_n)/4, y = 0.925*max(BC$dat$tau)/1.25,
               label = labelc, parse = T)+
      theme_bw()
  }
  return(q)
}
