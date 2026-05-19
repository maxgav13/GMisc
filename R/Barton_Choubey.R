#' @title Barton-Choubey criterion
#' @description Calculates the Barton-Choubey criterion for the given data and for the given level of stress (unit weight and depth), and an option for plotting it.
#' @param JRC The Joint Roughness Coefficient (1-20)
#' @param JCS The Joint Compressive Strength in MPa
#' @param phi.r The residual friction angle of the rock
#' @param unit.weight The unit weight of the rock in kN/m3
#' @param depth The depth, in meters, to which calculate the parameters
#' @param BC The result from running \code{Barton_Choubey()}
#' @param units The units to use in the plot, either "kPa" or "MPa". Default is "kPa". If "MPa" is desired you can use units = "" or units = "MPa"
#' @export
#' @references Barton, N. & Choubey, V. (1977). The shear strength of rock joints in theory and practice. Rock Mechanichs, 10: 1-54.
#' @return For \code{Barton_Choubey()}: a list with 3 tibbles: data (normal stress and shear stress), stress level point, and parameters (c and phi). All stress values are in MPa
#' @family rock mechanics
#' @rdname Barton_Choubey
#'
Barton_Choubey = function(JRC, JCS, phi.r, unit.weight, depth) {
  sig.n = (unit.weight * depth) / 1000
  # phi.r = phi.b - JRC * log10(JCS/sig.n)
  x = seq(0.01, 3 * sig.n, 0.01)
  tau = x * tan(rads(phi.r + JRC * log10(JCS / x)))
  spl = stats::smooth.spline(tau ~ x)
  pred0 = stats::predict(spl, x = sig.n, deriv = 0)
  pred1 = stats::predict(spl, x = sig.n, deriv = 1)
  c = signif(pred0$y - (pred1$y * sig.n), 3)
  phi = signif(degs(atan(pred1$y)), 3)
  return(list(
    dat = tibble::tibble(sig_n = round(x, 3), tau = round(tau, 3)),
    stress.level = tibble::tibble(data.frame(pred0)),
    parameters = tibble::tibble(c, phi)
  ))
}

#' @export
#' @return For \code{Barton_Choubey_plot()}: a ggplot object for the Barton-Choubey criterion
#' @import ggplot2
#' @rdname Barton_Choubey
#' @examples
#' JRC = 10
#' JCS = 30
#' phi.r = 26
#' unit.weight = 18.6
#' depth = 40
#' BC = Barton_Choubey(JRC, JCS, phi.r, unit.weight, depth)
#' BC
#' Barton_Choubey_plot(BC)
#'
Barton_Choubey_plot = function(BC, units = "kPa") {
  unit.factor = ifelse(units == "kPa", 1000, 1)

  c = BC$parameters$c
  phi = BC$parameters$phi

  if (units == "kPa") {
    shear = function(x) {
      c * unit.factor + tan(rads(phi)) * x
    }
  } else {
    shear = function(x) {
      c + tan(rads(phi)) * x
    }
  }

  labelphi = sprintf("phi*minute == %0.1f*degree", phi)
  if (units == "kPa") {
    labelc = sprintf("c*minute == %0.1f~kPa", c * unit.factor)
  } else {
    labelc = sprintf("c*minute == %0.4f~MPa", c)
  }

  if (units == "kPa") {
    q = ggplot(
      BC$dat,
      aes(.data$sig_n * unit.factor, .data$tau * unit.factor)
    ) +
      geom_line(col = "red") +
      geom_point(
        data = BC$stress.level,
        aes(.data$x * unit.factor, .data$y * unit.factor),
        col = "blue",
        size = 2
      ) +
      stat_function(fun = shear, col = "blue") +
      labs(
        x = expression(paste("Normal stress ", sigma, " (kPa)")),
        y = expression(paste("Shear stress ", tau, " (kPa)"))
      ) +
      annotate(
        "text",
        x = max(BC$dat$sig_n * unit.factor) / 4,
        y = max(BC$dat$tau * unit.factor) / 1.25,
        label = labelphi,
        parse = T
      ) +
      annotate(
        "text",
        x = max(BC$dat$sig_n * unit.factor) / 4,
        y = 0.925 * max(BC$dat$tau * unit.factor) / 1.25,
        label = labelc,
        parse = T
      ) +
      theme_bw() +
      theme(
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)
      )
  } else {
    q = ggplot(BC$dat, aes(.data$sig_n, .data$tau)) +
      geom_line(col = "red") +
      geom_point(
        data = BC$stress.level,
        aes(.data$x, .data$y),
        col = "blue",
        size = 2
      ) +
      stat_function(fun = shear, col = "blue") +
      labs(
        x = expression(paste("Normal stress ", sigma, " (MPa)")),
        y = expression(paste("Shear stress ", tau, " (MPa)"))
      ) +
      annotate(
        "text",
        x = max(BC$dat$sig_n) / 4,
        y = max(BC$dat$tau) / 1.25,
        label = labelphi,
        parse = T
      ) +
      annotate(
        "text",
        x = max(BC$dat$sig_n) / 4,
        y = 0.925 * max(BC$dat$tau) / 1.25,
        label = labelc,
        parse = T
      ) +
      theme_bw() +
      theme(
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)
      )
  }
  return(q)
}
