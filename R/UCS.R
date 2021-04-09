#' @title Uniaxial Compressive Strength (UCS)
#' @description Calculates different possible values of UCS based on point load test measurements. It performs several regressions (Gaussian and robust), and also calculates corrected value based on the point load index.
#' @param W A vector of sample widths, in mm
#' @param D A vector of sample diameters, in mm
#' @param P A vector of loads at failure for each sample, in kN
#' @param digits Number of significant digits to display (Default is 2)
#' @export
#' @return A comparison table and graph
#' @references ASTM (1995). Standard Test Method for Determination of the Point Load Strength Index of Rock.
#' @import ggplot2
#' @name UCS
#' @examples
#' W = c(70.0,95.0,80.0,90.0,98.0,110.0,100.0,100.0,110.0)
#' D = c(40,55,60,50,25,60,40,70,50)
#' P = c(4.3,3.4,4.5,3.1,5.0,6.4,2.6,11,4.1)
#' UCS(W = W, D = D, P = P)
#'
UCS = function(W, D, P, digits = 2) {

  A = W * D
  De2 = 4 * A / pi
  De = sqrt(De2)

  dat.UCS = data.frame(P, De)
  dat.50 = data.frame(De = 50)

  I = P / De2 * 1000
  f = (De / 50)^.45
  I50 = I * f

  # regresion t
  # fit.t = heavyLm(P ~ De, data = dat.UCS, family = slash(df = nu))
  # summary(fit.t)
  # P50.t = fit.t$coefficients[[1]] + fit.t$coefficients[[2]] * 50
  # I50.t = (P50.t / 3183.1) * 1000
  # UCS.t = I50.t * 23

  # regresion robusta
  fit.r = MASS::rlm(P ~ De, data = dat.UCS)
  # summary(fit.r)
  P50.r = stats::predict(fit.r, newdata = dat.50)
  I50.r = (P50.r / 3183.1) * 1000
  UCS.r = I50.r * 23

  # regresion normal
  fit.g = stats::lm(P ~ De, data = dat.UCS)
  # summary(fit.g)
  P50.g = stats::predict(fit.g, newdata = dat.50)
  I50.g = (P50.g / 3183.1) * 1000
  UCS.g = I50.g * 23

  gg = ggplot(dat.UCS, aes(De, P)) +
    geom_point(size=2) +
    geom_smooth(method = 'lm', se = F, aes(col='Gaussian'), size=.8) +
    # geom_line(aes(De, fit.t$fitted.values, col='t-distributed'), size=.8) +
    geom_smooth(method = MASS::rlm, se = F, aes(col='Robust'), size=.8) +
    geom_vline(xintercept = 50, size=1, linetype=8) +
    theme_bw(base_size = 12) +
    labs(x='Equivalent diameter, De [mm]',y='Load, P [kN]',col='Method') +
    scale_color_brewer(type = 'qual', palette = 6) +
    theme(legend.position = 'top',
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12))

  UCS = c(mean(I50), stats::median(I50)) * 23

  UCS.res = data.frame(Method = c('Gaussian','Robust','Corrected (mean)','Corrected (median)'),
                       I50 = round(c(I50.g,I50.r,mean(I50),stats::median(I50)),digits = digits),
                       UCS = round(c(UCS.g,UCS.r,UCS[1],UCS[2]),digits = digits))

  names(UCS.res) = c('Method', 'I50 [MPa]', 'UCS [MPa]')

  return(list(Graph = gg, Summary = UCS.res))
}
