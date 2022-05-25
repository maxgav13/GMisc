#' @title Hoek-Brown criterion plot
#' @description Draws the Hoek-Brown criterion for the given data and for the given level of stress (unit weight and depth).
#' @param HB The result from running \code{Hoek_Brown()}
#' @export
#' @return A list with two ggplot objects for the Hoek-Brown criterion (principal stresses and normal-shear stresses)
#' @import ggplot2
#' @references Hoek, E. & Brown, E. T. (1980). Empirical strength criterion for rock masses. J. Geotech. Eng. Div. ASCE 106 (GT9): 1013-1035.
#' @references Hoek, E., Carranza-Torres, C. & Corkum, B. (2002). Hoek-Brown Failure Criterion - 2002 Edition. Proc. NARMS-TAC Conference, Toronto. 267-273.
#' @references Hoek, E. & Marinos, P. (2007). A brief history of the development of the Hoek-Brown criterion. Soils and Rocks, No. 2.
#' @examples
#' sig.ci = 16
#' GSI = 75
#' mi = 13
#' MR = 300
#' D = 0
#' height = 40
#' unit.weight = 18.6
#' HB = Hoek_Brown(sig.ci, GSI, mi, MR, D, height, unit.weight)
#' Hoek_Brown_plot(HB)
#'
Hoek_Brown_plot = function(HB) {
  p = ggplot(HB$princ.stress, aes(.data$sig.3, .data$sig.1,
                                  group = .data$crit, col = .data$crit)) +
    geom_vline(xintercept = 0, size = 0.2) +
    geom_hline(yintercept = 0, size = 0.2) +
    geom_line(size = 0.3) +
    coord_fixed(ratio = .1) +
    labs(x = expression(paste(sigma[3], " (MPa)")),
         y = expression(paste(sigma[1], " (MPa)"))) +
    scale_color_manual(name = "", values = c("blue", "magenta")) +
    theme_bw() +
    theme(legend.position = "top",
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14))

  labelphi = sprintf("phi*minute == %0.1f*degree", HB$Results$phi)
  labelc = sprintf("c*minute == %0.3f~MPa", HB$Results$c)
  labelphi.inst = sprintf("phi*minute == %0.1f*degree", HB$Results$phi.h)
  labelc.inst = sprintf("c*minute == %0.3f~MPa", HB$Results$c.h)

  q = ggplot(HB$shear, aes(.data$sig.n, .data$tau,
                           group = .data$crit, col = .data$crit)) +
    geom_vline(xintercept = 0, size = 0.2) +
    geom_hline(yintercept = 0, size = 0.2) +
    geom_line(size = 0.5) +
    geom_point(data = HB$stress.level, aes(.data$sign, .data$tau),
               col = "red", size = 2) +
    coord_fixed(ratio = .5) +
    labs(x = expression(paste(sigma[n], " (MPa)")),
         y = expression(paste(tau, " (MPa)"))) +
    scale_color_manual(name = "", values = c("blue", "magenta", "red")) +
    annotate("text", x = max(HB$shear$sig.n)/5, y = max(HB$shear$tau),
             label = labelphi, parse = T) +
    annotate("text", x = max(HB$shear$sig.n)/5, y = 0.95*max(HB$shear$tau),
             label = labelc, parse = T) +
    annotate("text", x = 1.5*HB$stress.level$sign, y = HB$stress.level$tau,
             label = labelphi.inst, parse = T) +
    annotate("text", x = 1.5*HB$stress.level$sign, y = 0.825*HB$stress.level$tau,
             label = labelc.inst, parse = T) +
    theme_bw() +
    theme(legend.position = "top",
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          legend.text = element_text(size = 12))

  return(HB_plots = list(princ.stresses = p, normal.shear = q))
}
