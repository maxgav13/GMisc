#' @title Mohr circle solution and plot
#' @description Solves a Mohr circle problem for the given normal stresses, shear stress, and rotation angle, and plots the results.
#' @param sigx A number corresponding with the mayor normal stress
#' @param sigy A number corresponding with the minor normal stress
#' @param tauxy A number corresponding with the shear stress
#' @param theta Angle of rotation for which to find the state of stresses
#' @param digits Number of significant digits to us
#' @param units A string with the units of the measurements
#' @export
#' @return A list containing a data frame with the results, and a ggplot object with the Mohr circle
#' @references Coduto, D. P. (1999). Geotechnical Engineering - Principles and Practices. Prentice Hall.
#' @references Holtz, R. D., Kovacs, W. D. & Sheahan, T. C. (2011). An Introduction to Geotechnical Engineering. Prentice Hall.
#' @references Gonzalez de Vallejo, L. I. (2004). Ingenieria Geologica. Prentice Hall.
#' @import stats
#' @import tidyverse
#' @examples
#' sigx = 143.6
#' sigy = 100.5
#' tauxy = -14.4
#' Mohr_Circle(sigx, sigy, tauxy)
#' Mohr_Circle(sigx, sigy, tauxy, theta = -35)
#'
Mohr_Circle = function(sigx, sigy, tauxy, theta = 0, digits = 5, units = 'kPa') {
  tauyx = -tauxy
  # Centro, radio, tau max, y esfuerzos principales
  C = (sigx+sigy)/2
  R = sqrt(((sigx-sigy)/2)^2+tauxy^2)
  sig1 = C+R
  sig2 = C-R
  tau_max = R

  theta_1 = degs(atan(tauxy/(sig1-sigy)))
  theta_2 = degs(atan(tauxy/(sig2-sigy)))

  # Esfuerzos en un plano rotado con respecto a la horizontal
  tau_theta = ((sigx-sigy)/2)*sin(rads(2*theta))+(tauxy*cos(rads(2*theta)))
  sig_theta_x = ((sigx+sigy)/2)+((sigx-sigy)/2)*cos(rads(2*theta))-(tauxy*sin(rads(2*theta)))
  sig_theta_y = (sig1+sig2)-sig_theta_x

  # Esfuerzos en un plano rotado con respecto a sig1
  tau_theta_p = ((sig1-sig2)/2)*sin(rads(2*theta));
  sig_theta_p = (sig1+sig2)/2+((sig1-sig2)/2)*cos(rads(2*theta))


  # Coordenadas del circulo
  circ = 0:360
  circ_x = C+R*cos(rads(circ))
  circ_y = R*sin(rads(circ))

  # Grafico del circulo
  g = ggplot()+theme_bw()+
    geom_path(aes(circ_x,circ_y),na.rm = T)+
    xlim(C-1.5*R,C+1.5*R)+ylim(-1.5*R,1.5*R)+
    coord_fixed()+
    geom_hline(yintercept = 0,na.rm = T)+
    geom_point(aes(sigx,tauxy),size=3,col="blue")+
    geom_point(aes(sigy,tauyx),size=3,col="blue")+
    geom_path(aes(c(sigx,tauxy),c(sigy,tauyx)),col="blue",na.rm = T)+
    geom_point(aes(sig1,0),size=2)+
    geom_point(aes(sig2,0),size=2)+
    geom_point(aes(C,R),size=2)+
    geom_point(aes(sig_theta_x,tau_theta),size=3,col="red")+
    labs(x=paste0('Normal stress (',units,')'),
         y=paste0('Shear stress (',units,')')) +
    # labs(x=TeX(str_glue('$\\sigma$ ({units})')),
    #      y=TeX(str_glue('$\\tau$ ({units})')))+
    annotate("text",x=C+1.1*R,y=R/12,label=sprintf("sigma[1]"),parse=T)+
    annotate("text",x=C-1.1*R,y=R/12,label=sprintf("sigma[2]"),parse=T)+
    annotate("text",x=C,y=1.1*R,label=sprintf("tau[max]"),parse=T)+
    annotate("text",x=sigx+R/4,y=tauxy,label=bquote("(sigma[x]~tau[xy])"),parse=T)+
    annotate("text",x=sigy-R/4,y=tauyx,label="paste((sigma[y]~tau[yx]))",parse=T)+
    annotate("text",x=sig_theta_x-R/4,y=tau_theta,label="paste((sigma[theta]~tau[theta]))",parse=T)+
    theme(axis.text=element_text(size=12),axis.title=element_text(size=15))
  df = signif(data.frame(sig1, sig2, sig_theta = sig_theta_x, tau_theta, tau_max), digits)
  return(list(results = df, figure = g))
}
