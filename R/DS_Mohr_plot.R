#' @title Draws the Mohr circles for a direct shear set of results
#' @description Draws the Mohr circles along with the Mohr-Coulomb failure envelope for a set of direct shear lab test results, and annotates the graph with the values of cohesion and friction angle.
#' @param sig.n A numeric vector of the normal stress values
#' @param tau A numeric vector of the shear stress values
#' @param units A string with the units of the measurements
#' @export
#' @return A ggplot of the Mohr circles
#' @references Coduto, D. P. (1999). Geotechnical Engineering - Principles and Practices. Prentice Hall.
#' @references Holtz, R. D., Kovacs, W. D. & Sheahan, T. C. (2011). An Introduction to Geotechnical Engineering. Prentice Hall.
#' @references Gonzalez de Vallejo, L. I. (2004). Ingenieria Geologica. Prentice Hall.
#' @import ggplot2
#' @examples
#' sig.n = c(80,237,395)
#' tau = c(127,345,475)
#' DS_Mohr_plot(sig.n, tau)
#'
DS_Mohr_plot <- function(sig.n, tau, units = 'kPa') {
  # Calculates failure envelope and c and phi parameters
  ds = stats::lm(tau~sig.n)
  DS = stats::coef(ds)
  # c = DS[1]
  # phi = degs(atan(DS[2]))
  if(DS[[1]] > 0){
    c = DS[[1]]
    phi = degs(atan(DS[[2]]))
  } else {
    DS = stats::coef(stats::lm(tau ~ sig.n - 1))
    c = 0
    phi = degs(atan(DS[[1]]))
  }
  r2 = summary(ds)$r.squared
  r = stats::cor(sig.n,tau)
  (rmse = sqrt(sum((stats::residuals(ds))^2)/stats::df.residual(ds)))

  # Calculates center and radius for each pair
  C = sig.n+(tau/tan(rads(90-phi)))
  R = tau/sin(rads(90-phi))

  # Calculates principal stresses
  sig1 = C+R
  sig3 = C-R

  # MC failure envelope
  x = seq(0,max(sig.n)*1.2)
  y = c+tan(rads(phi))*x

  # Mohr circle calculations
  circ = 0:180
  circ_x = matrix(0,length(circ),length(tau))
  circ_y = matrix(0,length(circ),length(tau))

  for (i in 1:length(tau)) {
    circ_x[,i] = C[i]+R[i]*cos(rads(circ))
  }

  for (i in 1:length(tau)) {
    circ_y[,i] = R[i]*sin(rads(circ))
  }

  Mohr = list()
  for (i in 1:length(tau)) {
    Mohr[[i]] = data.frame(X=circ_x[,i],Y=circ_y[,i],cir=i)
  }
  #MOHR = do.call(rbind,Mohr)
  MOHR = dplyr::bind_rows(Mohr,.id = 'circ')
  MOHR$cir = as.factor(MOHR$cir)

  # Text to annotate with phi and c parameters
  phitext = substitute(paste(phi, "' = ",p,"deg"),list(p=round(phi,digits=1)))
  ctext = substitute(paste("c' = ",c," kPa"),list(c=round(c,digits=1)))
  labelphi = sprintf("phi*minute == %0.1f*degree",phi)
  labelc = sprintf("c*minute == %0.1f~%s",c,units)

  # Plot failure envelope and Mohr circles
  gstm = ggplot()+geom_line(aes(x,y),col="red")+
    coord_fixed(xlim=c(0,C[length(C)]+1.25*R[length(R)]),
                ylim=c(0,1.25*R[length(R)]))+
    geom_line(data=MOHR,aes(.data$X,.data$Y,group=.data$cir),col="blue")+
    labs(x=paste0('Normal stress (',units,')'),
         y=paste0('Shear stress (',units,')')) +
    # labs(x=TeX(str_glue('$\\sigma$ ({units})')),
    #      y=TeX(str_glue('$\\tau$ ({units})')))+
    annotate("text",x=C[length(C)]+0.7*R[length(R)],y=1.15*R[length(R)],
             label=labelphi,parse=T)+
    annotate("text",x=C[length(C)]+0.7*R[length(R)],y=1.05*R[length(R)],
             label=labelc,parse=T)+
    theme_bw()
  return(gstm)
}
