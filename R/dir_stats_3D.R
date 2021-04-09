#' @title Directional statistics for the 3D case
#' @description Calculates the directional (spherical) statistics of a sample of directional measurements for the 3D case (plane with dip direction and dip angle or line with trend and plunge).
#' @param dir A vector of dip directions measurements in degrees
#' @param dip A vector of dip angles measurements in degrees
#' @param conf.level Confidence level to use for the cone of confidence (Default is 0.95)
#' @details This always considers a plane expressed as dip direction and dip angle, it could be a bedding plane or a fault plane
#' @export
#' @return A data frame with the mean direction, mean dip angle, mean resultant length, spherical variance, concentration parameter, and cone of confidence
#' @references Borradaile, G. (2003). Statistics of Earth Science Data. Springer.
#' @examples
#' dir = c(12,18,22,15,10,20)
#' dip = c(42,40,48,30,42,30)
#' dir_stats_3D(dir, dip)
#'
dir_stats_3D = function(dir, dip, conf.level = 0.95) {
  # Vector of spherical measurements
  d = dir # dip directions
  i = dip # dip angles
  drad = d*pi/180
  irad = i*pi/180

  # Components of mean vector
  x = cos(irad)*cos(drad)
  y = cos(irad)*sin(drad)
  z = sin(irad)

  R = sqrt(sum(x)^2+sum(y)^2+sum(z)^2)
  Rbar = R/length(d) # mean resultant length
  ss = 1-Rbar # spherical variance
  k = (length(d)-2)/(length(d)-R)
  meandir = (atan(sum(y)/sum(x)))*180/pi # mean dip direction in degrees
  meandip = (asin(sum(z)/R))*180/pi # mean dip angle in degress

  # True mean dip direction
  if (sum(y)>0 & sum(x)>0) {
    meandirtrue = meandir
  } else if (sum(y)>0 & sum(x)<0) {
    meandirtrue = meandir+180
  } else if (sum(y)<0 & sum(x)<0) {
    meandirtrue = meandir+180
  } else {
    meandirtrue = meandir+360
  }

  # Cone of confidence
  p = 1 - conf.level
  N = length(d)
  cono = (acos(1-((N-R)/R)*((1/p)^(1/(N-1))-1)))*180/pi
  cono_sup = meandirtrue + cono
  cono_inf = meandirtrue - cono

  cono_sup = ifelse(cono_sup > 360, cono_sup - 360, cono_sup)
  cono_inf = ifelse(cono_inf < 0, 360 + cono_inf, cono_inf)

  dir.stats = data.frame(Mean.Dir = round(meandirtrue, 1), MeanDip = round(meandip, 1), R = signif(Rbar, 4), Sph.Var = signif(ss, 4),
                         Conc.Param = signif(k, 4), Cone.lower = round(cono_inf, 1), Cone.upper = round(cono_sup, 1), Cone = round(cono, 2))
  return(dir.stats)
}
