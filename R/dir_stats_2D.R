#' @title Directional statistics for the 2D case
#' @description Calculates the directional (circular) statistics of a sample of directional measurements for the 2D case (just the dip direction or strike).
#' @param x A vector of angular measurements in degrees
#' @param dir A logical indicating if the data are directional (1) or non-directional (0)
#' @param conf.level Confidence level to use for the cone of confidence (Default is 0.95)
#' @export
#' @details Directional data refers to dipping planes or lines, data expressed as dip direction (trend). Non-directional data refers to strike or lineations expressed as azimuths that can take two angles (i.e. 45 or 225)
#' @return A data frame with the mean direction, mean resultant length, circular variance, concentration parameter, and cone of confidence
#' @references Davis, J. C. (2002). Statistical and Data Analysis in Geology. 3rd ed. John Wiley & Sons.
#' @references Swan, A. R. H. & Sandilands, M. (1995). Introduction to Geological Data Analysis. Blackwell Science.
#' @references Borradaile, G. (2003). Statistics of Earth Science Data. Springer.
#' @examples
#' x <- c(255, 239, 222, 231, 199, 271, 222, 274, 228,
#'        246, 177, 199, 257, 201, 237, 209, 216)
#' dir_stats_2D(x)
#' # non-directional data example
#' dir_stats_2D(carolina, dir = 0)
#'
dir_stats_2D = function(x, dir = 1, conf.level = 0.95) {
  theta = x
  # Adjusts double angles for non-directional data
  if (dir == 0) {
    theta = theta * 2
  }
  thetarad = theta*pi/180

  # Components of mean vector
  x = sum(sin(thetarad))
  y = sum(cos(thetarad))
  meanrad = atan(x/y) # mean direction in radians
  meandeg = meanrad*180/pi # mean direction in degrees

  # True mean direction
  if (x > 0 & y > 0) {
    meantrue = meandeg
  } else if (x > 0 & y < 0) {
    meantrue = meandeg + 180
  } else if (x < 0 & y < 0) {
    meantrue = meandeg + 180
  } else {
    meantrue = meandeg + 360
  }

  # True direction for non-directional data
  meantrue = ifelse(dir == 0, meantrue/2, meantrue)

  # R = sqrt(x^2+y^2)/length(theta) # mean resultant length
  R = CircStats::est.rho(thetarad)
  s0 = 1-R # circular variance
  k = ifelse(R<0.65, R/6*(12+6*R^2+5*R^4), 1/(2*(1-R)-(1-R)^2-(1-R)^3))

  # Cone of confidence
  a = 1 - conf.level
  se = (1/sqrt(length(theta)*R*k))*180/pi
  se = ifelse(dir == 0, se/2, se) # True se for non-directional data
  cono = stats::qnorm(1-a/2)*se
  cono_sup = meantrue + cono
  cono_inf = meantrue - cono

  cono_sup = ifelse(cono_sup > 360, cono_sup - 360, cono_sup)
  cono_inf = ifelse(cono_inf < 0, 360 + cono_inf, cono_inf)

  cono_sup = ifelse(cono_sup < cono_inf & cono_sup < 180,
                    cono_sup+180, cono_sup)
  cono_inf = ifelse(cono_inf > cono_sup & cono_inf > 180,
                    cono_inf-180, cono_inf)
  meantrue = ifelse(dplyr::between(meantrue, cono_inf, cono_sup),
                    meantrue, cono_sup - cono)

  dir.stats = data.frame(Mean.Dir = round(meantrue, 1), R = signif(R, 4), Circ.Var = signif(s0, 4),
                         Conc.Param = signif(k, 4), Cone.lower = round(cono_inf, 1), Cone.upper = round(cono_sup, 1), Cone = round(cono, 2))
  return(dir.stats)
}
