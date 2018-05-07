#' @title Directional statistics test of uniformity (2D)
#' @description Performes the uniformity test, for the 2D case, for a set of angle measurements to determine if they come from a uniform distribution (no prefered mean direction).
#' @param x A vector of angular measurements in degrees
#' @param dir A logical indicating if the data are directional (1) or non-directional (0)
#' @param conf.level Confidence level to use for the critical value (Default is 0.95, and see details section below)
#' @export
#' @details Directional data refers to dipping planes or lines, data expressed as dip direction (trend). Non-directional data refers to strike or lineations expressed as azimuths that can take two angles (i.e. 45 or 225). The options for confidence level are 0.99, 0.975, 0.95, or 0.90
#' @return A list with the mean resultant length, the critical mean resultant length, and the interpretation of these values
#' @import stats
#' @references Davis, J. C. (2002). Statistical and Data Analysis in Geology. 3rd ed. John Wiley & Sons.
#' @references Swan, A. R. H. & Sandilands, M. (1995). Introduction to Geological Data Analysis. Blackwell Science.
#' @references Borradaile, G. (2003). Statistics of Earth Science Data. Springer.
#' @examples
#' x = c(255, 239, 222, 231, 199, 271, 222, 274, 228, 246, 177, 199, 257, 201, 237, 209, 216, 180, 182, 250, 219, 196, 197, 246, 218, 235, 232, 243, 232, 180, 231, 254, 242, 149, 212, 210, 230, 205, 220, 268)
#' dir_unif_test_2D(x)
#'
dir_unif_test_2D = function(x, dir = 1, conf.level = 0.95) {

  r = dir_stats_2D(x, dir = dir)
  n = length(x)

  R = r$R
  Rcrit = R_crit(n, case = "2D", conf.level = conf.level)
  # Rcrit = (sqrt(2.605 * n) - .04) / n

  interpretation = ifelse(R > Rcrit,
                          paste("Reject H0 and conclude that the sample has a preferred mean direction"),
                          paste("Do not reject H0 and conclude there is not enough evidence to suggest that the sample has a preferred mean direction"))

  return(list(R = round(R, 3), Rcrit = round(Rcrit, 3),
              interpretation = interpretation))
}
