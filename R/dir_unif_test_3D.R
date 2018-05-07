#' @title Directional statistics test of uniformity (3D)
#' @description Performes the uniformity test, for the 3D case, for a set of angle measurements to determine if they come from a uniform distribution (no prefered mean direction).
#' @param dir A vector of dip directions measurements in degrees
#' @param dip A vector of dip angles measurements in degrees
#' @param conf.level Confidence level to use for the critical value (Default is 0.95, and see details section below)
#' @export
#' @details The options for confidence level are 0.99, 0.975, 0.95, or 0.90
#' @return A list with the mean resultant length, the critical mean resultant length, and the interpretation of these values
#' @import stats
#' @references Davis, J. C. (2002). Statistical and Data Analysis in Geology. 3rd ed. John Wiley & Sons.
#' @references Swan, A. R. H. & Sandilands, M. (1995). Introduction to Geological Data Analysis. Blackwell Science.
#' @references Borradaile, G. (2003). Statistics of Earth Science Data. Springer.
#' @examples
#' dir = c(12,18,22,15,10,20)
#' dip = c(42,40,48,30,42,30)
#' dir_unif_test_3D(dir, dip)
#'
dir_unif_test_3D = function(dir, dip, conf.level = 0.95) {

  x = dir
  y = dip

  r = dir_stats_3D(x, y)
  n = length(x)

  R = r$R
  Rcrit = R_crit(n, case = "3D", conf.level = conf.level)
  # Rcrit = (sqrt(2.605 * n) - .04) / n

  interpretation = ifelse(R > Rcrit,
                          paste("Reject H0 and conclude that the sample has a preferred mean direction"),
                          paste("Do not reject H0 and conclude there is not enough evidence to suggest that the sample has a preferred mean direction"))

  return(list(R = round(R, 3), Rcrit = round(Rcrit, 3),
              interpretation = interpretation))
}
