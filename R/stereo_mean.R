#' @title Stereonet (mean)
#' @description Draws the mean direction and cone of confidence for a set of measurements.
#' @param dir A vector of dir directions in degrees
#' @param dip A vector of dip angles in degrees
#' @param conf.level Confidence level for the cone of confidence (Default is 0.95)
#' @param mean.col Color for the mean and cone of confidence
#' @param add A logical indicating to add or not another set of measurements and its calculations
#' @export
#' @return A stereonet plot of data points and its mean estimations
#' @import stats
#' @import RFOC
#' @examples
#' dir <- runif(min = 30, max = 80, n = 20)
#' dip <- runif(min = 10, max = 60, n = 20)
#' stereo_mean(dir, dip)
#'
stereo_mean = function(dir, dip, conf.level = 0.95, mean.col = "red", add = FALSE) {

  r = dir_stats_3D(dir, dip, conf.level = conf.level)

  if (add == TRUE) {
    par(new = TRUE)
  }

  net(col = gray(0.85), lwd = .5)
  # stereo_plane(r$Mean.Dir, r$MeanDip, dir = "dip", col = mean.col, add = T)
  focpoint(r$Mean.Dir, r$MeanDip, pch = 19, col = mean.col)
  addsmallcirc(r$Mean.Dir, 90 - r$MeanDip, r$Cone, col = mean.col)
}

