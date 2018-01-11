#' @title Stereonet (mean)
#' @description Draws the mean great circle, mean direction and cone of confidence for a set of measurements.
#' @param dir A vector of dir directions in degrees
#' @param dip A vector of dip angles in degrees
#' @param conf.level Confidence level for the cone of confidence (Default is 0.95)
#' @param points.shape Shape value (pch) for the data points
#' @param points.col Color for the data points
#' @param plane.col Color for the mean plane and cone of confidence
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
stereo_mean = function(dir, dip, conf.level = 0.95, points.shape = 3, points.col = "blue", plane.col = "red", add = FALSE) {

  r = dir_stats_3D(dir, dip, conf.level = conf.level)

  if (add == TRUE) {
    par(new = TRUE)
  }

  net(col = gray(0.85), lwd = .5)
  focpoint(dir, dip, pch = points.shape, col = points.col)
  stereo_plane(r$Mean.Dir, r$MeanDip, dir = "dip", col = plane.col, add = T)
  focpoint(r$Mean.Dir, r$MeanDip, pch = 19, col = plane.col)
  addsmallcirc(r$Mean.Dir, 90 - r$MeanDip, r$Cone, col = plane.col)
}

