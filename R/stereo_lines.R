#' @title Stereonet (lines)
#' @description Draws lines (as points) for a set of measurements.
#' @param dir A vector of dir directions in degrees
#' @param dip A vector of dip angles in degrees
#' @param points.shape Shape value (pch) for the data points
#' @param points.col Color for the data points
#' @param add A logical indicating to add or not another set of measurements and its calculations
#' @export
#' @return A stereonet plot of data points
#' @import stats
#' @import RFOC
#' @examples
#' dir <- runif(min = 30, max = 80, n = 20)
#' dip <- runif(min = 10, max = 60, n = 20)
#' stereo_lines(dir, dip)
#'
stereo_lines = function(dir, dip, points.shape = 3, points.col = "blue", add = FALSE) {

  r = dir_stats_3D(dir, dip)

  if (add == TRUE) {
    par(new = TRUE)
  }

  net(col = gray(0.85), lwd = .5)
  focpoint(dir, dip, pch = points.shape, col = points.col)
}

