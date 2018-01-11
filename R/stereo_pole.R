#' @title Stereonet (poles)
#' @description Draws the poles to planes (great circles) on an equal area stereonet.
#' @param dir A vector of dip directions in degrees
#' @param dip A vector of dip angles in degrees
#' @param points.col Color for the poles
#' @param points.shape Shape value (pch) for the poles
#' @param add A logical indicating to add or not the poles to an existing plot
#' @export
#' @details This can be added to a stereonet from \code{stereo_plane()}
#' @return A stereonet plot
#' @import stats
#' @import RFOC
#' @examples
#' dip.dir <- runif(min = 120, max = 170, n = 20)
#' dip <- runif(min = 10, max = 60, n = 20)
#' stereo_pole(dip.dir, dip)
#' stereo_plane(dip.dir, dip, dir = "dip")
#' stereo_pole(dip.dir, dip, add = TRUE)
#'
stereo_pole = function(dir, dip, points.col = "blue", points.shape = 3, add = FALSE) {

  dir = ifelse(dir >= 180, dir - 180, dir + 180)

  if (add == FALSE) {
    net(col = gray(0.85), lwd = .5)
    qpoint(dir, dip, col = points.col, pch = points.shape, PLOT = add)
  }
  qpoint(dir, dip, col = points.col, pch = points.shape, PLOT = TRUE)
}
