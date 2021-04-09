#' @title Stereonet (planes)
#' @description Draws the great circles of planes on an equal area stereonet.
#' @param az A vector of angular measurements in degrees (strike or dip directions)
#' @param dip A vector of dip angles in degrees
#' @param dir A string indicating which direction is being given: "strike" (default) or "dip"
#' @param col Color for the great circles
#' @param add A logical indicating to add or not the great circles to an existing plot
#' @export
#' @details If "strike" is given it assumes it is using the 'right-hand-rule'. This can be added to a stereonet from \code{stereo_pole()}
#' @return A stereonet plot
#' @examples
#' strike <- runif(min = 30, max = 80, n = 20)
#' dip.dir <- s2d(strike)
#' dip <- runif(min = 10, max = 60, n = 20)
#' strike2 <- runif(min = 240, max = 290, n = 20)
#' dip2 <- runif(min = 10, max = 60, n = 20)
#' stereo_plane(strike, dip)
#' stereo_plane(dip.dir, dip, dir = "dip")
#' stereo_plane(strike2, dip2, col = "red", add = TRUE)
#'
stereo_plane = function(az, dip, dir = c("strike", "dip"), col = "blue", add = FALSE) {
  if(any(dir == "strike")) {
    az = az
  } else {
    az = d2s(az)
  }
  if (add == TRUE) {
    graphics::par(new = TRUE)
  }
  RFOC::net(col = gray(0.85), lwd = .5)
  for (i in 1:length(dip)) {
    RFOC::lowplane(az = az[i], dip = dip[i], col = col)
  }
}
