#' @title Stereonet (principal stresses)
#' @description Adds the principal stresses to a stereonet.
#' @param dir A vector of dir directions in degrees
#' @param dip A vector of dip angles in degrees
#' @export
#' @return A stereonet plot of principal stresses
#' @examples
#' dir <- runif(min = 30, max = 80, n = 20)
#' dip <- runif(min = 10, max = 60, n = 20)
#' stereo_mean(dir, dip)
#' stereo_ps(dir, dip)
#'
stereo_ps = function(dir, dip) {

  r = dir_stats_ps(dir, dip)

  RFOC::qpoint(r[1,], 90-r[2,], pch = 18, col = 6, lab = c("1", "2", "3"), cex = 1.5)

}
