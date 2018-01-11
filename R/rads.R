#' @title Radians
#' @description Converts from degrees to radians.
#' @param x Angle in degrees
#' @export
#' @return Angle in radians
#' @import stats
#' @examples
#' rads(45)
#'
rads <- function (x){
  x * pi / 180
}
