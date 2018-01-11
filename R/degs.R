#' @title Degrees
#' @description Converts from radians to degrees.
#' @param x Angle in radians
#' @export
#' @return Angle in degrees
#' @import stats
#' @examples
#' degs(0.7854)
#'
degs <- function (x){
  x * 180 / pi
}
