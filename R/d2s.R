#' @title Conversion from dip directions to strike
#' @description Calculates strike directions from dip directions.
#' @param x A vector of directions in degrees
#' @export
#' @return A vector or directions
#' @import stats
#' @examples
#' x <- runif(min = 30, max = 80, n = 20)
#' strike <- d2s(x)
#' dip.dir <- s2d(strike)
#'
d2s = function(x) {
  az = ifelse(x >= 0 & x < 90, x - 90 + 360, x - 90)

  return(strike = az)
}
