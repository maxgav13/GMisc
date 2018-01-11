#' @title Conversion from strike to dip directions
#' @description Calculates dip directions from strike directions .
#' @param x A vector of directions in degrees
#' @export
#' @return A vector or directions
#' @import stats
#' @examples
#' x <- runif(min = 120, max = 170, n = 20)
#' dip.dir <- s2d(x)
#' strike <- d2s(strike)
#'
s2d = function(x) {
  dip.dir = ifelse(x >= 270, x + 90 - 360, x + 90)

  return(dip.dir = dip.dir)
}

