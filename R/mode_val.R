#' @title Modal value
#' @description Calculates the mode (modal value) of a vector of numbers
#' @param x A vector of numbers
#' @export
#' @return The modal value of a numeric vector
#' @importFrom stats density
#' @examples
#' x = rnorm(50, 80, 15)
#' mode_val(x)
#'
mode_val = function(x) {
  d = density(x, na.rm = TRUE)
  mode = d$x[which.max(d$y)]
  mode
}
