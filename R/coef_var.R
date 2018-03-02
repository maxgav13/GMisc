#' @title Coefficient of variation
#' @description Calculates the coefficient of variation, disregarding NAs.
#' @param x A numeric vector
#' @export
#' @return The coefficient of variation for the given vector
#' @import stats
#' @examples
#' x = rnorm(50, 150, 30)
#' coef_var(x)
#' z = data.frame(N1 = rnorm(50, 150, 30), N2 = rnorm(50, 50, 10))
#' apply(z, 2, coef_var)
#'
coef_var = function(x) {
  cv = round(sd(x, na.rm = T) / mean(x, na.rm = T), 3)
  cv
}
