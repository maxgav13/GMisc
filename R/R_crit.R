#' @title Mean resultant length critical value
#' @description Calculates the critical value for the mean resultant length for directional statistics.
#' @param n Sample size of the correlated values
#' @param case String idicating for which case to calculate the critical value, 2D or 3D
#' @param conf.level Confidence level to use for the critical value (Default is 0.95, and see details section below)
#' @export
#' @return Mean resultant length's critical value for a given sample size
#' @references Davis, J. C. (2002). Statistical and Data Analysis in Geology. 3rd ed. John Wiley & Sons.
#' @details The result of this function is mostly to be used with \code{dir_unif_test_xD()}, but can be used as standalone. The options for confidence level are 0.99, 0.975, 0.95, or 0.90
#' @examples
#' R_crit(10)
#' R_crit(10, conf.level = 0.9)
#' R_crit(10, case = "3D", conf.level = 0.9)
#'
R_crit = function(n, case = "2D", conf.level = 0.95) {
  if (case == "2D") {
    if (conf.level == 0.99) {
      Rcrit = 1.9018*(n)^-0.467
    } else if (conf.level == 0.975) {
      Rcrit = 1.7868*(n)^-0.48
    } else if (conf.level == 0.95) {
      Rcrit = 1.6699*(n)^-0.491
    } else if (conf.level == 0.9) {
      Rcrit = 1.5192*(n)^-0.501
    }
  } else {
    if (conf.level == 0.99) {
      Rcrit = 1.7678*(n)^-0.474
    } else if (conf.level == 0.975) {
      Rcrit = 1.6787*(n)^-0.479
    } else if (conf.level == 0.95) {
      Rcrit = 1.5519*(n)^-0.49
    } else if (conf.level == 0.9) {
      Rcrit = 1.4381*(n)^-0.501
    }
  }

  return(R.crit = signif(Rcrit, 3))
}
