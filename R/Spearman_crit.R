#' @title Spearman critical value
#' @description Calculates the critical value for Spearman's correlation coeficient.
#' @param n Sample size of the correlated values
#' @param conf.level Confidence level to use for the critical value (Default is 0.95, and see details section below)
#' @export
#' @return Spearman's critical value for a given sample size
#' @references Swan, A. R. H. & Sandilands, M. (1995). Introduction to Geological Data Analysis. Blackwell Science.
#' @import stats
#' @details The result of this function can be used to estimate the statistical signficance of Spearman's correlation coeficient. The options for confidence level are 0.99, 0.95, or 0.90
#' @examples
#' Spearman_crit(10)
#' Spearman_crit(10, conf.level = 0.9)
#'
Spearman_crit = function(n, conf.level = 0.95) {

  if (conf.level == 0.9) {
    rcrit = 2.0496*(n-1)^-0.551
  } else if (conf.level == 0.95) {
    rcrit = 2.2269*(n-1)^-0.53
  } else if (conf.level == 0.99) {
    rcrit = 2.4793*(n-1)^-0.492
  }

  return(rs.crit = signif(rcrit, 3))
}
