#' @title Directional statistics two-sample test (3D)
#' @description Performes the two-sample test, for the 3D case, for a pair of angular measurements to determine if they could belong to the same population (event).
#' @param d1 A vector of dip direction measurements in degrees for sample 1
#' @param i1 A vector of dip measurements in degrees for sample 1
#' @param d2 A vector of dip direction measurements in degrees for sample 2
#' @param i2 A vector of dip measurements in degrees for sample 2
#' @param conf.level Confidence level for the F-statistic calculation and p-value interpretation (Default is 0.95)
#' @export
#' @return A list with the F-statistic, the critical value of F, the p-value, and the interpretation of these values
#' @import stats
#' @references Borradaile, G. (2003). Statistics of Earth Science Data. Springer.
#' @examples
#' d1 = c(12,18,22,15,10,20)
#' i1 = c(42,40,48,30,42,30)
#' d2 = c(111.7,109.2,185.1,83.1,235.2,226.7)
#' i2 = c(40.4,31.4,25.9,26.2,27,31.5)
#' dir_2sample_test_3D(d1, i1, d2, i2)
#'
dir_2sample_test_3D = function(d1, i1, d2, i2, conf.level = 0.95) {

  dc = c(d1, d2)
  ic = c(i1, i2)

  r1 = dir_stats_3D(d1, i1)
  r2 = dir_stats_3D(d2, i2)
  r3 = dir_stats_3D(dc, ic)

  R1 = r1$R * length(d1)
  R2 = r2$R * length(d2)
  Rt = r3$R * length(dc)

  f = ((length(dc) - 2) * (R1 + R2 - Rt)) / (length(dc) - R1 - R2)
  fcrit = qf(conf.level, 2, 2 * (length(dc) - 2))
  p = pf(f, 2, 2 * (length(dc) - 2), lower.tail = F)

  interpretation = ifelse(f > fcrit,
                          paste("Reject H0 and conclude that the two samples could not come from the same population with the same mean direction and mean dip angle"),
                          paste("Do not reject H0 and conclude that there is not enough evidence that the two samples could not come from the same population with the same mean direction and mean dip angle"))

  return(list(f = round(f, 2), fcrit = round(fcrit, 2),
              p_value = signif(p, 3), interpretation = interpretation))
}
