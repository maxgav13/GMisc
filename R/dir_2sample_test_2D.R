#' @title Directional statistics two-sample test (2D)
#' @description Performes the two-sample test, for the 2D case, for a pair of angle measurements to determine if they could belong to the same population (event).
#' @param x A vector of angular measurements in degrees for sample 1
#' @param y A vector of angular measurements in degrees for sample 2
#' @param dir A logical indicating if the data are directional (1) or non-directional (0)
#' @param conf.level Confidence level for the F-statistic calculation and p-value interpretation (Default is 0.95)
#' @export
#' @details Directional data refers to dipping planes or lines, data expressed as dip direction (trend). Non-directional data refers to strike or lineations expressed as azimuths that can take two angles (i.e. 45 or 225)
#' @return A list with the F-statistic, the critical value of F, the degrees of freedom, the p-value, and the interpretation of these values
#' @references Swan, A. R. H. & Sandilands, M. (1995). Introduction to Geological Data Analysis. Blackwell Science.
#' @examples
#' x = c(255, 239, 222, 231, 199, 271, 222, 274, 228, 246, 177, 199,
#'   257, 201, 237, 209, 216, 180, 182, 250, 219, 196, 197, 246, 218, 235,
#'   232, 243, 232, 180, 231, 254, 242, 149, 212, 210, 230, 205, 220, 268)
#' y = c(225, 208, 172, 198, 204, 183, 190, 212, 247, 127, 167, 234,
#'   217, 192, 212, 171, 169, 210, 245, 222, 185, 227, 193, 178, 187, 182,
#'   194, 217, 168, 211, 234, 204, 221, 198, 261, 228, 146, 201, 146, 231)
#' dir_2sample_test_2D(x, y)
#'
dir_2sample_test_2D = function(x, y, dir = 1, conf.level = 0.95) {

  xy = c(x, y)

  R1 = CircStats::est.rho(rads(x)) * length(x)
  R2 = CircStats::est.rho(rads(y)) * length(y)
  Rt = CircStats::est.rho(rads(xy)) * length(xy)
  kt = CircStats::est.kappa(rads(xy))

  df2 = length(xy) - 2
  f = ifelse(kt > 10, ((df2) * (R1 + R2 - Rt)) / (length(xy) - R1 - R2),
             (1 + 3 / (8 * kt)) * ((df2) * (R1 + R2 - Rt)) / (length(xy) - R1 - R2))
  fcrit = stats::qf(conf.level, 1, df2)
  p = stats::pf(f, 1, df2, lower.tail = F)

  interpretation = ifelse(f > fcrit,
                          paste("Reject H0 and conclude that the two samples could not come from the same population with the same mean direction"),
                          paste("Do not reject H0 and conclude that there is not enough evidence that the two samples could come from the same population with the same mean direction"))

  return(list(f = round(f, 2), fcrit = round(fcrit, 2),
              df1 = 1, df2 = df2,
              p_value = signif(p, 3), interpretation = interpretation))
}
