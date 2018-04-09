#' @title Barton-Choubey criterion
#' @description Calculates the Barton-Choubey criterion for the given data and for the given level of stress (unit weight and depth).
#' @param JRC The Joint Roughness Coefficient (1-20)
#' @param JCS The Joint Compressive Strength in MPa
#' @param phi.r The residual friction angle of the rock
#' @param unit.weight The unit weight of the rock in kN/m3
#' @param depth The depth, in meters, to which calculate the parameters
#' @export
#' @references Barton, N. & Choubey, V. (1977). The shear strength of rock joints in theory and practice. Rock Mechanichs, 10: 1-54.
#' @return A list with 3 data frames: data (normal stress and shear stress), stress level point, and parameters (c and phi). All stress values are in MPa
#' @import stats
#' @examples
#' JRC = 10
#' JCS = 30
#' phi.r = 26
#' unit.weight = 18.6
#' depth = 40
#' Barton_Choubey(JRC, JCS, phi.r, unit.weight, depth)
#'
Barton_Choubey = function(JRC, JCS, phi.r, unit.weight, depth) {
  sig.n = (unit.weight * depth)/1000
  # phi.r = phi.b - JRC * log10(JCS/sig.n)
  x = seq(0.01, 3*sig.n, 0.01)
  tau = x * tan(rads(phi.r + JRC * log10(JCS/x)))
  spl = smooth.spline(tau ~ x)
  pred0 = predict(spl, x = sig.n, deriv = 0)
  pred1 = predict(spl, x = sig.n, deriv = 1)
  c = signif(pred0$y - (pred1$y * sig.n), 3)
  phi = signif(degs(atan(pred1$y)), 3)
  return(list(dat = data.frame(sig_n = round(x,3), tau = round(tau,3)),
              stress.level = data.frame(pred0),
              parameters = data.frame(c, phi)))
}
