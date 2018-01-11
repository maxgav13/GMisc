#' @title Hoek-Brown criterion
#' @description Calculates the Hoek-Brown criterion for the given data and for the given level of stress (unit weight and depth).
#' @param sig.ci The unconfined compressive strength of the intact rock, in MPa
#' @param GSI The Geological Strength Index
#' @param mi The intact rock constant
#' @param MR the modulus ratio
#' @param D The quality of the explosive campaing design
#' @param height The height or depth for which to determine the stress level and parameters
#' @param unit.weight The unit weight of the rock, in kN/m3
#' @param use The use expected for the rock mass (general, tunnels, or slopes)
#' @details Tables for some of the input parameters can be found in the directory where the package is installed, just run the following command to find this path: \code{system.file(package = "GMisc")}
#' @export
#' @references Hoek, E. & Brown, E. T. (1980). Empirical strength criterion for rock masses. J. Geotech. Eng. Div. ASCE 106 (GT9): 1013-1035.
#' @references Hoek, E., Carranza-Torres, C. & Corkum, B. (2002). Hoek-Brown Failure Criterion - 2002 Edition. Proc. NARMS-TAC Conference, Toronto. 267-273.
#' @references Hoek, E. & Marinos, P. (2007). A brief history of the development of the Hoek-Brown criterion. Soils and Rocks, No. 2.
#' @return A list with a list of the resulting parameters of the rock mass and 3 data frames: Shear envelopes for Mohr-Coulomb and Hoek-Brown criteria, Principal stresses envelopes for Mohr-Coulomb and Hoek-Brown criteria, and stress level for the given height and unit weight
#' @import stats
#' @examples
#' sig.ci = 16
#' GSI = 75
#' mi = 13
#' MR = 300
#' D = 0
#' height = 40
#' unit.weight = 18.6
#' Hoek_Brown(sig.ci, GSI, mi, MR, D, height, unit.weight)
#'
Hoek_Brown = function(sig.ci, GSI, mi, MR, D, height, unit.weight, use = c("general", "tunnels", "slopes")) {

  unit.weight = unit.weight / 1000

  mb = signif(mi * exp((GSI - 100) / (28 - 14 * D)), 3)
  s = signif(exp((GSI - 100) / (9 - 3 * D)), 3)
  a = signif(.5 + 1/6 * (exp(- GSI / 15) - exp(- 20 / 3)), 3)

  sig.c = signif(sig.ci * s ^ a, 3)
  sig.t = signif(- (s * sig.ci) / mb, 3)
  Ei = sig.ci * MR
  Erm = signif(Ei * (0.02 + ((1 - D/2) / (1 + exp((60 + 15 * D - GSI) / 11)))), 6)
  sig.cm = signif(sig.ci * (((mb + 4 * s - a * (mb - 8 * s)) * ((mb / (4 + s)) ^ (a - 1)) / (2 * (1 + a) * (2 + a)))), 3)

  if (any(use == "general")) {
    sig.3max = sig.ci / 4
  } else if (use == "slopes") {
    sig.3max = ((sig.cm / (unit.weight * height)) ^ -0.91) * sig.cm * 0.72
  } else if (use == "tunnels") {
    sig.3max = ((sig.cm / (unit.weight * height)) ^ -0.94) * sig.cm * 0.47
  }
  sig.3max = signif(sig.3max, 3)
  sig.3n = signif(sig.3max / sig.ci, 3)
  sig.3 = seq(sig.t, sig.3max, .001)

  phi = signif(degs(asin(((6 * a * mb) * ((s + mb * sig.3n) ^ (a - 1))) /
                          ((2 * (1 + a) * (2 + a)) + (6 * a * mb) * ((s + mb * sig.3n) ^ (a - 1))))), 3)
  c = signif((sig.ci * ((1 + 2 * a) * s + (1 - a) * mb * sig.3n) * ((s + mb * sig.3n) ^ (a - 1)))/
               ((1 + a) * (2 + a) * sqrt((1 + ((6 * a * mb) * ((s + mb * sig.3n) ^ (a - 1))))/((1 + a) * (2 + a)))), 3)

  sig.1n = signif((2 * c * cos(rads(phi))) / (1 - sin(rads(phi))) + (1 + sin(rads(phi))) / (1 - sin(rads(phi))) * sig.3n, 3)


  sig.n.inst = signif(unit.weight * height, 3)
  h.inst = (1 + ((16 * (mb * sig.n.inst + s * sig.ci)) / (3 * mb ^ 2 * sig.ci)))
  theta.inst = (1/3) * (90 + degs(atan((1 / (sqrt(h.inst ^ 3 - 1))))))
  phi.inst = signif(degs(atan(1 / (sqrt(4 * h.inst * (cos(rads(theta.inst))) ^ 2 - 1)))), 3)
  tau.inst = signif((((1 / tan(rads(phi.inst))) - cos(rads(phi.inst))) * ((mb * sig.ci) / 8)), 3)
  c.inst = signif(tau.inst - sig.n.inst * tan(rads(phi.inst)), 3)

  if (any(use == "general")) {
    sig.n = seq(sig.t, sig.3max, .001)
  } else {
    sig.n = seq(sig.t, 3 * sig.n.inst, .001)
  }
  tau.MC = c + sig.n * tan(rads(phi))
  tau.MC.inst = c.inst + sig.n * tan(rads(phi.inst))
  tau.MC[1] = 0
  tau.MC.inst[1] = 0

  h = (1 + ((16 * (mb * sig.n + s * sig.ci)) / (3 * mb ^ 2 * sig.ci)))
  theta = (1/3) * (90 + degs(atan((1 / (sqrt(h ^ 3 - 1))))))
  phi.HB = degs(atan(1 / (sqrt(4 * h * (cos(rads(theta))) ^ 2 - 1))))
  tau.HB = (((1 / tan(rads(phi.HB))) - cos(rads(phi.HB))) * ((mb * sig.ci) / 8))

  sig.1.HB = sig.3 + sig.ci * (mb * sig.3 / sig.ci + s) ^ a
  sig.1.MC = (2 * c * cos(rads(phi))) / (1 - sin(rads(phi))) + (1 + sin(rads(phi))) / (1 - sin(rads(phi))) * sig.3
  sig.1.HB[1] = 0
  sig.1.MC[1] = 0

  MC = data.frame(sig.n, tau.HB, tau.MC, tau.MC.inst)
  names(MC) = c("sig.n", "Hoek-Brown", "Mohr-Coulomb", "Stress level")
  MC.tidy = gather(MC, key = "crit", value = "tau", -sig.n)

  prin.stress = data.frame(sig.3, sig.1.HB, sig.1.MC)
  names(prin.stress) = c("sig.3", "Hoek-Brown", "Mohr-Coulomb")
  prin.stress.tidy = gather(prin.stress, key = "crit", value = "sig.1", -sig.3)

  stress.level = data.frame(sig.n.inst, tau.inst, crit = "Stress level")
  names(stress.level) = c("sign", "tau", "crit")

  lista = list(Results = list(sig.ci = sig.ci, GSI = GSI, mi = mi, D = D, mb = mb, s = s, a = a, MR = MR, Ei = Ei,
                              sig.3.max = sig.3max, sig.t = sig.t, sig.c = sig.c, sig.cm = sig.cm, Erm = Erm,
                              phi = phi, c = c, phi.h = phi.inst, c.h = c.inst),
               shear = MC.tidy,
               princ.stress = prin.stress.tidy,
               stress.level = stress.level)

  return(lista)
}
