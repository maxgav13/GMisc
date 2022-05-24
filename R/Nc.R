#' Bearing capacity factor Nc
#' @description Taken from "geotech" package. Calculate the bearing capacity factor Nc using either the Terzaghi or Vesic methods.
#' @param phi Friction angle in degrees
#' @param case "general" or "local" to indicate general or local shear failure ("general" is default)
#' @param method "Terzaghi" or "Vesic" ("Terzaghi" is default)
#'
#' @return Bearing capacity factor
#' @export
#'
#' @references Coduto, D.P., Kitch, W.A., and Yeung, M.R. (2016). Foundation Design: Principles and Practices, Pearson, Boston.
#' @references Terzaghi, K. (1943). Theoretical Soil Mechanics, John Wiley, New York
#' @references Vesic, A.S. (1973). Analysis of Ultimate Loads of Shallow Foundations, ASCE Journal of the Soil Mechanics and Foundations Division, Vol. 99, No. SM1, pp. 45-73.
#'
#' @examples
#' Nc(phi = 20, case = "local", method = "Terzaghi")
#'
Nc = function (phi, case = "general", method = "Terzaghi")
{
  if (case != "general" && case != "local")
    stop("Case may only be 'general' or 'local'.")
  if (method != "Terzaghi" && method != "Vesic")
    stop("Case may only be 'Terzaghi' or 'Vesic'.")
  if (case == "local")
    phi <- atan(2/3 * tan(phi * pi/180)) * 180/pi
  phi.deg <- phi
  phi.rad <- phi * pi/180
  if (method == "Terzaghi") {
    Nq.value <- Nq(phi = phi.deg, case = "general", method = "Terzaghi")
    if (phi.deg == 0)
      return(5.7)
    if (phi.deg > 0)
      return((Nq.value - 1)/tan(phi.rad))
  }
  else {
    if (method == "Vesic") {
      Nq.value <- Nq(phi = phi.deg, case = "general", method = "Vesic")
      if (phi.deg == 0)
        return(5.14)
      if (phi.deg > 0)
        return((Nq.value - 1)/tan(phi.rad))
    }
  }
}
