#' @title Bearing capacity
#' @description Calculates and plots the allowable bearing capacity for different footings.
#' @param B The footing's width (in meters)
#' @param D The footing's embedment depth (in meters)
#' @param L The footing's length for a rectangular footing (in meters)
#' @param gamma.h The wet unit weight of the soil (in kN/m3)
#' @param gamma.s The saturated unit weight of the soil (in kN/m3)
#' @param tau0 The soil's cohesion (in kPa)
#' @param phi The soil's friction angle
#' @param wl The depth to the water level (in meters)
#' @param FS The Factor of Safety to use for the calculation of qa
#' @param footing Type of footing for which to calculate the bearing capacity (Default is "strip")
#' @export
#' @return A data frame with the solution (Depth in rows, Width in columns) and the respective plot
#' @import ggplot2
#' @references Day, R. W. (2010). Foundation Engineering Handbook. McGraw Hill.
#' @details The \code{B} and \code{D} parameters can be vectors for multiple cases comparisons or single values for a single case estimates. If  \code{FS = 1} then \code{qa = qu}.
#' For a total stress analysis (TSA) in a cohesive soil (plastic silts and clays) set the friction angle equal to zero (\code{phi = 0}) and the cohesion equal to the undrained shear strength (\code{tau0 = Su}).
#' For an effective stress analysis (ESA) in a cohesive soil use the effective friction angle and effective cohesion.
#' For a coarse-grained soil (gravels, sands, and non-plastic silts) usually TSA = ESA, and the friction angle and cohesion should be used, and if the material has no-cohesion then set cohesion equal to zero (tau0 = 0)
#' @examples
#' B = seq(0.5, 2, 0.25)
#' D = seq(0, 2, 0.25)
#' L = NULL
#' gamma.h = 15.5
#' gamma.s = 18.5
#' tau0 = 10
#' phi = 30
#' FS = 3
#' wl = 1
#' bearing_capacity(B, D, L, gamma.h, gamma.s, tau0, phi, wl, FS)
#' bearing_capacity(B, D, L, gamma.h, gamma.s, tau0, phi, wl, FS, footing = "square")
#' bearing_capacity(B, D, L = 3, gamma.h, gamma.s, tau0, phi, wl, FS, footing = "rectangular")
#'
bearing_capacity = function(B, D, L = NULL, gamma.h, gamma.s, tau0, phi, wl, FS, footing = c("strip", "square", "rectangular","circular")) {

  Ng = geotech::Ngamma(phi)
  Nq = geotech::Nq(phi)
  Nc = geotech::Nc(phi)

  sc = ifelse(any(footing == "strip"), 1, ifelse(footing == "rectangular", 1 + 0.3 * (B / L), 1.3))
  sg = ifelse(any(footing == "strip"), 1, ifelse(footing == "cicrular", 0.6, 0.8))

  gamma.b = gamma.s - 9.807

  BD = expand.grid(B = B, D = D)

  for (i in 1:nrow(BD)) {
    if (wl >= (BD$D[i] + BD$B[i])) {
      BD$qu[i] = 0.5 * BD$B[i] * Ng * gamma.h * sg + BD$D[i] * Nq * gamma.h + tau0 * Nc * sc
    } else if (wl >= BD$D[i] & wl < (BD$D[i] + BD$B[i])) {
      BD$qu[i] = 0.5 * BD$B[i] * Ng * gamma.b * sg + BD$D[i] * Nq * gamma.h + tau0 * Nc * sc
    } else if (wl < BD$D[i]) {
      BD$qu[i] = 0.5 * BD$B[i] * Ng * gamma.b * sg + BD$D[i] * Nq * gamma.b + tau0 * Nc * sc
    }
  }

  BD$qa = BD$qu / FS

  BD_spread = BD %>%
    dplyr::select(B, D, qa) %>%
    tidyr::pivot_wider(names_from = B, values_from = qa)
  noms = names(BD_spread)
  names(BD_spread) = c('D/B',noms[2:length(noms)])

  if (length(B) > 1 & length(D) > 1) {
   q = ggplot(BD, aes(D, qa, group = B, col = as.factor(B))) +
    geom_line() +
    geom_point() +
    labs(x = "Depth, D (m)", y = "Allowable bearing capacity, qa (kPa)", col = "Width, B (m)") +
    theme_bw() + ggtitle(paste("Bearing capacity for a", footing, "footing with a FS =", FS)) +
     theme(axis.title = element_text(size = 16),
           axis.text = element_text(size = 14),
           legend.text = element_text(size = 12))
  } else if (length(B) > 1 & length(D) == 1) {
    q = ggplot(BD, aes(B, qa, col = as.factor(D))) +
      geom_line() +
      geom_point() +
      labs(x = "Width, B (m)", y = "Allowable bearing capacity, qa (kPa)", col = "Depth, D (m)") +
      theme_bw() + ggtitle(paste("Bearing capacity for a", footing, "footing with a FS =", FS)) +
      theme(axis.title = element_text(size = 16),
            axis.text = element_text(size = 14),
            legend.text = element_text(size = 12))
  } else if (length(B) == 1 & length(D) > 1) {
    q = ggplot(BD, aes(D, qa, col = as.factor(B))) +
      geom_line() +
      geom_point() +
      labs(x = "Depth, D (m)", y = "Allowable bearing capacity, qa (kPa)", col = "Width, B (m)") +
      theme_bw() + ggtitle(paste("Bearing capacity for a", footing, "footing with a FS =", FS)) +
      theme(axis.title = element_text(size = 16),
            axis.text = element_text(size = 14),
            legend.text = element_text(size = 12))
  } else {
    q = ggplot(BD, aes(D, qa, col = as.factor(B))) +
      geom_point() +
      labs(x = "Depth, D (m)", y = "Allowable bearing capacity, qa (kPa)", col = "Width, B (m)") +
      theme_bw() + ggtitle(paste("Bearing capacity for a", footing, "footing with a FS =", FS)) +
      theme(axis.title = element_text(size = 16),
            axis.text = element_text(size = 14),
            legend.text = element_text(size = 12))
  }
  return(list(Result = round(BD_spread, 2), Plot = q))
}
