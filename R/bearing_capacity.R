#' @title Bearing capacity
#' @description Calculates the ultimate and allowable bearing capacity of a shallow foundation using the general bearing capacity equation, and plots the relationship between depth and allowable bearing capacity for different footing widths and embedment depths.
#' @param B The footing's width or diameter
#' @param D The footing's embedment depth
#' @param Lf Length factor for the footing
#' @param gamma.h The wet/moist unit weight of the soil
#' @param gamma.s The saturated unit weight of the soil
#' @param tau0 The soil's cohesion (\eqn{c, \tau_0}) or undrained shear strength (\eqn{s_u})
#' @param phi The soil's friction angle (\eqn{\phi}) (in degrees)
#' @param wl The depth to the water level from the ground surface
#' @param FS The Factor of Safety to use for the calculation of \eqn{q_a}
#' @param footing Type of footing for which to calculate the bearing capacity: "square" (default), "strip", "rectangular", "circular"
#' @param method The method to use to calculate the bearing capacity factor \eqn{N_\gamma}: "Vesic" (default), "Terzaghi", "Meyerhof", "Hansen", "Michalowski" or "Davis & Booker"
#' @param case The case to calculate the bearing capacity: "general" (default) or "local"
#' @param metric Logical. If \code{TRUE} (default) the function will assume SI units (kN/m3 for unit weight, kPa for strength parameters, and meters for dimensions). If \code{FALSE}, the function will assume imperial units (pcf for unit weight, psf for strength parameters, and feet for dimensions).
#'
#' @export
#' @return A list with three elements: "qu" is a data frame with the calculated ultimate bearing capacity for each combination of \code{B} and \code{D}, "qa" is a data frame with the calculated allowable bearing capacity for each combination of \code{B} and \code{D}, and "Plot" is a ggplot object showing the relationship between \code{D} and \code{qa} for different values of \code{B}.
#' @import ggplot2
#'
#' @references Budhu, M. (2011). Soil mechanics and foundations (3rd ed). Wiley.
#' @references Day, R. W. (2010). Foundation Engineering Handbook. McGraw Hill.
#'
#' @details The \code{B} and \code{D} parameters can be vectors for multiple cases comparisons or single values for a single case estimates. If \code{FS = 1} then \eqn{q_a = q_u}.
#' For a total stress analysis (TSA) in a cohesive soil (plastic silts and clays) set the friction angle equal to zero (\eqn{\phi = 0}) and the cohesion equal to the undrained shear strength (\eqn{\tau_0 = s_u}).
#' For an effective stress analysis (ESA) in a cohesive soil use the effective friction angle and effective cohesion.
#' For a coarse-grained soil (gravels, sands, and non-plastic silts) usually TSA = ESA, and the friction angle and cohesion should be used, and if the material has no-cohesion then set cohesion equal to zero (\eqn{\tau_0 = 0})
#'
#' The general bearing capacity is calculated using the following equation:
#'
#' \deqn{q_u = \tau_0 N_c s_c d_c + \gamma D \left(N_q - 1\right) s_q d_q + 0.5 B * N_\gamma \gamma s_\gamma d_\gamma}
#' Where:
#' \eqn{N_c}, \eqn{N_q}, and \eqn{N_\gamma} are the bearing capacity factors, which are functions of the soil's friction angle (\eqn{\phi}) and the method used to calculate them,
#' \eqn{s_c}, \eqn{s_q}, and \eqn{s_\gamma} are the shape factors, which depend on the footing type and dimensions,
#' \eqn{d_c}, \eqn{d_q}, and \eqn{d_\gamma} are the depth factors, which depend on the footing's embedment depth and the water level, and
#' \eqn{\gamma} is the unit weight of the soil, which can be either the wet/moist unit weight (\eqn{\gamma_h}) or the bouyant unit weight (\eqn{\gamma_b = \gamma_s - 9.807}), depending on the depth of the water level relative to the footing.
#'
#' For a TSA, the equation simplifies to:
#'
#' \deqn{q_u = s_u N_c s_c d_c}
#' Where:
#' \eqn{\tau_0 = s_u} is the undrained shear strength of the soil, and
#' \eqn{N_c} is 5.7 for "Terzaghi" and 5.14 for all the others.
#'
#' For an ESA, the equation simplifies to:
#'
#' \deqn{q_u = \gamma D \left(N_q - 1\right) s_q d_q + 0.5 B * N_\gamma \gamma s_\gamma d_\gamma}
#'
#' The allowable bearing capacity (\eqn{q_a}) is then calculated by dividing the ultimate bearing capacity (\eqn{q_u}) by the Factor of Safety (\eqn{FS}):
#'
#' \deqn{q_a = \frac{q_u}{FS} + \gamma D}
#'
#'
#' @family soil mechanics
#' @examples
#' B = seq(0.5, 2, 0.25)
#' D = seq(0, 2, 0.25)
#' Lf = 1
#' gamma.h = 15.5
#' gamma.s = 18.5
#' tau0 = 10
#' phi = 30
#' FS = 3
#' wl = 1
#' bearing_capacity(B, D, Lf, gamma.h, gamma.s, tau0, phi, wl, FS)
#' bearing_capacity(B, D, Lf, gamma.h, gamma.s, tau0, phi, wl, FS, footing = "strip")
#' bearing_capacity(B, D, Lf = 3, gamma.h, gamma.s, tau0, phi, wl, FS,
#'                  footing = "rectangular", method = "Meyerhof")
#'
bearing_capacity = function(
  B,
  D,
  Lf = 1,
  gamma.h,
  gamma.s,
  tau0,
  phi,
  wl,
  FS,
  footing = c("square", "strip", "rectangular", "circular"),
  method = c(
    "Vesic",
    "Terzaghi",
    "Meyerhof",
    "Hansen",
    "Michalowski",
    "Davis & Booker"
  ),
  case = c("general", "local"),
  metric = TRUE
) {
  footing <- match.arg(footing)
  method <- match.arg(method)
  case <- match.arg(case)

  phi = ifelse(
    case == 'general',
    phi,
    atan(2 / 3 * tan(phi * pi / 180)) * 180 / pi
  )
  tau0 = ifelse(case == 'general', tau0, 2 / 3 * tau0)

  phi.rad = phi * pi / 180

  gamma.w = ifelse(metric, 9.81, 62.4)
  gamma.b = gamma.s - gamma.w

  Nq = ifelse(
    method == 'Terzaghi',
    (exp(2 * pi * (3 / 4 - phi / 360) * tan(phi.rad))) /
      (2 * (cos((45 + phi / 2) * pi / 180))^2),
    exp(pi * tan(phi.rad)) * tan(45 * pi / 180 + phi.rad / 2)^2
  )

  Nc = ifelse(
    method == 'Terzaghi' & phi == 0,
    5.7,
    ifelse(method != 'Terzaghi' & phi == 0, 5.14, (Nq - 1) / tan(phi.rad))
  )

  Ng = dplyr::case_when(
    method == 'Terzaghi' ~ (2 * (Nq + 1) * tan(phi.rad)) /
      (1 + 0.4 * sin(4 * phi.rad)),
    method == 'Vesic' ~ 2 * (Nq + 1) * tan(phi.rad),
    method == 'Meyerhof' ~ (Nq - 1) * tan(1.4 * phi.rad),
    method == 'Hansen' ~ 1.5 * (Nq - 1) * tan(phi.rad),
    method == 'Michalowski' ~ exp(0.66 + 5.11 * tan(phi.rad)) * tan(phi.rad),
    method == 'Davis & Booker' ~ .1054 * exp(9.6 * phi.rad)
  )

  # gamma_D = dplyr::case_when(
  #   wl < D ~ gamma.h * wl + gamma.b * (D - wl),
  #   wl >= D & wl < D + B ~ gamma.h * D,
  #   wl >= D + B ~ gamma.h * D
  # )
  # gamma_B = dplyr::case_when(
  #   wl < D ~ gamma.b * B,
  #   wl >= D & wl < D + B ~ gamma.h * (wl - D) + gamma.b * (B - (wl - D)),
  #   wl >= D + B ~ gamma.h * B
  # )

  if (length(D) == 1 & length(B) == 1) {
    L = dplyr::case_when(
      footing == 'rectangular' ~ B * Lf,
      footing == 'strip' ~ Inf,
      .default = B
    )

    if (footing == 'strip') {
      sc = 1
      sq = 1
      sg = 1
    } else {
      sc = 1 + .2 * (B / L)
      sq = 1 + (B / L) * tan(phi.rad)
      sg = 1 - .4 * (B / L)
    }

    dc = ifelse(D / B <= 1, 1 + .33 * (D / B), 1 + .33 * atan(D / B))
    dq = ifelse(
      D / B <= 1,
      1 + 2 * tan(phi.rad) * (1 - sin(phi.rad))^2 * (D / B),
      1 + 2 * tan(phi.rad) * (1 - sin(phi.rad))^2 * atan(D / B)
    )
    dg = 1

    if (wl < D) {
      z = wl - 0
      gamma_D = gamma.h * z + gamma.b * (D - z)
      gamma_B = gamma.b * B
    } else if (wl >= D & wl < D + B) {
      z = wl - D
      gamma_D = gamma.h * D
      gamma_B = gamma.h * z + gamma.b * (B - z)
    } else if (wl >= D + B) {
      gamma_D = gamma.h * D
      gamma_B = gamma.h * B
    }

    qu = Nc *
      tau0 *
      sc *
      dc +
      gamma_D * (Nq - 1) * sq * dq +
      .5 * gamma_B * Ng * sg * dg

    qa = ifelse(
      phi == 0,
      qu / FS + gamma.s * D,
      qu / FS + gamma_D
    )
  }

  bc_full = tidyr::expand_grid(B = B, D = D) |>
    dplyr::mutate(
      L = if (footing == 'rectangular') {
        B * Lf
      } else if (footing == 'strip') {
        Inf
      } else {
        B
      },
      sc = ifelse(
        footing == 'strip',
        1,
        1 + .2 * (B / L)
      ),
      sq = ifelse(
        footing == 'strip',
        1,
        1 + (B / L) * tan(phi.rad)
      ),
      sg = ifelse(
        footing == 'strip',
        1,
        1 - .4 * (B / L)
      ),
      dc = ifelse(D / B <= 1, 1 + .33 * (D / B), 1 + .33 * atan(D / B)),
      dq = ifelse(
        D / B <= 1,
        1 + 2 * tan(phi.rad) * (1 - sin(phi.rad))^2 * (D / B),
        1 + 2 * tan(phi.rad) * (1 - sin(phi.rad))^2 * atan(D / B)
      ),
      dg = 1,
      nf.case = dplyr::case_when(
        wl < D ~ 1,
        wl >= D & wl < D + B ~ 2,
        wl >= D + B ~ 3
      ),
      z = dplyr::case_when(
        nf.case == 1 ~ wl - 0,
        nf.case == 2 ~ wl - D,
        .default = wl
      ),
      gamma_D = dplyr::case_when(
        nf.case == 1 ~ gamma.h * z + gamma.b * (D - z),
        .default = gamma.h * D
      ),
      gamma_B = dplyr::case_when(
        nf.case == 1 ~ gamma.b * B,
        nf.case == 2 ~ gamma.h * z + gamma.b * (B - z),
        .default = gamma.h * B
      ),
      qu = Nc *
        tau0 *
        sc *
        dc +
        gamma_D * (Nq - 1) * sq * dq +
        .5 * gamma_B * Ng * sg * dg,
      qa = if (phi == 0) {
        qu / FS + gamma.s * D
      } else {
        qu / FS + gamma_D
      }
    )

  bc_spread_qa = bc_full |>
    dplyr::select(.data$B, .data$D, .data$qa) |>
    tidyr::pivot_wider(names_from = .data$B, values_from = .data$qa)
  noms = names(bc_spread_qa)
  names(bc_spread_qa) = c('D/B', noms[2:length(noms)])

  bc_spread_qu = bc_full |>
    dplyr::select(.data$B, .data$D, .data$qu) |>
    tidyr::pivot_wider(names_from = .data$B, values_from = .data$qu)
  noms = names(bc_spread_qu)
  names(bc_spread_qu) = c('D/B', noms[2:length(noms)])

  if (length(B) > 1 & length(D) > 1) {
    q = ggplot(
      bc_full,
      aes(.data$D, .data$qa, group = .data$B, col = as.factor(.data$B))
    ) +
      geom_line() +
      geom_point() +
      labs(
        x = ifelse(metric, "Depth, D (m)", "Depth, D (ft)"),
        y = ifelse(
          metric,
          "Allowable bearing capacity, qa (kPa)",
          "Allowable bearing capacity, qa (psf)"
        ),
        col = ifelse(metric, "Width, B (m)", "Width, B (ft)")
      ) +
      theme_bw() +
      ggtitle(paste(
        "Bearing capacity for a",
        footing,
        "footing with a FS =",
        FS
      )) +
      theme(
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 12)
      )
  } else if (length(B) > 1 & length(D) == 1) {
    q = ggplot(bc_full, aes(.data$B, .data$qa, col = as.factor(.data$D))) +
      geom_line() +
      geom_point() +
      labs(
        x = ifelse(metric, "Width, B (m)", "Width, B (ft)"),
        y = ifelse(
          metric,
          "Allowable bearing capacity, qa (kPa)",
          "Allowable bearing capacity, qa (psf)"
        ),
        col = ifelse(metric, "Depth, D (m)", "Depth, D (ft)")
      ) +
      theme_bw() +
      ggtitle(paste(
        "Bearing capacity for a",
        footing,
        "footing with a FS =",
        FS
      )) +
      theme(
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 12)
      )
  } else if (length(B) == 1 & length(D) > 1) {
    q = ggplot(bc_full, aes(.data$D, .data$qa, col = as.factor(.data$B))) +
      geom_line() +
      geom_point() +
      labs(
        x = ifelse(metric, "Depth, D (m)", "Depth, D (ft)"),
        y = ifelse(
          metric,
          "Allowable bearing capacity, qa (kPa)",
          "Allowable bearing capacity, qa (psf)"
        ),
        col = ifelse(metric, "Width, B (m)", "Width, B (ft)")
      ) +
      theme_bw() +
      ggtitle(paste(
        "Bearing capacity for a",
        footing,
        "footing with a FS =",
        FS
      )) +
      theme(
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 12)
      )
  } else {
    q = ggplot(bc_full, aes(.data$D, .data$qa, col = as.factor(.data$B))) +
      geom_point() +
      labs(
        x = ifelse(metric, "Depth, D (m)", "Depth, D (ft)"),
        y = ifelse(
          metric,
          "Allowable bearing capacity, qa (kPa)",
          "Allowable bearing capacity, qa (psf)"
        ),
        col = ifelse(metric, "Width, B (m)", "Width, B (ft)")
      ) +
      theme_bw() +
      ggtitle(paste(
        "Bearing capacity for a",
        footing,
        "footing with a FS =",
        FS
      )) +
      theme(
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 12)
      )
  }
  return(list(
    qu = round(bc_spread_qu, 2),
    qa = round(bc_spread_qa, 2),
    Plot = q
  ))
}
