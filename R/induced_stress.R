#' @title Induced stress on a surface
#' @description Plots the stress distribution under the center of a footing of different shapes.
#' @param qs The stress applied by the footing onto the surface
#' @param B The footing's width (in meters)
#' @param L The footing's length for a rectangular footing (in meters)
#' @param z.end The depth to which calculate the stresses (in meters)
#' @param footing Type of footing for which to calculate the stresses (Default is "strip")
#' @param rect.pos For rectangular or square footings, whether the stress is calculated at the center of the footing (Default is "center") or at the corner of the footing (i.e., at the edge of the footing) (rect.pos = "corner")
#' @export
#' @return A list with three elements: \code{Profile} is a tibble with the stress distribution, \code{Plot} is a ggplot object with the stress distribution, and \code{Plotly} is an interactive plotly object with the stress distribution.
#' @import ggplot2
#' @references Holtz, R. D., Kovacs, W. D. & Sheahan, T. C. (2011). An Introduction to Geotechnical Engineering. Prentice Hall.
#' @references Budhu, M. (2011). Soil mechanics and foundations (3rd ed). Wiley.
#' @references Das, B. M., & Sobhan, K. (2018). Principles of geotechnical engineering (Ninth edition). Cengage Learning.
#' @details The stresses are calculated using the solutions from Boussinesq. The parameter \code{L} only applies for rectangular footings, otherwise is not considered, thus the default value of \code{NULL}. For rectangular footings the length (\code{L}) can be varied, while for the other footing shapes the width \code{B} can be varied. For circular footings the width is equal to the radius (\code{B=R})
#' @family soil mechanics
#' @examples
#' # Singular value of B for a strip footing
#' qs = 1
#' B = 1
#' L = NULL
#' z.end = 8
#' induced_stress(qs, B, L, z.end)
#' # Different values of B for a square footing
#' qs = 1
#' B = c(1, 2, 4)
#' L = NULL
#' z.end = 8
#' induced_stress(qs, B, L, z.end, footing = "square")
#' # Different values of L for a rectangular footing
#' qs = 1
#' B = 1
#' L = c(2, 3, 4)
#' z.end = 8
#' res = induced_stress(qs, B, L, z.end, footing = "rectangular")
#' res$Profile |> dplyr::filter(z==4)
#' res$Plot
#'
induced_stress = function(
  qs,
  B,
  L = NULL,
  z.end,
  footing = c("strip", "square", "rectangular", "circular"),
  rect.pos = c("center", "corner")
) {
  footing = match.arg(footing)
  rect.pos = match.arg(rect.pos)

  if (footing == "rectangular" && is.null(L)) {
    stop("For rectangular footings, the length (L) must be provided.")
  }

  z = seq(0.1, z.end, .1)
  rect.fct = ifelse(rect.pos == "center", 4, 1)

  # square footing
  a.z = c(
    0,
    .2,
    .4,
    .6,
    .8,
    1,
    1.2,
    1.4,
    1.6,
    1.8,
    2,
    2.4,
    2.8,
    3.2,
    3.6,
    4,
    5,
    6,
    8,
    10,
    12,
    16,
    20,
    Inf
  )
  I.az = c(
    0,
    .0188,
    .0716,
    .1494,
    .2410,
    .336,
    .4276,
    .5108,
    .5844,
    .6476,
    .7008,
    .7832,
    .8408,
    .8812,
    .9096,
    .93,
    .9604,
    .9756,
    .9892,
    .9944,
    .9968,
    .9984,
    .9992,
    1
  )

  # strip footing
  b.z = c(
    0,
    .1,
    .2,
    .5,
    .8,
    1,
    1.2,
    1.5,
    2,
    2.5,
    3,
    3.5,
    4,
    4.5,
    5,
    5.5,
    6,
    6.5,
    7,
    8,
    9,
    10,
    100,
    Inf
  )
  I.bz = c(
    0,
    .064,
    .127,
    .306,
    .462,
    .55,
    .624,
    .716,
    .817,
    .889,
    .92,
    .943,
    .96,
    .97,
    .977,
    .983,
    .986,
    .989,
    .991,
    .994,
    .996,
    .997,
    1,
    1
  )

  if (footing == "rectangular" || footing == "square") {
    if (footing == "square") {
      L = B
    }
    if (rect.pos == "center") {
      B = B / 2
      L = L / 2
    } else if (rect.pos == "corner") {
      B = B
      L = L
    }

    if (footing == "rectangular") {
      M = B / z
    } else if (footing == "square") {
      M = list()
    }

    N = list()
    if (length(L) == 1) {
      M = B / z
      N = L / z
    } else {
      for (i in 1:length(L)) {
        if (footing == "rectangular") {
          M = B / z
        } else if (footing == "square") {
          M[[i]] = L[i] / z
        }
        N[[i]] = L[i] / z
      }
    }

    I.z = list()

    if (length(L) == 1) {
      I.z = 1 /
        (2 * pi) *
        ((M * N) /
          sqrt(M^2 + N^2 + 1) *
          (1 / (M^2 + 1) + (1 / (N^2 + 1))) +
          atan((M * N) / sqrt(M^2 + N^2 + 1)))
    } else {
      if (footing == "rectangular") {
        for (i in 1:length(L)) {
          I.z[[i]] = 1 /
            (2 * pi) *
            ((M * N[[i]]) /
              sqrt(M^2 + N[[i]]^2 + 1) *
              (1 / (M^2 + 1) + (1 / (N[[i]]^2 + 1))) +
              atan((M * N[[i]]) / sqrt(M^2 + N[[i]]^2 + 1)))
        }
      } else if (footing == "square") {
        for (i in 1:length(L)) {
          I.z[[i]] = 1 /
            (2 * pi) *
            ((M[[i]] * N[[i]]) /
              sqrt(M[[i]]^2 + N[[i]]^2 + 1) *
              (1 / (M[[i]]^2 + 1) + (1 / (N[[i]]^2 + 1))) +
              atan((M[[i]] * N[[i]]) / sqrt(M[[i]]^2 + N[[i]]^2 + 1)))
        }
      }
    }

    sig.z = list()
    if (length(L) == 1) {
      sig.z = qs * I.z * rect.fct
    } else {
      for (i in 1:length(L)) {
        sig.z[[i]] = qs * I.z[[i]] * rect.fct
      }
    }

    if (footing == "rectangular") {
      col.title = "Length (m)"
      name.txt = "L="
      rect.title = ifelse(
        rect.pos == "center",
        paste("Induced stress under the center of a rectangular footing"),
        paste("Induced stress under the corner of a rectangular footing")
      )
    } else if (footing == "square") {
      col.title = "Width (m)"
      name.txt = "B="
      rect.title = ifelse(
        rect.pos == "center",
        paste("Induced stress under the center of a square footing"),
        paste("Induced stress under the corner of a square footing")
      )
    }

    if (length(L) > 1) {
      if (rect.pos == "center") {
        names(sig.z) = paste(name.txt, L * 2, sep = "")
      } else if (rect.pos == "corner") {
        names(sig.z) = paste(name.txt, L, sep = "")
      }
    }

    if (length(L) == 1) {
      if (rect.pos == "center") {
        sz = data.frame(sig.z = sig.z, z = z, L = L * 2)
      } else if (rect.pos == "corner") {
        sz = data.frame(sig.z = sig.z, z = z, L = L)
      }
    } else {
      sz = dplyr::bind_rows(sig.z)
      sz = data.frame(sz, z = z)
      if (rect.pos == "center") {
        names(sz)[1:length(L)] = paste(name.txt, L * 2, sep = "")
      } else if (rect.pos == "corner") {
        names(sz)[1:length(L)] = paste(name.txt, L, sep = "")
      }
      sz = tidyr::pivot_longer(
        sz,
        cols = -.data$z,
        names_to = "L",
        values_to = 'sig.z'
      )
    }

    if (length(L) > 1) {
      q = ggplot(sz, aes(.data$sig.z, .data$z, col = .data$L)) +
        geom_line() +
        scale_y_reverse(limits = c(0, z.end)) +
        theme_bw() +
        labs(x = "Stress (kPa)", y = "Depth (m)", col = col.title) +
        ggtitle(rect.title) +
        theme(
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          legend.text = element_text(size = 12)
        )
    } else {
      q = ggplot(sz, aes(.data$sig.z, .data$z, col = as.factor(.data$L))) +
        geom_line() +
        scale_y_reverse(limits = c(0, z.end)) +
        theme_bw() +
        labs(x = "Stress (kPa)", y = "Depth (m)", col = col.title) +
        ggtitle(rect.title) +
        theme(
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          legend.text = element_text(size = 12)
        )
    }
  } else {
    M = list()
    if (length(B) == 1) {
      M = B / z
    } else {
      for (i in 1:length(B)) {
        M[[i]] = B[i] / z
      }
    }

    method = "linear"
    I.z = list()
    if (length(B) == 1) {
      if (footing == "strip") {
        # I.z = pracma::interp1(b.z, I.bz, M, method = method)
        x = 0
        I.z = 1 /
          pi *
          (atan(z / (x - B / 2)) +
            pi -
            atan(z / (x + B / 2)) -
            (B * z * (x^2 - z^2 - (B^2 / 4))) /
              ((x^2 + z^2 - (B^2 / 4))^2 + B^2 * z^2))
        # } else if (footing == "square") {
        #   I.z = pracma::interp1(a.z, I.az, M, method = method)
      } else if (footing == "circular") {
        I.z = 1 - (1 / (1 + (M)^2))^(3 / 2)
      }
    } else {
      for (i in 1:length(B)) {
        if (footing == "strip") {
          # I.z[[i]] = pracma::interp1(b.z, I.bz, M[[i]], method = method)
          x = 0
          I.z[[i]] = 1 /
            pi *
            (atan(z / (x - B[[i]] / 2)) +
              pi -
              atan(z / (x + B[[i]] / 2)) -
              (B[[i]] * z * (x^2 - z^2 - (B[[i]]^2 / 4))) /
                ((x^2 + z^2 - (B[[i]]^2 / 4))^2 + B[[i]]^2 * z^2))
          # } else if (footing == "square") {
          #   I.z[[i]] = pracma::interp1(a.z, I.az, M[[i]], method = method)
        } else if (footing == "circular") {
          I.z[[i]] = 1 - (1 / (1 + (M[[i]])^2))^(3 / 2)
        }
      }
    }

    sig.z = list()
    if (length(B) == 1) {
      sig.z = qs * I.z
    } else {
      for (i in 1:length(B)) {
        sig.z[[i]] = qs * I.z[[i]]
      }
    }
    if (length(B) > 1) {
      names(sig.z) = paste("B=", B, sep = "")
    }

    if (length(B) == 1) {
      sz = data.frame(sig.z = sig.z, z = z, B = B)
    } else {
      sz = dplyr::bind_rows(sig.z)
      sz = data.frame(sz, z = z)
      names(sz)[1:length(B)] = paste("B=", B, sep = "")
      sz = tidyr::pivot_longer(
        sz,
        cols = -.data$z,
        names_to = "B",
        values_to = 'sig.z'
      )
    }

    if (footing == "strip") {
      texto = "Width (m)"
      # } else if (footing == "square") {
      #   texto = "Width (m)"
    } else if (footing == "circular") {
      texto = "Radius (m)"
    }
    if (length(B) > 1) {
      q = ggplot(sz, aes(.data$sig.z, .data$z, col = .data$B)) +
        geom_line() +
        scale_y_reverse(limits = c(0, z.end)) +
        theme_bw() +
        labs(x = "Stress (kPa)", y = "Depth (m)", col = texto) +
        ggtitle(paste("Induced stress under a", footing[1], "footing ")) +
        theme(
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12)
        )
    } else {
      q = ggplot(sz, aes(.data$sig.z, .data$z, col = as.factor(.data$B))) +
        geom_line() +
        scale_y_reverse(limits = c(0, z.end)) +
        theme_bw() +
        labs(x = "Stress (kPa)", y = "Depth (m)", col = texto) +
        ggtitle(paste("Induced stress under a", footing[1], "footing ")) +
        theme(
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12)
        )
    }
  }
  return(list(
    Profile = tibble::tibble(sz),
    Plot = q,
    Plotly = plotly::ggplotly(q, dynamicTicks = T)
  ))
}
