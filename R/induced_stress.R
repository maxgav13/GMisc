#' @title Induced stress on a surface
#' @description Plots the stress distribution under the center of a footing of different shapes.
#' @param qs The stress applied by the footing onto the surface
#' @param B The footing's width (in meters)
#' @param L The footing's length for a rectangular footing (in meters)
#' @param z.end The depth to which calculate the stresses (in meters)
#' @param footing Type of footing for which to calculate the stresses (Default is "strip")
#' @export
#' @return A stress distribution plot for the desired footing
#' @import stats
#' @import geotech
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import plotly
#' @import pracma
#' @references Holtz, R. D., Kovacs, W. D. & Sheahan, T. C. (2011). An Introduction to Geotechnical Engineering. Prentice Hall.
#' @details The stresses are calculated using the solutions from Boussinesq. The parameter \code{L} only applies for rectangular footings, otherwise is not considered, thus the default value of \code{NULL}. For rectangular footings the length (\code{L}) can be varied, while for the other footing shapes the width \code{B} can be varied. For circular footings the width is equal to the radius (\code{B=R})
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
#' L = c(1, 2, 4)
#' z.end = 8
#' induced_stress(qs, B, L, z.end, footing = "rectangular")
#'
induced_stress = function(qs, B, L = NULL, z.end, footing = c("strip", "square", "rectangular", "circular")) {
  z = seq(0.01, z.end, .01)

  # square footing
  a.z = c(0,.2,.4,.6,.8,1,1.2,1.4,1.6,1.8,2,2.4,2.8,3.2,3.6,4,5,6,8,10,12,16,20,Inf)
  I.az = c(0,.0188,.0716,.1494,.2410,.336,.4276,.5108,.5844,.6476,.7008,.7832,.8408,.8812,.9096,.93,.9604,.9756,.9892,.9944,.9968,.9984,.9992,1)

  # strip footing
  b.z = c(0,.1,.2,.5,.8,1,1.2,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,8,9,10,100,Inf)
  I.bz = c(0,.064,.127,.306,.462,.55,.624,.716,.817,.889,.92,.943,.96,.97,.977,.983,.986,.989,.991,.994,.996,.997,1,1)

  if (any(footing == "rectangular") & is.null(L) == F) {
    B = B / 2
    L = L / 2

    M = B / z
    N = list()
    if (length(L) == 1) {
      N = L / z
    } else {
      for (i in 1:length(L)) {
        N[[i]] = L[i] / z
      }
    }

    I.z = list()
    if (length(L) == 1) {
      I.z = 1/(2*pi) * ((M*N)/sqrt(M^2 + N^2 + 1) * (1/(M^2+1)+(1/(N^2+1))) + atan((M*N)/sqrt(M^2 + N^2 + 1)))
    } else {
      for (i in 1:length(L)) {
        I.z[[i]] = 1/(2*pi) * ((M*N[[i]])/sqrt(M^2 + N[[i]]^2 + 1) * (1/(M^2+1)+(1/(N[[i]]^2+1))) + atan((M*N[[i]])/sqrt(M^2 + N[[i]]^2 + 1)))
      }
    }

    sig.z = list()
    if (length(L) == 1) {
      sig.z = qs * I.z * 4
    } else {
      for (i in 1:length(L)) {
        sig.z[[i]] = qs * I.z[[i]] * 4
      }
    }
    if (length(L) > 1) {
      names(sig.z) = paste("L=",L*2, sep = "")
    }

    if (length(L) == 1) {
      sz = data.frame(sig.z = sig.z, d = z, L = L)
    } else {
      sz = bind_rows(sig.z)
      sz = data.frame(sz, d = z)
      names(sz)[1:length(L)] = paste("L=",L*2, sep = "")
      sz = sz %>% gather(L,sig.z,-d)
    }

    if (length(L) > 1) {
      q = ggplot(sz, aes(sig.z, d, col = L)) +
        geom_line() +
        scale_y_reverse() +
        theme_bw() +
        labs(x = "Stress [kPa]", y = "Depth [m]", col = "Length [m]") +
        ggtitle(paste("Induced stress under a", footing, "footing "))
    } else {
      q = ggplot(sz, aes(sig.z, d, col = as.factor(L))) +
        geom_line() +
        scale_y_reverse() +
        theme_bw() +
        labs(x = "Stress [kPa]", y = "Depth [m]", col = "Length [m]") +
        ggtitle(paste("Induced stress under a", footing, "footing "))
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
      if (any(footing == "strip")) {
        I.z = interp1(b.z, I.bz, M, method = method)
      } else if (footing == "square") {
        I.z = interp1(a.z, I.az, M, method = method)
      } else if (footing == "circular") {
        I.z = 1 - (1 / (1 + (M)^2))^(3/2)
      }
    } else {
      for (i in 1:length(B)) {
        if (any(footing == "strip")) {
          I.z[[i]] = interp1(b.z, I.bz, M[[i]], method = method)
        } else if (footing == "square") {
          I.z[[i]] = interp1(a.z, I.az, M[[i]], method = method)
        } else if (footing == "circular") {
          I.z[[i]] = 1 - (1 / (1 + (M[[i]])^2))^(3/2)
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
      names(sig.z) = paste("B=",B, sep = "")
    }

    if (length(B) == 1) {
      sz = data.frame(sig.z = sig.z, d = z, B = B)
    } else {
      sz = bind_rows(sig.z)
      sz = data.frame(sz, d = z)
      names(sz)[1:length(B)] = paste("B=",B, sep = "")
      sz = sz %>% gather(B,sig.z,-d)
    }

    if (any(footing == "strip")) {
      texto = "Width [m]"
    } else if (footing == "square") {
      texto = "Width [m]"
    } else if (footing == "circular") {
      texto = "Radius [m]"
    }
    if (length(B) > 1) {
      q = ggplot(sz, aes(sig.z, d, col = B)) +
        geom_line() +
        scale_y_reverse() +
        theme_bw() +
        labs(x = "Stress [kPa]", y = "Depth [m]", col = texto)  +
        ggtitle(paste("Induced stress under a", footing, "footing "))
    } else {
      q = ggplot(sz, aes(sig.z, d, col = as.factor(B))) +
        geom_line() +
        scale_y_reverse() +
        theme_bw() +
        labs(x = "Stress [kPa]", y = "Depth [m]", col = texto) +
        ggtitle(paste("Induced stress under a", footing, "footing "))
    }
  }
  return(list(Plot = q, Plotly = ggplotly(q)))
}
