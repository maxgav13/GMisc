#' @title Get particle size from sieve number
#'
#' @description Converts ASTM sieve numbers to particle sizes in millimetres or inches.
#'
#' @param sieve A numeric vector of ASTM sieve numbers (e.g. 4, 10, 200).
#' @param metric Logical. If \code{TRUE} (default), returns sizes in millimetres; if \code{FALSE}, returns sizes in inches.
#'
#' @returns A numeric vector of particle sizes corresponding to each sieve number.
#' @details This function is taken from the "geotech" package from James Kaklamanos
#'
#' @family soil mechanics, classification
#' @export
#' @examples
#' size.from.sieve(c(4, 10, 40, 200))
#' size.from.sieve(c(4, 10, 200), metric = FALSE)
size.from.sieve = function(sieve, metric = T) {
  sieve.no <- c(
    3,
    2,
    1.5,
    1,
    3 / 4,
    1 / 2,
    3 / 8,
    4,
    8,
    10,
    16,
    20,
    30,
    40,
    50,
    60,
    100,
    140,
    200
  )
  sieve.in <- c(
    3,
    2,
    1.5,
    1,
    3 / 4,
    1 / 2,
    3 / 8,
    0.187,
    0.0929,
    0.0787,
    0.0465,
    0.0335,
    0.0236,
    0.0167,
    0.0118,
    0.00984,
    0.00591,
    0.00417,
    0.00295
  )
  sieve.mm <- c(
    76.2,
    50.8,
    38.1,
    25.4,
    19,
    12.7,
    9.5,
    4.75,
    2.36,
    2,
    1.18,
    0.85,
    0.6,
    0.425,
    0.3,
    0.25,
    0.15,
    0.106,
    0.075
  )
  size <- vector(length = length(sieve))
  if (metric == TRUE) {
    for (i in 1:length(sieve)) {
      size[i] <- sieve.mm[match(sieve[i], sieve.no)]
    }
  } else {
    if (metric == FALSE) {
      for (i in 1:length(sieve)) {
        size[i] <- sieve.in[match(sieve[i], sieve.no)]
      }
    }
  }
  return(size)
}

#' @title Compute uniformity and curvature coefficients from a grain-size distribution
#'
#' @description Calculates the uniformity coefficient and the curvature coefficient from percent passing data. The characteristic diameters can be supplied directly or derived from sieve numbers or particle sizes.
#'
#' @param percent Numeric vector of percent passing values (0–100).
#' @param sieve Numeric vector of ASTM sieve numbers. Used when \code{size} is not
#'   provided. Default \code{NA}.
#' @param size Numeric vector of particle sizes (mm if \code{metric = TRUE}, inches
#'   otherwise). Default \code{NA}.
#' @param D10 Diameter at 10 percent passing. If \code{NA} (default) it is interpolated from
#'   the data.
#' @param D30 Diameter at 30 percent passing. If \code{NA} (default) it is interpolated from
#'   the data.
#' @param D60 Diameter at 60 percent passing. If \code{NA} (default) it is interpolated from
#'   the data.
#' @param metric Logical. If \code{TRUE} (default) sizes are in millimetres.
#'
#' @returns A named list with components \code{Cu} (uniformity coefficient) and \code{Cc}
#' @details This function is taken from the "geotech" package from James Kaklamanos
#'   (curvature coefficient).
#' @family soil mechanics, classification
#' @export
#' @examples
#' grainSize.coefs(
#'   percent = c(0, 10, 30, 60, 90, 100),
#'   size    = c(0.075, 0.15, 0.3, 0.6, 1.18, 2.36)
#' )
grainSize.coefs = function(
  percent,
  sieve = NA,
  size = NA,
  D10 = NA,
  D30 = NA,
  D60 = NA,
  metric = T
) {
  if (is.na(D10) == TRUE && is.na(D30) == TRUE && is.na(D60) == TRUE) {
    if (all(is.na(sieve) == FALSE)) {
      size <- size.from.sieve(sieve = sieve, metric = metric)
    }
    if (all(is.na(size) == FALSE)) {
      D10 <- Dsize(N = 10, size = size, percent = percent)
      D30 <- Dsize(N = 30, size = size, percent = percent)
      D60 <- Dsize(N = 60, size = size, percent = percent)
    }
  }
  Cu <- D60 / D10
  Cc <- D30^2 / (D10 * D60)
  return(list(Cu = Cu, Cc = Cc))
}

#' @title Interpolate a characteristic diameter from a grain-size distribution
#'
#' @description Returns the diameter at which \code{N} percent of the sample (by mass) is finer, using log-linear interpolation. Extrapolation is used when \code{N} falls outside the range of \code{percent}, with a warning.
#'
#' @param N Numeric. The desired percent-finer value (e.g. 10, 30, or 60).
#' @param sieve Numeric vector of ASTM sieve numbers. Used to derive \code{size} when
#'   \code{size} is \code{NA}. Default \code{NA}.
#' @param size Numeric vector of particle sizes corresponding to \code{percent}.
#'   Default \code{NA}.
#' @param percent Numeric vector of percent passing values (0–100).
#' @param metric Logical. If \code{TRUE} (default) \code{size} is in millimetres.
#'
#' @returns A single numeric value: the particle diameter at \code{N} percent passing.
#' @details This function is taken from the "geotech" package from James Kaklamanos
#' @family soil mechanics, classification
#' @export
#' @examples
#' Dsize(
#'   N       = 60,
#'   percent = c(0, 10, 30, 60, 90, 100),
#'   size    = c(0.075, 0.15, 0.3, 0.6, 1.18, 2.36)
#' )
Dsize = function(N, sieve = NA, size = NA, percent, metric = T) {
  if (all(is.na(sieve) == FALSE)) {
    size <- size.from.sieve(sieve = sieve, metric = metric)
  }
  DN <- 10^stats::approx(x = percent, y = log10(size), xout = N)$y
  if (N > max(percent)) {
    delPercent <- sort(percent)[length(percent)] -
      sort(percent)[
        length(percent) -
          1
      ]
    delSize <- log10(sort(size))[length(size)] -
      log10(sort(size))[
        length(size) -
          1
      ]
    slope <- delSize / delPercent
    DN <- 10^(log10(sort(size))[length(size)] +
      slope *
        (N -
          sort(percent)[length(percent)]))
    warning(
      "Desired percent is beyond the range of the data; extrapolation is used."
    )
  } else {
    if (N < min(percent)) {
      delPercent <- sort(percent)[2] - sort(percent)[1]
      delSize <- log10(sort(size))[2] - log10(sort(size))[1]
      slope <- delSize / delPercent
      DN <- 10^(log10(sort(size))[1] -
        slope *
          (sort(percent)[1] -
            N))
      warning(
        "Desired percent is beyond the range of the data; extrapolation is used."
      )
    }
  }
  return(DN)
}

#' @title Estimate gravel, sand, and fines content
#'
#' @description Computes the percentages of gravel (greater than 4.75 mm), sand, and fines (smaller than 0.075 mm) from a grain-size distribution curve.
#'
#' @param sieve Numeric vector of ASTM sieve numbers. Used to derive \code{size} when \code{size} is \code{NA}. Default \code{NA}.
#' @param size Numeric vector of particle sizes. Default \code{NA}.
#' @param percent Numeric vector of percent passing values (0–100).
#' @param metric Logical. If \code{TRUE} (default) \code{size} is in millimetres;
#'   if \code{FALSE} it is in inches.
#'
#' @returns A named list with components:
#'   \describe{
#'     \item{pg}{Percent gravel.}
#'     \item{ps}{Percent sand.}
#'     \item{pf}{Percent fines.}
#'   }
#' @details This function is taken from the "geotech" package from James Kaklamanos
#' @family soil mechanics, classification
#' @export
#' @examples
#' percentComponents(
#'   percent = c(0, 10, 40, 70, 90, 100),
#'   size    = c(0.075, 0.25, 0.6, 2, 4.75, 19)
#' )
percentComponents = function(sieve = NA, size = NA, percent, metric = T) {
  if (all(is.na(sieve) == FALSE)) {
    size <- size.from.sieve(sieve = sieve, metric = metric)
  }
  if (metric == TRUE) {
    pg_pass <- stats::approx(x = size, y = percent, xout = 4.75, rule = 2)$y
    pf <- stats::approx(x = size, y = percent, xout = 0.075, rule = 2)$y
    pg <- 100 - pg_pass
    # pg <- 100 - percent[match(4.75, round(size, 2))]
    # pf <- percent[match(0.075, round(size, 3))]
    ps <- 100 - pg - pf
  } else {
    if (metric == FALSE) {
      pg_pass <- stats::approx(x = size, y = percent, xout = .187, rule = 2)$y
      pf <- stats::approx(x = size, y = percent, xout = 0.003, rule = 2)$y
      pg <- 100 - pg_pass
      # pg <- 100 - percent[match(0.187, round(size, 3))]
      # pf <- percent[match(0.003, round(size, 3))]
      ps <- 100 - pg - pf
    }
  }
  return(list(pg = pg, ps = ps, pf = pf))
}

#' @title Determine the USCS symbol for fine-grained soils
#' @description Classifies silt and clay soils according to the Unified Soil Classification
#' System (USCS / ASTM D2487) using the Atterberg limits and the A-line.
#'
#' @param LL Numeric. Liquid limit (percent).
#' @param PL Numeric. Plastic limit (percent). Default \code{NA}; used to compute \code{PI} when \code{PI} is not provided.
#' @param PI Numeric. Plasticity index (percent). Default \code{NA}; computed as \code{LL - PL} when not supplied.
#'
#' @returns A character string with the USCS group symbol (e.g. \code{"CL"}, \code{"MH"}, \code{"CL-ML"}).
#' @details This function is taken from the "geotech" package from James Kaklamanos
#' @family soil mechanics, classification
#' @export
#' @examples
#' USCS.fine.symbol(LL = 40, PL = 20)
#' USCS.fine.symbol(LL = 60, PI = 25)
USCS.fine.symbol = function(LL, PL = NA, PI = NA) {
  A.line = function(LL) {
    if (LL <= 4) {
      return(LL)
    } else if (LL > 4 && LL <= 25.5) {
      return(4)
    } else if (LL > 25.5) {
      return(0.73 * (LL - 20))
    }
  }

  if (is.na(PI)) {
    PI <- LL - PL
  }
  if (PI > LL) {
    stop("PI cannot be greater than LL")
  }
  if (PI >= A.line(LL)) {
    Letter1 <- "C"
  } else if (PI < A.line(LL)) {
    Letter1 <- "M"
  }
  if (LL >= 50) {
    Letter2 <- "H"
  } else if (LL < 50) {
    Letter2 <- "L"
  }
  symbol <- paste(Letter1, Letter2, sep = "")
  if (symbol == "CL") {
    if (PI <= 7) {
      symbol <- "CL-ML"
    }
  }

  # if (LL == 0) {
  #   symbol <- "M-np"
  # }
  # else {
  #   symbol <- symbol
  # }

  return(symbol)
}

#' @title Determine the USCS symbol for coarse-grained soils
#' @description Classifies gravel and sand soils according to the Unified Soil Classification
#' System (USCS / ASTM D2487) from gradation characteristics and, when fines
#' exceed 5 percent, Atterberg limits.
#'
#' @param pg Numeric. Percent gravel (retained on the No. 4 / 4.75 mm sieve).
#' @param ps Numeric. Percent sand.
#' @param pf Numeric. Percent fines (passing No. 200 / 0.075 mm sieve).
#' @param Cc Numeric. Curvature coefficient.
#' @param Cu Numeric. Uniformity coefficient.
#' @param LL Numeric. Liquid limit (percent). Required when \code{pf} is between 5 and 12, or greater than 12.
#' @param PL Numeric. Plastic limit (percent). Used together with `LL` to derive \code{PI}.
#' @param PI Numeric. Plasticity index (percent). Default \code{NA}; computed from \code{LL} and \code{PL} if not provided.
#'
#' @returns A character string with the USCS group symbol (e.g. \code{"GW"}, \code{"SP-SM"}).
#' @details This function is taken from the "geotech" package from James Kaklamanos
#' @family soil mechanics, classification
#' @export
#' @examples
#' USCS.coarse.symbol(pg = 60, ps = 35, pf = 5, Cc = 1.5, Cu = 5,
#'                    LL = 0, PL = 0, PI = 0)
USCS.coarse.symbol = function(pg, ps, pf, Cc, Cu, LL, PL, PI = NA) {
  if (ps < pg) {
    coarse <- "G"
  } else if (ps >= pg) {
    coarse <- "S"
  }
  if (coarse == "G") {
    if (pf < 5) {
      if (Cu >= 4 && (Cc >= 1 && Cc <= 3)) {
        symbol <- "GW"
      } else {
        symbol <- "GP"
      }
    } else if (pf >= 5 && pf <= 12) {
      fines <- USCS.fine.symbol(PI = PI, LL = LL, PL = PL)
      if (Cu >= 4 && (Cc >= 1 && Cc <= 3)) {
        if (substr(fines, 1, 1) == "M") {
          symbol <- "GW-GM"
        } else if (substr(fines, 1, 1) == "C") {
          symbol <- "GW-GC"
        }
      } else {
        if (substr(fines, 1, 1) == "M") {
          symbol <- "GP-GM"
        } else if (substr(fines, 1, 1) == "C") {
          symbol <- "GP-GC"
        }
      }
    } else if (pf > 12) {
      fines <- USCS.fine.symbol(PI = PI, LL = LL, PL = PL)
      if (substr(fines, 1, 1) == "M") {
        symbol <- "GM"
      } else if (fines == "CL" || fines == "CH") {
        symbol <- "GC"
      } else if (fines == "CL-ML") {
        symbol <- "GC-GM"
      }
    }
  }
  if (coarse == "S") {
    if (pf < 5) {
      if (Cu >= 6 && (Cc >= 1 && Cc <= 3)) {
        symbol <- "SW"
      } else {
        symbol <- "SP"
      }
    } else if (pf >= 5 && pf <= 12) {
      fines <- USCS.fine.symbol(PI = PI, LL = LL, PL = PL)
      if (Cu >= 6 && (Cc >= 1 && Cc <= 3)) {
        if (substr(fines, 1, 1) == "M") {
          symbol <- "SW-SM"
        } else if (substr(fines, 1, 1) == "C") {
          symbol <- "SW-SC"
        }
      } else {
        if (substr(fines, 1, 1) == "M") {
          symbol <- "SP-SM"
        } else if (substr(fines, 1, 1) == "C") {
          symbol <- "SP-SC"
        }
      }
    } else if (pf > 12) {
      fines <- USCS.fine.symbol(PI = PI, LL = LL, PL = PL)
      if (substr(fines, 1, 1) == "M") {
        symbol <- "SM"
      } else if (fines == "CL" || fines == "CH") {
        symbol <- "SC"
      } else if (fines == "CL-ML") {
        symbol <- "SC-SM"
      }
    }
  }
  return(symbol)
}

#' @title Classify a soil using the Unified Soil Classification System (USCS)
#'
#' @description Determines the USCS group symbol and descriptive name (ASTM D2487) for a
#' soil sample. Inputs can be provided as percent components and gradation
#' coefficients, or derived automatically from a grain-size distribution and
#' Atterberg limits.
#'
#' @param pg Numeric. Percent gravel. Default \code{NA}.
#' @param ps Numeric. Percent sand. Default \code{NA}.
#' @param pf Numeric. Percent fines. Default \code{NA}.
#' @param Cc Numeric. Curvature coefficient. Default \code{NA}.
#' @param Cu Numeric. Uniformity coefficient. Default \code{NA}.
#' @param LL Numeric. Liquid limit (percent). Default \code{NA}.
#' @param PL Numeric. Plastic limit (percent). Default \code{NA}.
#' @param PI Numeric. Plasticity index (percent). Default \code{NA}; computed from \code{LL} and \code{PL} when missing.
#' @param sieve Numeric vector of ASTM sieve numbers. Used to derive \code{size} when \code{size} is \code{NA}. Default \code{NA}.
#' @param size Numeric vector of particle sizes. Default \code{NA}.
#' @param percent Numeric vector of percent passing values (0–100). Default \code{NA}.
#' @param metric Logical. If \code{TRUE} (default) \code{size} is in millimetres.
#' @param spanish Logical. If \code{TRUE} the group name is returned in Spanish;
#'   default \code{FALSE} (English).
#'
#' @returns A named list with:
#'   \describe{
#'     \item{symbol}{USCS group symbol (character).}
#'     \item{name}{Descriptive group name (character).}
#'     \item{pg}{Percent gravel.}
#'     \item{ps}{Percent sand.}
#'     \item{pf}{Percent fines.}
#'     \item{Cu}{Uniformity coefficient (only for coarse soils with \eqn{pf \le 12}).}
#'     \item{Cc}{Curvature coefficient (only for coarse soils with \eqn{pf \le 12}).}
#'   }
#' @details This function is taken from the "geotech" package from James Kaklamanos
#' @family soil mechanics, classification
#' @export
#' @examples
#' USCS(pg = 15, ps = 34, pf = 51, Cc = 1, Cu = 4, LL = 40, PL = 10)
#'
USCS = function(
  pg = NA,
  ps = NA,
  pf = NA,
  Cc = NA,
  Cu = NA,
  LL = NA,
  PL = NA,
  PI = NA,
  sieve = NA,
  size = NA,
  percent = NA,
  metric = T,
  spanish = F
) {
  if (is.na(PI) & !is.na(LL) & !is.na(PL)) {
    PI = LL - PL
  }
  if (is.na(PL) & !is.na(LL) & !is.na(PI)) {
    PL = LL - PI
  }
  if (is.na(LL) & !is.na(PL) & !is.na(PI)) {
    LL = PL + PI
  }

  if (is.na(pg) == TRUE && is.na(ps) == TRUE && is.na(pf) == TRUE) {
    if (all(is.na(sieve) == FALSE)) {
      size <- size.from.sieve(sieve = sieve, metric = metric)
      metric <- TRUE
    }
    pg <- percentComponents(size = size, percent = percent, metric = metric)$pg
    ps <- percentComponents(size = size, percent = percent, metric = metric)$ps
    pf <- percentComponents(size = size, percent = percent, metric = metric)$pf
    Cu <- grainSize.coefs(size = size, percent = percent)$Cu
    Cc <- grainSize.coefs(size = size, percent = percent)$Cc
  }

  if (pf >= 50) {
    symbol <- USCS.fine.symbol(PI = PI, LL = LL, PL = PL)
    if (LL < 50) {
      if (symbol == "CL") {
        if (pf >= 70) {
          if (pf >= 85) {
            name <- ifelse(isFALSE(spanish), "Lean clay", "Arcilla delgada")
          } else if (pf >= 70 && pf <= 85) {
            if (ps >= pg) {
              name <- ifelse(
                isFALSE(spanish),
                "Lean clay with sand",
                "Arcilla delgada con arena"
              )
            } else if (ps < pg) {
              name <- ifelse(
                isFALSE(spanish),
                "Lean clay with gravel",
                "Arcilla delgada con grava"
              )
            }
          }
        } else if (pf >= 50 && pf < 70) {
          if (ps >= pg) {
            if (pg < 15) {
              name <- ifelse(
                isFALSE(spanish),
                "Sandy lean clay",
                "Arcilla delgada arenosa"
              )
            } else if (pg >= 15) {
              name <- ifelse(
                isFALSE(spanish),
                "Sandy lean clay with gravel",
                "Arcilla delgada arenosa con grava"
              )
            }
          } else if (ps < pg) {
            if (ps < 15) {
              name <- ifelse(
                isFALSE(spanish),
                "Gravelly lean clay",
                "Arcilla delgada gravosa"
              )
            } else if (ps >= 15) {
              name <- ifelse(
                isFALSE(spanish),
                "Gravelly lean clay with sand",
                "Arcilla delgada gravosa con arena"
              )
            }
          }
        }
      } else if (symbol == "CL-ML") {
        if (pf >= 70) {
          if (pf >= 85) {
            name <- ifelse(isFALSE(spanish), "Silty clay", "Arcilla limosa")
          } else if (pf >= 70 && pf < 85) {
            if (ps >= pg) {
              name <- ifelse(
                isFALSE(spanish),
                "Silty clay with sand",
                "Arcilla limosa con arena"
              )
            } else if (ps < pg) {
              name <- ifelse(
                isFALSE(spanish),
                "Silty clay with gravel",
                "Arcilla limosa con grava"
              )
            }
          }
        } else if (pf >= 50 && pf < 70) {
          if (ps >= pg) {
            if (pg < 15) {
              name <- ifelse(
                isFALSE(spanish),
                "Sandy silty clay",
                "Arcilla limosa arenosa"
              )
            } else if (pg >= 15) {
              name <- ifelse(
                isFALSE(spanish),
                "Sandy silty clay with gravel",
                "Arcilla limosa arenosa con grava"
              )
            }
          } else if (ps < pg) {
            if (ps < 15) {
              name <- ifelse(
                isFALSE(spanish),
                "Gravelly silty clay",
                "Arcilla limosa gravosa"
              )
            } else if (ps >= 15) {
              name <- ifelse(
                isFALSE(spanish),
                "Gravelly silty clay with sand",
                "Arcilla limosa gravosa con arena"
              )
            }
          }
        }
      } else if (symbol == "ML") {
        if (pf >= 70) {
          if (pf >= 85) {
            name <- ifelse(isFALSE(spanish), "Silt", "Limo")
          } else if (pf >= 70 && pf < 85) {
            if (ps >= pg) {
              name <- ifelse(
                isFALSE(spanish),
                "Silt with sand",
                "Limo con arena"
              )
            } else if (ps < pg) {
              name <- ifelse(
                isFALSE(spanish),
                "Silt with gravel",
                "Limo con grava"
              )
            }
          }
        } else if (pf >= 50 && pf < 70) {
          if (ps >= pg) {
            if (pg < 15) {
              name <- ifelse(isFALSE(spanish), "Sandy silt", "Limo arenoso")
            } else if (pg >= 15) {
              name <- ifelse(
                isFALSE(spanish),
                "Sandy silt with gravel",
                "Limo arenoso con grava"
              )
            }
          } else if (ps < pg) {
            if (ps < 15) {
              name <- ifelse(isFALSE(spanish), "Gravelly silt", "Limo gravoso")
            } else if (ps >= 15) {
              name <- ifelse(
                isFALSE(spanish),
                "Gravelly silt with sand",
                "Limo gravoso con arena"
              )
            }
          }
        }
      }
    } else if (LL >= 50) {
      if (symbol == "CH") {
        if (pf >= 70) {
          if (pf >= 85) {
            name <- ifelse(isFALSE(spanish), "Fat clay", "Arcilla gruesa")
          } else if (pf >= 70 && pf < 85) {
            if (ps >= pg) {
              name <- ifelse(
                isFALSE(spanish),
                "Fat clay with sand",
                "Arcilla gruesa con arena"
              )
            } else if (ps < pg) {
              name <- ifelse(
                isFALSE(spanish),
                "Fat clay with gravel",
                "Arcilla gruesa con grava"
              )
            }
          }
        } else if (pf >= 50 && pf < 70) {
          if (ps >= pg) {
            if (pg < 15) {
              name <- ifelse(
                isFALSE(spanish),
                "Sandy fat clay",
                "Arcilla gruesa arenosa"
              )
            } else if (pg >= 15) {
              name <- ifelse(
                isFALSE(spanish),
                "Sandy fat clay with gravel",
                "Arcilla gruesa arenosa con grava"
              )
            }
          } else if (ps < pg) {
            if (ps < 15) {
              name <- ifelse(
                isFALSE(spanish),
                "Gravelly fat clay",
                "Arcilla gruesa gravosa"
              )
            } else if (ps >= 15) {
              name <- ifelse(
                isFALSE(spanish),
                "Gravelly fat clay with sand",
                "Arcilla gruesa gravosa con arena"
              )
            }
          }
        }
      } else if (symbol == "MH") {
        if (pf >= 70) {
          if (pf >= 85) {
            name <- ifelse(
              isFALSE(spanish),
              "Elastic silt",
              "Limo el\u00e1stico"
            )
          } else if (pf >= 70 && pf < 85) {
            if (ps >= pg) {
              name <- ifelse(
                isFALSE(spanish),
                "Elastic silt with sand",
                "Limo el\u00e1stico con arena"
              )
            } else if (ps < pg) {
              name <- ifelse(
                isFALSE(spanish),
                "Elastic silt with gravel",
                "Limo el\u00e1stico con grava"
              )
            }
          }
        } else if (pf >= 50 && pf < 70) {
          if (ps >= pg) {
            if (pg < 15) {
              name <- ifelse(
                isFALSE(spanish),
                "Sandy elastic silt",
                "Limo el\u00e1stico arenoso"
              )
            } else if (pg >= 15) {
              name <- ifelse(
                isFALSE(spanish),
                "Sandy elastic silt with gravel",
                "Limo el\u00e1stico arenoso con grava"
              )
            }
          } else if (ps < pg) {
            if (ps < 15) {
              name <- ifelse(
                isFALSE(spanish),
                "Gravelly elastic silt",
                "Limo el\u00e1stico gravoso"
              )
            } else if (ps >= 15) {
              name <- ifelse(
                isFALSE(spanish),
                "Gravelly elastic silt with sand",
                "Limo el\u00e1stico gravoso con arena"
              )
            }
          }
        }
      }
    }
  } else if (pf < 50) {
    symbol <- USCS.coarse.symbol(
      pg = pg,
      ps = ps,
      pf = pf,
      Cc = Cc,
      Cu = Cu,
      PI = PI,
      LL = LL,
      PL = PL
    )
    if (symbol == "GW") {
      if (ps < 15) {
        name <- ifelse(
          isFALSE(spanish),
          "Well-graded gravel",
          "Grava bien gradada"
        )
      } else if (ps >= 15) {
        name <- ifelse(
          isFALSE(spanish),
          "Well-graded gravel with sand",
          "Grava bien gradada con arena"
        )
      }
    } else if (symbol == "GP") {
      if (ps < 15) {
        name <- ifelse(
          isFALSE(spanish),
          "Poorly-graded gravel",
          "Grava pobremente gradada"
        )
      } else if (ps >= 15) {
        name <- ifelse(
          isFALSE(spanish),
          "Poorly-graded gravel with sand",
          "Grava pobremente gradada con arena"
        )
      }
    } else if (symbol == "GW-GM") {
      if (ps < 15) {
        name <- ifelse(
          isFALSE(spanish),
          "Well-graded gravel with silt",
          "Grava bien gradada con limo"
        )
      } else if (ps >= 15) {
        name <- ifelse(
          isFALSE(spanish),
          "Well-graded gravel with silt and sand",
          "Grava bien gradada con limo y arena"
        )
      }
    } else if (symbol == "GW-GC") {
      if (ps < 15) {
        name <- ifelse(
          isFALSE(spanish),
          "Well-graded gravel with clay (or silty clay)",
          "Grava bien gradada con arcilla (o arcilla limosa)"
        )
      } else if (ps >= 15) {
        name <- ifelse(
          isFALSE(spanish),
          "Well-graded gravel with clay & sand (or silty clay & sand)",
          "Grava bien gradada con arcilla y arena (o arcilla limosa y arena)"
        )
      }
    } else if (symbol == "GP-GM") {
      if (ps < 15) {
        name <- ifelse(
          isFALSE(spanish),
          "Poorly-graded gravel with silt",
          "Grava pobremente gradada con limo"
        )
      } else if (ps >= 15) {
        name <- ifelse(
          isFALSE(spanish),
          "Poorly-graded gravel with silt and sand",
          "Grava pobremente gradada con limo y arena"
        )
      }
    } else if (symbol == "GP-GC") {
      if (ps < 15) {
        name <- ifelse(
          isFALSE(spanish),
          "Poorly-graded gravel with clay (or silty clay)",
          "Grava pobremente gradada con arcilla (o arcilla limosa)"
        )
      } else if (ps >= 15) {
        name <- ifelse(
          isFALSE(spanish),
          "Poorly-graded gravel with clay & sand (or silty clay & sand)",
          "Grava pobremente gradada con arcilla y arena (o arcilla limosa y arena)"
        )
      }
    } else if (symbol == "GM") {
      if (ps < 15) {
        name <- ifelse(isFALSE(spanish), "Silty gravel", "Grava limosa")
      } else if (ps >= 15) {
        name <- ifelse(
          isFALSE(spanish),
          "Silty gravel with sand",
          "Grava limosa con arena"
        )
      }
    } else if (symbol == "GC") {
      if (ps < 15) {
        name <- ifelse(isFALSE(spanish), "Clayey gravel", "Grava arcillosa")
      } else if (ps >= 15) {
        name <- ifelse(
          isFALSE(spanish),
          "Clayey gravel with sand",
          "Grava arcillosa con arena"
        )
      }
    } else if (symbol == "GC-GM") {
      if (ps < 15) {
        name <- ifelse(
          isFALSE(spanish),
          "Silty, clayey gravel",
          "Grava arcillosa limosa"
        )
      } else if (ps >= 15) {
        name <- ifelse(
          isFALSE(spanish),
          "Silty, clayey gravel with sand",
          "Grava arcillosa limosa con arena"
        )
      }
    } else if (symbol == "SW") {
      if (pg < 15) {
        name <- ifelse(
          isFALSE(spanish),
          "Well-graded sand",
          "Arena bien gradada"
        )
      } else if (pg >= 15) {
        name <- ifelse(
          isFALSE(spanish),
          "Well-graded sand with gravel",
          "Arena bien gradada con grava"
        )
      }
    } else if (symbol == "SP") {
      if (pg < 15) {
        name <- ifelse(
          isFALSE(spanish),
          "Poorly-graded sand",
          "Arena pobremente gradada"
        )
      } else if (pg >= 15) {
        name <- ifelse(
          isFALSE(spanish),
          "Poorly-graded sand with gravel",
          "Arena pobremente gradada con grava"
        )
      }
    } else if (symbol == "SW-SM") {
      if (pg < 15) {
        name <- ifelse(
          isFALSE(spanish),
          "Well-graded sand with silt",
          "Arena bien gradada con limo"
        )
      } else if (pg >= 15) {
        name <- ifelse(
          isFALSE(spanish),
          "Well-graded sand with silt and gravel",
          "Arena bien gradada con limo y grava"
        )
      }
    } else if (symbol == "SW-SC") {
      if (pg < 15) {
        name <- ifelse(
          isFALSE(spanish),
          "Well-graded sand with clay (or silty clay)",
          "Arena bien gradada con arcilla (o arcilla limosa)"
        )
      } else if (pg >= 15) {
        name <- ifelse(
          isFALSE(spanish),
          "Well-graded sand with clay & gravel (or silty clay & gravel)",
          "Arena bien gradada con arcilla y grava (o arcilla limosa y grava)"
        )
      }
    } else if (symbol == "SP-SM") {
      if (pg < 15) {
        name <- ifelse(
          isFALSE(spanish),
          "Poorly-graded sand with silt",
          "Arena pobremente gradada con limo"
        )
      } else if (pg >= 15) {
        name <- ifelse(
          isFALSE(spanish),
          "Poorly-graded sand with silt and gravel",
          "Arena pobremente gradada con limo y grava"
        )
      }
    } else if (symbol == "SP-SC") {
      if (pg < 15) {
        name <- ifelse(
          isFALSE(spanish),
          "Poorly-graded sand with clay (or silty clay)",
          "Arena pobremente gradada con arcilla (o arcilla limosa)"
        )
      } else if (pg >= 15) {
        name <- ifelse(
          isFALSE(spanish),
          "Poorly-graded sand with clay & gravel (or silty clay & gravel)",
          "Arena pobremente gradada con arcilla y grava (o arcilla limosa y grava"
        )
      }
    } else if (symbol == "SM") {
      if (pg < 15) {
        name <- ifelse(isFALSE(spanish), "Silty sand", "Arena limosa")
      } else if (pg >= 15) {
        name <- ifelse(
          isFALSE(spanish),
          "Silty sand with gravel",
          "Arena limosa con grava"
        )
      }
    } else if (symbol == "SC") {
      if (pg < 15) {
        name <- ifelse(isFALSE(spanish), "Clayey sand", "Arena arcillosa")
      } else if (pg >= 15) {
        name <- ifelse(
          isFALSE(spanish),
          "Clayey sand with gravel",
          "Arena arcillosa con grava"
        )
      }
    } else if (symbol == "SC-SM") {
      if (pg < 15) {
        name <- ifelse(
          isFALSE(spanish),
          "Silty, clayey sand",
          "Arena arcillosa limosa"
        )
      } else if (pg >= 15) {
        name <- ifelse(
          isFALSE(spanish),
          "Silty, clayey sand with gravel",
          "Arena arcillosa limosa con grava"
        )
      }
    }
  }

  if (pf <= 12) {
    return(list(
      symbol = symbol,
      name = name,
      pg = pg,
      ps = ps,
      pf = pf,
      Cu = Cu,
      Cc = Cc
    ))
  } else {
    return(list(symbol = symbol, name = name, pg = pg, ps = ps, pf = pf))
  }
}
