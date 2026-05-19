#' Compute angles between lines or planes
#'
#' Calculates the angle between two lines, the angle between two planes, the
#' line of intersection of two planes, or the plane containing two apparent
#' dips, depending on the value of \code{ans0}.
#'
#' @param trd1 Numeric. Trend of the first line, or strike of the first plane
#'   (radians, right-hand rule for planes).
#' @param plg1 Numeric. Plunge of the first line, or dip of the first plane
#'   (radians).
#' @param trd2 Numeric. Trend of the second line, or strike of the second plane
#'   (radians).
#' @param plg2 Numeric. Plunge of the second line, or dip of the second plane
#'   (radians).
#' @param ans0 Character. Operation selector:
#'   \describe{
#'     \item{`"l"`}{Angle between two lines (trend/plunge input).}
#'     \item{`"p"`}{Angle between two planes (strike/dip input, right-hand rule).}
#'     \item{`"i"`}{Trend and plunge of the intersection line of two planes.}
#'     \item{`"a"`}{Strike and dip of the plane containing two apparent dips.}
#'   }
#' @references Allmendinger, R. W., Cardozo, N., & Fisher, D. M. (2012). Structural geology algorithms: Vectors and tensors. Cambridge university press.
#'
#' @returns A named list with components \code{ans1} and \code{ans2}. For \code{"l"} and
#'   \code{"p"}, these are the angle and its supplement (degrees). For \code{"i"} and
#'   \code{"a"}, these are the trend/strike and plunge/dip of the resulting line or
#'   plane (degrees).
#'
#' @family structural algorithms
#' @export
#' @examples
#' # Angle between two lines
#' Angles(trd1 = 3 * pi / 180, plg1 = 20 * pi / 180,
#'        trd2 = 80 * pi / 180, plg2 = 40 * pi / 180,
#'        ans0 = "l")
#'
#' # Intersection of two planes
#' Angles(trd1 = 40 * pi / 180, plg1 = 30 * pi / 180,
#'        trd2 = 120 * pi / 180, plg2 = 50 * pi / 180,
#'        ans0 = "i")
Angles <- function(trd1, plg1, trd2, plg2, ans0) {
  #If planes have been entered
  if (ans0 == 'i' | ans0 == 'p') {
    k <- 1
    #Else if lines have been entered
  } else if (ans0 == 'a' | ans0 == 'l') {
    k <- 0
  }

  #Calculate the direction cosines of the lines or poles to planes
  res1 <- SphToCartD(trd1, plg1, k)
  res2 <- SphToCartD(trd2, plg2, k)

  #If angle between 2 lines or between the poles to 2 planes
  if (ans0 == 'l' | ans0 == 'p') {
    # Use dot product = Sum of the products of the direction cosines
    ans1 <- (acos(
      res1$cn * res2$cn + res1$ce * res2$ce + res1$cd * res2$cd
    )) *
      180 /
      pi
    ans2 <- (pi) * 180 / pi - ans1
  }

  #If intersection of two planes or pole to a plane containing two
  #apparent dips
  if (ans0 == 'a' | ans0 == 'i') {
    #If the 2 planes or apparent dips are parallel return an error
    if (trd1 == trd2 && plg1 == plg2) {
      stop('lines or planes are parallel')
      #Else use cross product
    } else {
      cn <- res1$ce * res2$cd - res1$cd * res2$ce
      ce <- res1$cd * res2$cn - res1$cn * res2$cd
      cd <- res1$cn * res2$ce - res1$ce * res2$cn
      #Make sure the vector points down into the lower hemisphere
      if (cd < 0.0) {
        cn <- -cn
        ce <- -ce
        cd <- -cd
      }
      #Convert vector to unit vector by dividing it by its length
      r <- sqrt(cn * cn + ce * ce + cd * cd)
      # Calculate line of intersection or pole to plane
      res <- CartToSphD(cn / r, ce / r, cd / r)
      #If intersection of two planes
      if (ans0 == 'i') {
        ans1 <- res[1]
        ans2 <- res[2]
        #Else if plane containing two dips, calculate plane from its pole
      } else if (ans0 == 'a') {
        res <- Pole(res[1] * pi / 180, res[2] * pi / 180, 0)
        ans1 = res[1] * 180 / pi
        ans2 = res[2] * 180 / pi
      }
    }
  }
  return(list(ans1 = ans1, ans2 = ans2))
}

#' Convert between a plane and its pole
#'
#' Returns either the strike and dip of a plane given its pole, or the trend
#' and plunge of a pole given its plane (right-hand rule convention).
#'
#' @param trd Numeric. Trend of the pole (radians) when \code{k = 0}, or strike of
#'   the plane (radians, right-hand rule) when \code{k = 1}.
#' @param plg Numeric. Plunge of the pole (radians) when \code{k = 0}, or dip of
#'   the plane (radians) when \code{k = 1}.
#' @param k Integer. Direction of conversion: \code{0} returns the plane from its
#'   pole; \code{1} returns the pole from its plane.
#'
#' @returns A named numeric vector. When \code{k = 0}: \code{c(strike, dip)} in radians.
#'   When \code{k = 1}: \code{c(trd, plg)} in radians.
#' @references Allmendinger, R. W., Cardozo, N., & Fisher, D. M. (2012). Structural geology algorithms: Vectors and tensors. Cambridge university press.
#'
#' @family structural algorithms
#' @export
#' @examples
#' # Pole to plane (k = 1)
#' Pole(trd = pi / 4, plg = pi / 6, k = 1)
#'
#' # Plane from its pole (k = 0)
#' Pole(trd = pi / 3, plg = pi / 8, k = 0)
Pole <- function(trd, plg, k) {
  out <- c(trd, plg)
  #Some constants
  east <- pi / 2.0

  #Calculate plane given its pole
  if (k == 0) {
    # trd = GMisc::rads(trd)
    # plg = GMisc::rads(plg)
    if (plg >= 0.0) {
      out[2] <- east - plg
      dipaz <- trd - pi
    } else {
      out[2] <- east + plg
      dipaz <- trd
    }
    #Calculate trd1 and make sure it is between 0 and 2*pi
    out[1] <- ZeroTwoPi(dipaz - east)
    names(out) = c('strike', 'dip')
    #Else calculate pole given its plane
  } else if (k == 1) {
    cned <- SphToCart(trd, plg, k)
    out <- CartToSph(cn = cned[1], ce = cned[2], cd = cned[3])
    names(out) = c('trd', 'plg')
  }
  return(out)
}

#' Constrain an azimuth to the range \[0, 2π\)
#'
#' Wraps an azimuth value in radians so that it always lies in the interval
#' \[0, 2π).
#'
#' @param a Numeric. Input azimuth in radians (may be outside \[0, 2π)).
#'
#' @returns A numeric value: the equivalent azimuth in \[0, 2π) (radians).
#' @references Allmendinger, R. W., Cardozo, N., & Fisher, D. M. (2012). Structural geology algorithms: Vectors and tensors. Cambridge university press.
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' # Triple colon required because it's not exported
#' GMisc:::ZeroTwoPi(-0.5)
#' GMisc:::ZeroTwoPi(7)
#' }
ZeroTwoPi <- function(a) {
  b <- a
  twopi <- 2.0 * pi
  if (b < 0.0) {
    b <- b + twopi
  } else if (b >= twopi) {
    b <- b - twopi
  }
  return(b)
}

#' Convert spherical to Cartesian coordinates (radians)
#'
#' Converts a line or plane orientation given in spherical coordinates
#' (trend/plunge or strike/dip) to direction cosines in a North–East–Down
#' coordinate system.
#'
#' @param trd Numeric. Trend of the line (radians, \code{k = 0}) or strike of the
#'   plane (radians, right-hand rule, \code{k = 1}).
#' @param plg Numeric. Plunge of the line (radians, \code{k = 0}) or dip of the
#'   plane (radians, \code{k = 1}).
#' @param k Integer. \code{0} for a line (trend/plunge); \code{1} for a plane
#'   (strike/dip, right-hand rule). Default \code{0}.
#'
#' @returns A named numeric vector \code{c(cn, ce, cd)} with the north, east, and
#'   down direction cosines.
#' @references Allmendinger, R. W., Cardozo, N., & Fisher, D. M. (2012). Structural geology algorithms: Vectors and tensors. Cambridge university press.
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' # Triple colon required because it's not exported
#' GMisc:::SphToCart(trd = pi / 4, plg = pi / 6)
#' GMisc:::SphToCart(trd = pi / 3, plg = pi / 5, k = 1)
#' }
SphToCart <- function(trd, plg, k = 0) {
  if (k == 0) {
    cd <- sin(plg)
    ce <- cos(plg) * sin(trd)
    cn <- cos(plg) * cos(trd)
  } else if (k == 1) {
    cd <- cos(plg)
    ce <- -sin(plg) * cos(trd)
    cn <- sin(plg) * sin(trd)
  }
  return(c(cn = cn, ce = ce, cd = cd))
}

#' Convert spherical to Cartesian coordinates (degrees)
#'
#' Converts a line or plane orientation
#' in spherical coordinates to direction cosines in a North–East–Down system.
#'
#' @param trd Numeric. Trend of the line (degrees, \code{k = 0}) or strike of the
#'   plane (degrees, right-hand rule, \code{k = 1}).
#' @param plg Numeric. Plunge of the line (degrees, \code{k = 0}) or dip of the
#'   plane (degrees, \code{k = 1}).
#' @param k Integer. \code{0} for a line (trend/plunge); \code{1} for a plane
#'   (strike/dip, right-hand rule). Default \code{0}.
#'
#' @returns A named list with components \code{cn}, \code{ce}, and \code{cd} (north, east, and
#'   down direction cosines).
#' @references Allmendinger, R. W., Cardozo, N., & Fisher, D. M. (2012). Structural geology algorithms: Vectors and tensors. Cambridge university press.
#'
#' @family structural algorithms
#' @export
#' @examples
#' SphToCartD(trd = 45, plg = 30)
#' SphToCartD(trd = 60, plg = 36, k = 1)
SphToCartD <- function(trd, plg, k = 0) {
  trd0 = trd
  plg0 = plg
  trd = (trd0) * pi / 180
  plg = (plg0) * pi / 180

  if (k == 0) {
    cd <- sin(plg)
    ce <- cos(plg) * sin(trd)
    cn <- cos(plg) * cos(trd)
  } else if (k == 1) {
    cd <- cos(plg)
    ce <- -sin(plg) * cos(trd)
    cn <- sin(plg) * sin(trd)
  }
  return(list(cn = cn, ce = ce, cd = cd))
}

#' Convert Cartesian to spherical coordinates (radians)
#'
#' Converts North–East–Down direction cosines to the trend and plunge of the
#' corresponding line in radians.
#'
#' @param cn Numeric. North direction cosine.
#' @param ce Numeric. East direction cosine.
#' @param cd Numeric. Down direction cosine.
#'
#' @returns A named numeric vector \code{c(trd, plg)} with trend and plunge in
#'   radians.
#' @references Allmendinger, R. W., Cardozo, N., & Fisher, D. M. (2012). Structural geology algorithms: Vectors and tensors. Cambridge university press.
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' # Triple colon required because it's not exported
#' GMisc:::CartToSph(cn = 0.707, ce = 0.5, cd = 0.5)
#' }
CartToSph <- function(cn, ce, cd) {
  plg <- asin(cd)

  # Trend
  # If north direction cosine is zero, trend is east or west
  # Choose which one by the sign of the east direction cosine
  if (cn == 0.0) {
    if (ce < 0.0) {
      trd <- 3.0 / 2.0 * pi # trend is west
    } else {
      trd <- pi / 2.0 # trend is east
    }
  } else {
    trd <- atan(ce / cn)
    if (cn < 0.0) {
      # Add pi
      trd <- trd + pi
    }
    # Make sure trd is between 0 and 2*pi
    trd <- ZeroTwoPi(trd)
  }

  return(c(trd = trd, plg = plg))
}

#' Convert Cartesian to spherical coordinates (degrees)
#'
#' Converts North–East–Down direction
#' cosines to the trend and plunge of the corresponding line in degrees.
#'
#' @param cn Numeric. North direction cosine.
#' @param ce Numeric. East direction cosine.
#' @param cd Numeric. Down direction cosine.
#'
#' @returns A named numeric vector \code{c(trd, plg)} with trend and plunge in
#'   degrees.
#' @references Allmendinger, R. W., Cardozo, N., & Fisher, D. M. (2012). Structural geology algorithms: Vectors and tensors. Cambridge university press.
#'
#' @family structural algorithms
#' @export
#' @examples
#' CartToSphD(cn = 0.707, ce = 0.5, cd = 0.5)
CartToSphD <- function(cn, ce, cd) {
  plg <- asin(cd)

  # Trend
  # If north direction cosine is zero, trend is east or west
  # Choose which one by the sign of the east direction cosine
  if (cn == 0.0) {
    if (ce < 0.0) {
      trd <- 3.0 / 2.0 * pi # trend is west
    } else {
      trd <- pi / 2.0 # trend is east
    }
  } else {
    trd <- atan(ce / cn)
    if (cn < 0.0) {
      # Add pi
      trd <- trd + pi
    }
    # Make sure trd is between 0 and 2*pi
    trd <- ZeroTwoPi(trd)
  }

  return(c(trd = (trd) * 180 / pi, plg = (plg) * 180 / pi))
}

#' Compute a great-circle path for a stereonet (radians)
#'
#' Computes the path of the great circle (trace) of a plane on an
#' equal-area or equal-angle stereonet of unit radius.
#'
#' @param strike Numeric. Strike of the plane in radians (right-hand rule).
#' @param dip Numeric. Dip of the plane in radians.
#' @param wulff Logical. If \code{TRUE} uses an equal-angle (Wulff) net; if \code{FALSE}
#'   (default) uses an equal-area (Schmidt) net.
#'
#' @returns Numeric matrix with columns \code{xp} and \code{yp} giving the
#'   stereonet coordinates of the great-circle path.
#' @references Allmendinger, R. W., Cardozo, N., & Fisher, D. M. (2012). Structural geology algorithms: Vectors and tensors. Cambridge university press.
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' # Triple colon required because it's not exported
#' GMisc:::GreatCircle(strike = pi / 4, dip = pi / 6)
#' GMisc:::GreatCircle(strike = pi / 3, dip = pi / 4, wulff = TRUE)
#' }
GreatCircle = function(strike, dip, wulff = F) {
  # GreatCircle computes the great circle path of a plane in an equal angle
  # or equal area stereonet of unit radius

  #    strike = strike of plane
  #    dip = dip of plane
  #   wulff = T for equal angle, F for equal area
  tpa <- Pole(strike, dip, k = 1)
  trd <- strike
  plg <- 0
  rot <- 0:180 * pi / 180
  rot = ifelse(rot == pi, rot * .9999, rot)
  out <- matrix(0, nrow = 181, ncol = 2)
  for (i in 1:181) {
    rtp <- Rotate(tpa[1], tpa[2], rot[i], trd, plg, "a")
    out[i, ] <- StCoordLine(rtp[1], rtp[2], wulff = wulff)
  }
  out
}

#' Compute a great-circle path for a stereonet (degrees)
#'
#' Computes the path of the great circle of a plane on an equal-area or equal-angle stereonet of unit
#' radius.
#'
#' @param strike Numeric. Strike of the plane in degrees (right-hand rule).
#' @param dip Numeric. Dip of the plane in degrees.
#' @param wulff Logical. If \code{TRUE} uses an equal-angle (Wulff) net; if \code{FALSE}
#'   (default) uses an equal-area (Schmidt) net.
#'
#' @returns A tibble with columns \code{xp} and \code{yp} giving the stereonet
#'   coordinates of the great-circle path.
#' @references Allmendinger, R. W., Cardozo, N., & Fisher, D. M. (2012). Structural geology algorithms: Vectors and tensors. Cambridge university press.
#'
#' @family structural algorithms
#' @export
#' @examples
#' GreatCircleD(strike = 45, dip = 30)
#' GreatCircleD(strike = 60, dip = 45, wulff = TRUE)
GreatCircleD = function(strike, dip, wulff = F) {
  strike = (strike) * pi / 180
  dip = (dip) * pi / 180

  tpa <- Pole(strike, dip, k = 1)
  trd <- strike
  plg <- 0
  rot <- 0:180 * pi / 180
  rot = ifelse(rot == pi, rot * .9999, rot)
  out <- matrix(0, nrow = 181, ncol = 2)
  for (i in 1:181) {
    rtp <- Rotate(tpa[1], tpa[2], rot[i], trd, plg, "a")
    out[i, ] <- StCoordLine(rtp[1], rtp[2], wulff = wulff)
  }
  tibble::as_tibble(out) %>% purrr::set_names(c('xp', 'yp'))
}

#' Compute a small-circle path for a stereonet (radians)
#'
#' Computes the path(s) of a small circle defined by its rotation axis and cone
#' angle on an equal-area or equal-angle stereonet of unit radius. When the
#' small circle crosses the primitive, the path is split into two segments.
#'
#' @param trda Numeric. Trend of the rotation axis in radians.
#' @param plga Numeric. Plunge of the rotation axis in radians.
#' @param coneAngle Numeric. Half-apex (cone) angle in radians.
#' @param wulff Logical. If \code{TRUE} uses an equal-angle (Wulff) net; if \code{FALSE}
#'   (default) uses an equal-area (Schmidt) net.
#' @param closed Logical. If \code{TRUE} the path segments are closed by appending
#'   the first point to the end. Default \code{FALSE}.
#'
#' @returns A named list with components:
#'   \describe{
#'     \item{`path1`}{Matrix of stereonet coordinates for the main segment.}
#'     \item{`path2`}{Matrix of stereonet coordinates for the secondary segment
#'       (empty when the circle does not cross the primitive).}
#'   }
#' @references Allmendinger, R. W., Cardozo, N., & Fisher, D. M. (2012). Structural geology algorithms: Vectors and tensors. Cambridge university press.
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' # Triple colon required because it's not exported
#' GMisc:::SmallCircle(trda = pi / 4, plga = pi / 6, coneAngle = pi / 8)
#' }
SmallCircle = function(trda, plga, coneAngle, wulff = F, closed = F) {
  if (plga < coneAngle) {
    plga = ifelse(plga == pi / 2.0, plga * 0.9999, plga)
    angle <- acos(cos(coneAngle) / cos(plga))
    trd <- ZeroTwoPi(trda + angle)
    plg <- 0
  } else {
    trd <- trda
    plg <- plga - coneAngle
  }
  rot <- (0:360) * pi / 180
  path1 <- NULL
  path2 <- NULL
  np1 <- 0
  np2 <- 0
  for (i in 1:360) {
    rtp = Rotate(trda, plga, rot[i], trd, plg, "v")
    if (rtp[2] >= 0) {
      path1 <- rbind(path1, StCoordLine(rtp[1], rtp[2], wulff = wulff))
    } else {
      path2 <- rbind(path2, StCoordLine(rtp[1], rtp[2], wulff = wulff))
    }
  }

  if (closed) {
    if (plga < coneAngle) {
      path2 = rbind(path2, -path1[1, ])
    } else {
      path1 = rbind(path1, path1[1, ])
    }
  }

  list(path1 = path1, path2 = path2)
}

#' Compute a small-circle path for a stereonet (degrees)
#'
#' Computes the path(s) of a small circle defined by its rotation axis and cone angle on an equal-area or
#' equal-angle stereonet of unit radius.
#'
#' @param trda Numeric. Trend of the rotation axis in degrees.
#' @param plga Numeric. Plunge of the rotation axis in degrees.
#' @param coneAngle Numeric. Half-apex (cone) angle in degrees.
#' @param wulff Logical. If \code{TRUE} uses an equal-angle (Wulff) net; if \code{FALSE}
#'   (default) uses an equal-area (Schmidt) net.
#' @param closed Logical. If \code{TRUE} the path segments are closed by appending
#'   the first point to the end. Default \code{FALSE}.
#'
#' @returns A named list with components:
#'   \describe{
#'     \item{`path1`}{Tibble of stereonet coordinates for the main segment.}
#'     \item{`path2`}{Tibble of stereonet coordinates for the secondary segment.}
#'   }
#' @references Allmendinger, R. W., Cardozo, N., & Fisher, D. M. (2012). Structural geology algorithms: Vectors and tensors. Cambridge university press.
#'
#' @family structural algorithms
#' @export
#' @examples
#' SmallCircleD(trda = 45, plga = 30, coneAngle = 20)
SmallCircleD = function(trda, plga, coneAngle, wulff = F, closed = F) {
  trda = (trda) * pi / 180
  plga = (plga) * pi / 180
  coneAngle = (coneAngle) * pi / 180

  if (plga < coneAngle) {
    plga = ifelse(plga == pi / 2.0, plga * 0.9999, plga)
    angle <- acos(cos(coneAngle) / cos(plga))
    trd <- ZeroTwoPi(trda + angle)
    plg <- 0
  } else {
    trd <- trda
    plg <- plga - coneAngle
  }
  rot <- (0:360) * pi / 180
  path1 <- NULL
  path2 <- NULL
  np1 <- 0
  np2 <- 0
  for (i in 1:360) {
    rtp = Rotate(trda, plga, rot[i], trd, plg, "v")
    if (rtp[2] >= 0) {
      path1 <- rbind(path1, StCoordLine(rtp[1], rtp[2], wulff = wulff))
    } else {
      path2 <- rbind(path2, StCoordLine(rtp[1], rtp[2], wulff = wulff))
    }
  }

  path1 = tibble::as_tibble(path1)
  path2 = tibble::as_tibble(path2)

  if (closed) {
    if (plga < coneAngle) {
      path2 = dplyr::bind_rows(path2, -path1[1, ])
    } else {
      path1 = dplyr::bind_rows(path1, path1[1, ])
    }
  }

  list(path1 = path1, path2 = path2)
}

#' Rotate a line about an axis
#'
#' Rotates a line (or axis) by applying a coordinate transformation matrix
#' computed from the rotation axis and rotation magnitude. Based on the
#' algorithm originally written by Randall A. Marrett.
#'
#' @param raz Numeric. Trend of the rotation axis in radians.
#' @param rdip Numeric. Plunge of the rotation axis in radians.
#' @param rot Numeric. Rotation magnitude in radians (positive = right-hand
#'   rule about the axis).
#' @param trd Numeric. Trend of the line to rotate in radians.
#' @param plg Numeric. Plunge of the line to rotate in radians.
#' @param ans0 Character. \code{"a"} if the line to rotate is an axis (result forced
#'   into lower hemisphere); \code{"v"} if it is a vector.
#'
#' @returns A named numeric vector \code{c(trd, plg)} with the trend and plunge of
#'   the rotated line in radians.
#' @references Allmendinger, R. W., Cardozo, N., & Fisher, D. M. (2012). Structural geology algorithms: Vectors and tensors. Cambridge university press.
#'
#' @family structural algorithms
#' @export
#' @examples
#' Rotate(raz = pi / 2, rdip = 0, rot = pi / 4,
#'        trd = pi / 4, plg = pi / 6, ans0 = "a")
Rotate = function(raz, rdip, rot, trd, plg, ans0) {
  a <- matrix(0, 3, 3)
  pole <- rep(0, 3)
  plotr <- rep(0, 3)
  temp <- rep(0, 3)
  p <- SphToCart(raz, rdip, 0)
  x <- 1 - cos(rot)
  sinRot <- sin(rot)
  cosRot <- cos(rot)
  a[1, 1] <- cosRot + p[1] * p[1] * x
  a[1, 2] <- -p[3] * sinRot + p[1] * p[2] * x
  a[1, 3] <- p[2] * sinRot + p[1] * p[3] * x
  a[2, 1] <- p[3] * sinRot + p[2] * p[1] * x
  a[2, 2] <- cosRot + p[2] * p[2] * x
  a[2, 3] <- -p[1] * sinRot + p[2] * p[3] * x
  a[3, 1] <- -p[2] * sinRot + p[3] * p[1] * x
  a[3, 2] <- p[1] * sinRot + p[3] * p[2] * x
  a[3, 3] <- cosRot + p[3] * p[3] * x
  temp <- SphToCart(trd, plg, 0)
  for (i in 1:3) {
    plotr[i] = 0
    for (j in 1:3) {
      plotr[i] = a[i, j] * temp[j] + plotr[i]
    }
  }
  if (plotr[3] < 0 && ans0 == "a") {
    plotr[1] <- -plotr[1]
    plotr[2] <- -plotr[2]
    plotr[3] <- -plotr[3]
  }
  CartToSph(plotr[1], plotr[2], plotr[3])
}

#' Compute stereonet coordinates for a line (radians)
#'
#' Converts the trend and plunge of a line to its \eqn{(x, y)} position on an
#' equal-area or equal-angle stereonet of unit radius.
#'
#' @param trd Numeric. Trend of the line in radians.
#' @param plg Numeric. Plunge of the line in radians. Lines plunging into the
#'   upper hemisphere (negative plunge) are automatically reflected.
#' @param wulff Logical. If \code{TRUE} uses an equal-angle (Wulff) projection; if
#'   \code{FALSE} (default) uses an equal-area (Schmidt) projection.
#'
#' @returns A one-row matrix with columns \code{xp} and \code{yp} giving the stereonet coordinates.
#' @references Allmendinger, R. W., Cardozo, N., & Fisher, D. M. (2012). Structural geology algorithms: Vectors and tensors. Cambridge university press.
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' # Triple colon required because it's not exported
#' GMisc:::StCoordLine(trd = pi / 4, plg = pi / 6)
#' GMisc:::StCoordLine(trd = pi / 3, plg = pi / 4, wulff = TRUE)
#' }
StCoordLine = function(trd, plg, wulff = F) {
  if (plg < 0) {
    trd <- ZeroTwoPi(trd + pi)
    plg <- -plg
  }
  piS4 <- pi / 4
  s2 <- sqrt(2)
  plgS2 <- plg / 2
  if (wulff) {
    xp <- tan(piS4 - plgS2) * sin(trd)
    yp <- tan(piS4 - plgS2) * cos(trd)
  } else {
    xp <- s2 * sin(piS4 - plgS2) * sin(trd)
    yp <- s2 * sin(piS4 - plgS2) * cos(trd)
  }
  cbind(xp, yp)
}

#' Compute stereonet coordinates for a line (degrees)
#'
#' Converts the trend and plunge of one or more lines to their \eqn{(x, y)} positions on an equal-area or
#' equal-angle stereonet of unit radius. Vectorised over \code{trd} and \code{plg}.
#'
#' @param trd Numeric vector. Trend of the line(s) in degrees.
#' @param plg Numeric vector. Plunge of the line(s) in degrees. Lines plunging
#'   into the upper hemisphere (negative plunge) are automatically reflected.
#' @param wulff Logical. If \code{TRUE} uses an equal-angle (Wulff) projection; if
#'   \code{FALSE} (default) uses an equal-area (Schmidt) projection.
#'
#' @returns A tibble with columns \code{xp} and \code{yp} giving the stereonet
#'   coordinates for each input line.
#' @references Allmendinger, R. W., Cardozo, N., & Fisher, D. M. (2012). Structural geology algorithms: Vectors and tensors. Cambridge university press.
#'
#' @family structural algorithms
#' @export
#' @examples
#' StCoordLineD(trd = c(30, 90, 180), plg = c(10, 45, 60))
#' StCoordLineD(trd = 45, plg = 30, wulff = TRUE)
StCoordLineD = function(trd, plg, wulff = F) {
  trd = (trd) * pi / 180
  plg = (plg) * pi / 180

  piS4 <- pi / 4
  s2 <- sqrt(2)
  plgS2 <- plg / 2

  xp = rep(0, length(trd))
  yp = rep(0, length(trd))

  for (i in 1:length(trd)) {
    if (plg[i] < 0) {
      trd[i] <- ZeroTwoPi(trd[i] + pi)
      plg[i] <- -plg[i]
    }

    if (wulff) {
      xp[i] <- tan(piS4 - plgS2[i]) * sin(trd[i])
      yp[i] <- tan(piS4 - plgS2[i]) * cos(trd[i])
    } else {
      xp[i] <- s2 * sin(piS4 - plgS2[i]) * sin(trd[i])
      yp[i] <- s2 * sin(piS4 - plgS2[i]) * cos(trd[i])
    }
  }

  tibble::tibble(xp, yp)
}

#' Compute equal-area stereonet coordinates from azimuth and inclination (degrees)
#'
#' Converts azimuth and inclination angle pairs to \eqn{(x, y)} positions on
#' an equal-area (Schmidt) stereonet of unit radius. Handles dips that exceed
#' 90° by reflecting the point through the centre of the net.
#'
#' @param az Numeric vector. Azimuth (trend) of the line(s) in degrees.
#' @param iang Numeric vector. Inclination angle of the line(s) in degrees
#'   (positive downward; values > 90° are handled via internal quadrant
#'   correction).
#'
#' @returns A tibble with columns \code{x} and \code{y} giving the stereonet coordinates
#'   for each input line.
#'
#' @family structural algorithms
#' @export
#' @examples
#' StPointD(az = c(0, 90, 180, 270), iang = c(10, 30, 60, 45))
StPointD = function(az, iang) {
  # StPointD computes the coordinates of a line
  fmod = function(k, m) {
    j = floor(k / m)
    a = k - m * j
    return(a)
  }

  FixDip = function(A) {
    az = A$az
    dip = A$dip
    DEG2RAD = pi / 180
    RAD2DEG = 180 / pi
    tdip = DEG2RAD * fmod(dip, 360)
    co = cos(tdip)
    si = sin(tdip)
    ang = RAD2DEG * atan2(si, co)
    quad = rep(1, length(dip))
    quad[co >= 0 & si >= 0] = 1
    quad[co < 0 & si >= 0] = 2
    quad[co < 0 & si < 0] = 3
    quad[co >= 0 & si < 0] = 4
    dip[quad == 1] = ang[quad == 1]
    dip[quad == 2] = 180 - ang[quad == 2]
    dip[quad == 3] = 180 + ang[quad == 3]
    dip[quad == 4] = -ang[quad == 4]
    az[quad == 1] = az[quad == 1]
    az[quad == 2] = 180 + az[quad == 2]
    az[quad == 3] = az[quad == 3]
    az[quad == 4] = 180 + az[quad == 4]
    A$az = fmod(az, 360)
    A$dip = dip
    return(A)
  }

  DEG2RAD = pi / 180
  sph = cos(DEG2RAD * iang)
  sph[sph >= 0] = 0
  sph[sph < 0] = 1

  A = list(az = az, dip = iang)
  A$dip = iang
  A$az = az
  B = FixDip(A)
  trot = DEG2RAD * B$az
  tdip = B$dip
  xi = DEG2RAD * tdip
  tq = sqrt(2) * sin(xi / 2)
  pltx = tq * sin(trot)
  plty = tq * cos(trot)

  return(tibble::tibble(x = pltx, y = plty))
}
