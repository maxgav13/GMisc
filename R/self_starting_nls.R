#' Chapman-Richards Growth Model
#'
#' @description
#' These functions provide Chapman-Richards equation (\code{chaprich.fun()}), as well as the self-starters for the [nls()] (\code{SSchaprich()}) and [drc::drm()] functions (\code{DRCchaprich()}).
#'
#' @param x A numeric vector of non-negative values (e.g., time, distance).
#' @param a The asymptote (upper limit) of the response. Must be positive.
#' @param b The rate parameter controlling the speed of growth. Must be positive.
#' @param c The shape parameter controlling the inflection point of the curve.
#'
#' @details
#' The Chapman-Richards model is defined as:
#'
#' \deqn{y = a \left(1 - e^{-bx}\right)^c}
#'
#' where \code{a} is the upper asymptote, \code{b} is the rate parameter, and
#' \code{c} is the shape parameter.
#'
#' @return \code{chaprich.fun()} and \code{SSchaprich()} return a numeric value, while \code{DRCchaprich()} returns a list containing the nonlinear function and the self starter function.
#'
#' @rdname chaprich
#' @order 2
#' @examples
#' x <- seq(0, 10, length.out = 50)
#' y <- chaprich.fun(x, a = 10, b = 0.5, c = 2) + rnorm(50, sd = 0.2)
#' df <- data.frame(x = x, y = y)
#' mod = nls(y ~ SSchaprich(x, a, b, c), data = df)
#' summary(mod)
#' plot(x,y,cex=.8)
#' lines(x,predict(mod),col='blue')
#'
#' @export
SSchaprich <- selfStart(
  function(x, a, b, c) {
    a * (1 - exp(-b * x))^c
  },
  function(mCall, LHS, data, ...) {
    xy <- sortedXyData(mCall[["x"]], LHS, data)
    x <- xy[, "x"]
    y <- xy[, "y"]

    a <- max(y) * 1.05 # larger buffer so y/a is safely < 1
    c <- 1

    pseudoy <- log(pmax(1 - (y / a)^(1 / c), .Machine$double.eps)) # guard against log(0) or NaN

    ## Linear regression on pseudo-y and pseudo-x
    coefs <- (coef(lm(pseudoy ~ x - 1)))
    b = -coefs[1]
    start <- c(a, b, c)
    names(start) <- mCall[c("a", "b", "c")]
    start
  },
  parameters = c("a", "b", "c")
)

#' @rdname chaprich
#' @order 1
#' @family non-linear functions, self-starters
#' @export
chaprich.fun <- function(x, a, b, c) {
  a * (1 - exp(-b * x))^c
}

#' Exponential Model with Inverse Predictor (Type 3)
#'
#' @description
#' These functions provide Exponential Model with Inverse Predictor (Type 3) equation (\code{exp3.fun()}), as well as the self-starters for the [nls()] (\code{SSexp3()}) and [drc::drm()] functions (\code{DRCexp3()}).
#'
#' @param x A numeric vector of non-zero positive values (e.g., time, distance).
#' @param a A scaling parameter representing the value of the response when
#'   \code{1/x} approaches zero (i.e., as \code{x} tends to infinity). Must be positive.
#' @param b The rate parameter in the exponent. Controls the curvature of the
#'   response; negative values yield a decreasing curve, positive values an
#'   increasing curve.
#'
#' @details
#' The model is defined as:
#'
#' \deqn{y = a \, e^{b / x}}
#'
#' where \code{a} is a scaling parameter representing the response as
#' \code{x} tends to infinity, and \code{b} is the rate parameter in the
#' exponent. Negative values of \code{b} yield a decreasing curve; positive
#' values yield an increasing curve.
#'
#' @return \code{exp3.fun()} and \code{SSexp3()} return a numeric value, while \code{DRCexp3()} returns a list containing the nonlinear function and the self starter function.
#'
#' @rdname exp3
#' @order 2
#' @examples
#' x <- seq(1, 20, length.out = 50)
#' y <- exp3.fun(x, a = 5, b = -3) + rnorm(50, sd = 0.1)
#' df <- data.frame(x = x, y = y)
#' mod = nls(y ~ SSexp3(x, a, b), data = df)
#' summary(mod)
#' plot(x, y, cex = 0.8)
#' lines(x, predict(mod), col = 'blue')
#'
#' @export
SSexp3 <- selfStart(
  function(x, a, b) {
    a * exp(b / x)
  },
  function(mCall, LHS, data, ...) {
    xy <- sortedXyData(mCall[["x"]], LHS, data)
    x <- xy[, "x"]
    y <- xy[, "y"]

    pseudox = 1 / x
    pseudoy = log(y)

    ## Linear regression on pseudo-y and pseudo-x
    coefs <- (coef(lm(pseudoy ~ pseudox)))
    b = coefs[2]
    a = exp(coefs[1])
    start <- c(a, b)
    names(start) <- mCall[c("a", "b")]
    start
  },
  parameters = c("a", "b")
)

#' @rdname exp3
#' @order 1
#' @family non-linear functions, self-starters
#' @export
exp3.fun <- function(x, a, b) {
  a * exp(b / x)
}


#' Negative Exponential Model
#'
#' @description
#' These functions provide Negative Exponential Model equation (\code{expneg.fun()}), as well as the self-starters for the [nls()] (\code{SSexpneg()}) and [drc::drm()] functions (\code{DRCexpneg()}).
#'
#' @param x A numeric vector of values (e.g., time, dose).
#' @param a The horizontal asymptote of the response as \code{x} tends to infinity.
#' @param b The initial offset from the asymptote at \code{x = 0}, i.e.
#'   \eqn{y(0) = a + b}.
#' @param c The rate parameter in the exponent. Negative values yield decay
#'   toward \code{a}; positive values yield growth away from \code{a}.
#'
#' @details
#' The model is defined as:
#'
#' \deqn{y = a + b \, e^{cx}}
#'
#' where \code{a} is the horizontal asymptote as \code{x} tends to infinity,
#' \code{b} is the initial offset from the asymptote at \code{x = 0}
#' (i.e. \eqn{y(0) = a + b}), and \code{c} is the rate parameter. Negative
#' values of \code{c} yield decay toward \code{a}; positive values yield
#' growth away from \code{a}.
#'
#' @return \code{expneg.fun()} and \code{SSexpneg()} return a numeric value, while \code{DRCexpneg()} returns a list containing the nonlinear function and the self starter function.
#'
#' @rdname expneg
#' @order 2
#' @examples
#' x <- seq(0, 10, length.out = 50)
#' y <- expneg.fun(x, a = 5, b = -4, c = -0.4) + rnorm(50, sd = 0.1)
#' df <- data.frame(x = x, y = y)
#' mod = nls(y ~ SSexpneg(x, a, b, c), data = df)
#' summary(mod)
#' plot(x, y, cex = 0.8)
#' lines(x, predict(mod), col = 'blue')
#'
#' @export
SSexpneg <- selfStart(
  function(x, a, b, c) {
    a + b * exp(c * x)
  },
  function(mCall, LHS, data, ...) {
    xy <- sortedXyData(mCall[["x"]], LHS, data)
    x <- xy[, "x"]
    y <- xy[, "y"]

    a = max(y)
    b = min(y) - a
    c = -.5

    start <- c(a, b, c)
    names(start) <- mCall[c("a", "b", "c")]
    start
  },
  parameters = c("a", "b", "c")
)

#' @rdname expneg
#' @order 1
#' @family non-linear functions, self-starters
#' @export
expneg.fun <- function(x, a, b, c) {
  a + b * exp(c * x)
}

#' Two-Asymptote Exponential Model
#'
#' @description
#' These functions provide Two-Asymptote Exponential Model equation (\code{expneg2.fun()}), as well as the self-starters for the [nls()] (\code{SSexpneg2()}) and [drc::drm()] functions (\code{DRCexpneg2()}).
#'
#' @param x A numeric vector of values (e.g., time, dose).
#' @param a The initial asymptote: the value of the response as \code{x} approaches
#'   zero (or negative infinity).
#' @param b The final asymptote: the value of the response as \code{x} tends to
#'   infinity.
#' @param c The rate parameter in the exponent. Negative values yield a
#'   transition from \code{a} toward \code{b} as \code{x} increases.
#'
#' @details
#' The model is defined as:
#'
#' \deqn{y = a \, e^{cx} + b \left(1 - e^{cx}\right)}
#'
#' This can be rearranged as \eqn{y = b + (a - b)\,e^{cx}}, making clear that
#' the curve transitions from \code{a} at \eqn{x = 0} to \code{b} as
#' \eqn{x \to \infty} (when \eqn{c < 0}). \code{a} is the initial asymptote,
#' \code{b} is the final asymptote, and \code{c} is the rate parameter.
#'
#' @return \code{expneg2.fun()} and \code{SSexpneg2()} return a numeric value, while \code{DRCexpneg2()} returns a list containing the nonlinear function and the self starter function.
#'
#' @rdname expneg2
#' @order 2
#' @examples
#' x <- seq(0, 10, length.out = 50)
#' y <- expneg2.fun(x, a = 2, b = 8, c = -0.4) + rnorm(50, sd = 0.2)
#' df <- data.frame(x = x, y = y)
#' mod = nls(y ~ SSexpneg2(x, a, b, c), data = df)
#' summary(mod)
#' plot(x, y, cex = 0.8)
#' lines(x, predict(mod), col = 'blue')
#'
#' @export
SSexpneg2 <- selfStart(
  function(x, a, b, c) {
    a * exp(c * x) + b * (1 - exp(c * x))
  },
  function(mCall, LHS, data, ...) {
    xy <- sortedXyData(mCall[["x"]], LHS, data)
    x <- xy[, "x"]
    y <- xy[, "y"]

    a = min(y)
    b = max(y)
    c = -.5

    start <- c(a, b, c)
    names(start) <- mCall[c("a", "b", "c")]
    start
  },
  parameters = c("a", "b", "c")
)

#' @rdname expneg2
#' @order 1
#' @family non-linear functions, self-starters
#' @export
expneg2.fun <- function(x, a, b, c) {
  a * exp(c * x) + b * (1 - exp(c * x))
}

#' Exponential Variogram Model
#'
#' @description
#' These functions provide Exponential Variogram Model equation (\code{varexp.fun()}), as well as the self-starters for the [nls()] (\code{SSvarexp()}) and [drc::drm()] functions (\code{DRCvarexp()}).
#'
#' @param x A numeric vector of non-negative lag distances.
#' @param nug The nugget: the semivariance at zero lag distance (intercept).
#' @param psill The partial sill: the difference between the sill (total
#'   variance) and the nugget.
#' @param range The range parameter controlling the rate of increase. The
#'   practical range at which ~95 percent of the sill is reached is approximately
#'   \eqn{3 \times \text{range}}.
#'
#' @details
#' The model is defined as:
#'
#' \deqn{y = \text{nug} + \text{psill} \left(1 - e^{-x / \text{range}}\right)}
#'
#' where \code{nug} is the nugget (semivariance at zero lag), \code{psill} is
#' the partial sill (difference between sill and nugget), and \code{range}
#' controls the rate of increase. The practical range at which ~95 percent of
#' the sill is reached is approximately \eqn{3 \times \text{range}}.
#'
#' This is a standard exponential variogram model commonly used in
#' geostatistics to describe spatial autocorrelation. Unlike the spherical
#' model, it approaches the sill asymptotically.
#'
#' @return \code{varexp.fun()} and \code{SSvarexp()} return a numeric value, while \code{DRCvarexp()} returns a list containing the nonlinear function and the self starter function.
#'
#' @rdname varexp
#' @order 2
#' @examples
#' x <- seq(0, 100, length.out = 50)
#' y <- varexp.fun(x, nug = 0.2, psill = 0.8, range = 20) + rnorm(50, sd = 0.02)
#' df <- data.frame(x = x, y = y)
#' mod = nls(y ~ SSvarexp(x, nug, psill, range), data = df)
#' summary(mod)
#' plot(x, y, cex = 0.8)
#' lines(x, predict(mod), col = 'blue')
#'
#' @export
SSvarexp <- selfStart(
  function(x, nug, psill, range) {
    nug + psill * (1 - exp(-x / range))
  },
  function(mCall, LHS, data, ...) {
    xy <- sortedXyData(mCall[["x"]], LHS, data)
    x <- xy[, "x"]
    y <- xy[, "y"]

    nug = min(y)
    psill = max(y) - nug
    range = x[which.max(y)] * 1 / 3

    start <- c(nug, psill, range)
    names(start) <- mCall[c("nug", "psill", "range")]
    start
  },
  parameters = c("nug", "psill", "range")
)

#' @rdname varexp
#' @order 1
#' @family non-linear functions, self-starters
#' @export
varexp.fun <- function(x, nug, psill, range) {
  nug + psill * (1 - exp(-x / range))
}

#' Gaussian Variogram Model
#'
#' @description
#' These functions provide Gaussian Variogram Model equation (\code{vargauss.fun()}), as well as the self-starters for the [nls()] (\code{SSvargauss()}) and [drc::drm()] functions (\code{DRCvargauss()}).
#'
#' @param x A numeric vector of non-negative lag distances.
#' @param nug The nugget: the semivariance at zero lag distance (intercept).
#' @param psill The partial sill: the difference between the sill (total
#'   variance) and the nugget.
#' @param range The range parameter controlling the rate of increase. The
#'   practical range at which ~95 percent of the sill is reached is
#'   approximately \eqn{\sqrt{3} \times \text{range}}.
#'
#' @details
#' The model is defined as:
#'
#' \deqn{y = \text{nug} + \text{psill} \left(1 - e^{-(x / \text{range})^2}\right)}
#'
#' where \code{nug} is the nugget (semivariance at zero lag), \code{psill} is
#' the partial sill, and \code{range} controls the rate of increase. The
#' practical range at which ~95 percent of the sill is reached is approximately
#' \eqn{\sqrt{3} \times \text{range}}. The Gaussian model has a smooth,
#' parabolic behaviour near the origin compared to the concave-up exponential.
#'
#' The Gaussian variogram model is commonly used in geostatistics to describe
#' spatial autocorrelation with a smooth, parabolic behaviour near the origin.
#' Like the exponential model, it approaches the sill asymptotically, but has
#' an S-shaped rise compared to the concave-up exponential curve.
#'
#' @return \code{vargauss.fun()} and \code{SSvargauss()} return a numeric value, while \code{DRCvargauss()} returns a list containing the nonlinear function and the self starter function.
#'
#' @rdname vargauss
#' @order 2
#' @examples
#' x <- seq(0, 100, length.out = 50)
#' y <- vargauss.fun(x, nug = 0.2, psill = 0.8, range = 20) + rnorm(50, sd = 0.02)
#' df <- data.frame(x = x, y = y)
#' mod = nls(y ~ SSvargauss(x, nug, psill, range), data = df)
#' summary(mod)
#' plot(x, y, cex = 0.8)
#' lines(x, predict(mod), col = 'blue')
#'
#' @export
SSvargauss <- selfStart(
  function(x, nug, psill, range) {
    nug + psill * (1 - exp(-(x / range)^2))
  },
  function(mCall, LHS, data, ...) {
    xy <- sortedXyData(mCall[["x"]], LHS, data)
    x <- xy[, "x"]
    y <- xy[, "y"]

    nug = min(y)
    psill = max(y) - nug
    range = x[which.max(y)] * 1 / sqrt(3)

    start <- c(nug, psill, range)
    names(start) <- mCall[c("nug", "psill", "range")]
    start
  },
  parameters = c("nug", "psill", "range")
)

#' @rdname vargauss
#' @order 1
#' @family non-linear functions, self-starters
#' @export
vargauss.fun <- function(x, nug, psill, range) {
  nug + psill * (1 - exp(-(x / range)^2))
}


#' Spherical Variogram Model
#'
#' @description
#' These functions provide Spherical Variogram Model equation (\code{varsph.fun()}), as well as the self-starters for the [nls()] (\code{SSvarsph()}) and [drc::drm()] functions (\code{DRCvarsph()}).
#'
#' @param x A numeric vector of non-negative lag distances.
#' @param nug The nugget: the semivariance at zero lag distance (intercept).
#' @param psill The partial sill: the difference between the sill (total
#'   variance) and the nugget.
#' @param range The range parameter: the lag distance at which the sill is
#'   reached and spatial autocorrelation ceases. Beyond this distance the
#'   semivariance is constant at \code{nug + psill}.
#'
#' @details
#' The model is defined as:
#'
#' \deqn{y = \begin{cases}
#'   \text{nug} + \text{psill}
#'     \left(\frac{3}{2}\frac{x}{\text{range}} -
#'           \frac{1}{2}\left(\frac{x}{\text{range}}\right)^3\right)
#'     & x \leq \text{range} \\
#'   \text{nug} + \text{psill} & x > \text{range}
#' \end{cases}}
#'
#' where \code{nug} is the nugget, \code{psill} is the partial sill, and
#' \code{range} is the lag distance at which the sill is reached exactly.
#' Unlike exponential and Gaussian models, the spherical model reaches the
#' sill at a finite distance.
#'
#' Unlike the exponential and Gaussian models, the spherical model reaches the
#' sill exactly at a finite distance (\code{range}), making it suitable when a hard
#' limit on spatial dependence is expected.
#'
#' @return \code{varsph.fun()} and \code{SSvarsph()} return a numeric value, while \code{DRCvarsph()} returns a list containing the nonlinear function and the self starter function.
#'
#' @rdname varsph
#' @order 2
#' @examples
#' x <- seq(0, 100, length.out = 50)
#' y <- varsph.fun(x, nug = 0.2, psill = 0.8, range = 30) + rnorm(50, sd = 0.02)
#' df <- data.frame(x = x, y = y)
#' mod = nls(y ~ SSvarsph(x, nug, psill, range), data = df)
#' summary(mod)
#' plot(x, y, cex = 0.8)
#' lines(x, predict(mod), col = 'blue')
#'
#' @export
SSvarsph <- selfStart(
  function(x, nug, psill, range) {
    ifelse(
      x <= range,
      nug + psill * (3 / 2 * (x / range) - 1 / 2 * (x / range)^3),
      nug + psill
    )
  },
  function(mCall, LHS, data, ...) {
    xy <- sortedXyData(mCall[["x"]], LHS, data)
    x <- xy[, "x"]
    y <- xy[, "y"]

    nug = min(y)
    psill = max(y) - nug
    range = x[which.max(y)] * 2 / 3

    start <- c(nug, psill, range)
    names(start) <- mCall[c("nug", "psill", "range")]
    start
  },
  parameters = c("nug", "psill", "range")
)

#' @rdname varsph
#' @order 1
#' @family non-linear functions, self-starters
#' @export
varsph.fun <- function(x, nug, psill, range) {
  ifelse(
    x <= range,
    nug + psill * (3 / 2 * (x / range) - 1 / 2 * (x / range)^3),
    nug + psill
  )
}

#' First-Order Inverse Polynomial Model
#'
#' @description
#' These functions provide First-Order Inverse Polynomial Model equation (\code{invpoly1.fun()}), as well as the self-starters for the [nls()] (\code{SSinvpoly1()}) and [drc::drm()] functions (\code{DRCinvpoly1()}).
#'
#' @param x A numeric vector of non-zero positive values (e.g., time, distance).
#' @param a The coefficient of the \eqn{1/x} term in the linearised form.
#'   Controls the initial rise of the curve near the origin.
#' @param b The coefficient of the \eqn{x} term in the denominator. The
#'   reciprocal \eqn{1/b} gives the asymptote of the response as
#'   \eqn{x \to \infty}.
#'
#' @details
#' The model is defined as:
#'
#' \deqn{y = \frac{x}{a + b x}}
#'
#' where \code{a} is the coefficient of the \eqn{1/x} term in the linearised
#' form (controls the initial rise near the origin) and \code{b} is the
#' coefficient of the linear term in the denominator. The reciprocal
#' \eqn{1/b} gives the asymptote as \eqn{x \to \infty}.
#'
#' The curve passes through the origin and approaches the asymptote \eqn{1/b}
#' as \eqn{x \to \infty}.
#'
#' @return \code{invpoly1.fun()} and \code{SSinvpoly1()} return a numeric value, while \code{DRCinvpoly1()} returns a list containing the nonlinear function and the self starter function.
#'
#' @rdname invpoly1
#' @order 2
#' @examples
#' x <- seq(1, 20, length.out = 50)
#' y <- invpoly1.fun(x, a = 2, b = 0.5) + rnorm(50, sd = 0.05)
#' df <- data.frame(x = x, y = y)
#' mod = nls(y ~ SSinvpoly1(x, a, b), data = df)
#' summary(mod)
#' plot(x, y, cex = 0.8)
#' lines(x, predict(mod), col = 'blue')
#'
#' @export
SSinvpoly1 <- selfStart(
  function(x, a, b) {
    x / (a + b * x)
  },
  function(mCall, LHS, data, ...) {
    xy <- sortedXyData(mCall[["x"]], LHS, data)
    x <- xy[, "x"]
    y <- xy[, "y"]

    x = x[which(y != 0)]
    y = y[which(y != 0)]

    pseudox = 1 / x
    pseudoy = 1 / y

    ## Linear regression on pseudo-y and pseudo-x
    coefs <- abs(coef(lm(pseudoy ~ pseudox)))
    b <- coefs[1]
    a = coefs[2]
    start <- c(a, b)
    names(start) <- mCall[c("a", "b")]
    start
  },
  parameters = c("a", "b")
)

#' @rdname invpoly1
#' @order 1
#' @family non-linear functions, self-starters
#' @export
invpoly1.fun <- function(x, a, b) {
  x / (a + b * x)
}

#' Second-Order Inverse Polynomial Model
#'
#' @description
#' These functions provide Second-Order Inverse Polynomial Model equation (\code{invpoly2.fun()}), as well as the self-starters for the [nls()] (\code{SSinvpoly2()}) and [drc::drm()] functions (\code{DRCinvpoly2()}).
#'
#' @param x A numeric vector of non-zero positive values (e.g., dose, time).
#' @param a The coefficient of the \eqn{1/x} term in the linearised form.
#'   Controls the initial rise of the curve near the origin.
#' @param b The coefficient of the linear term in the denominator. Along with
#'   \code{c}, determines the location and height of the response peak.
#' @param c The coefficient of the quadratic term in the denominator. A
#'   positive value causes the curve to decline after reaching a maximum,
#'   producing a unimodal response.
#'
#' @details
#' The model is defined as:
#'
#' \deqn{y = \frac{x}{a + b x + c x^2}}
#'
#' where \code{a} controls the initial rise near the origin, \code{b} is the
#' linear term in the denominator, and \code{c} is the quadratic term. A
#' positive \code{c} causes the curve to reach a maximum and subsequently
#' decline, producing a unimodal response.
#'
#' Unlike the first-order model [SSinvpoly1()], the quadratic term in the
#' denominator allows the curve to reach a maximum and subsequently decline,
#' making it suitable for unimodal dose-response relationships.
#'
#' @return \code{invpoly2.fun()} and \code{SSinvpoly2()} return a numeric value, while \code{DRCinvpoly2()} returns a list containing the nonlinear function and the self starter function.
#'
#' @rdname invpoly2
#' @order 2
#' @examples
#' x <- seq(1, 20, length.out = 50)
#' y <- invpoly2.fun(x, a = 1, b = 0.5, c = 0.02) + rnorm(50, sd = 0.05)
#' df <- data.frame(x = x, y = y)
#' mod = nls(y ~ SSinvpoly2(x, a, b, c), data = df)
#' summary(mod)
#' plot(x, y, cex = 0.8)
#' lines(x, predict(mod), col = 'blue')
#'
#' @export
SSinvpoly2 <- selfStart(
  function(x, a, b, c) {
    x / (a + b * x + c * x^2)
  },
  function(mCall, LHS, data, ...) {
    xy <- sortedXyData(mCall[["x"]], LHS, data)
    x <- xy[, "x"]
    y <- xy[, "y"]

    x = x[which(y != 0)]
    y = y[which(y != 0)]

    pseudox = 1 / x
    pseudoy = 1 / y

    ## Linear regression on pseudo-y and pseudo-x
    coefs <- abs(coef(lm(pseudoy ~ pseudox + x)))
    b <- coefs[1]
    a = coefs[2]
    c = coefs[3]
    start <- c(a, b, c)
    names(start) <- mCall[c("a", "b", "c")]
    start
  },
  parameters = c("a", "b", "c")
)

#' @rdname invpoly2
#' @order 1
#' @family non-linear functions, self-starters
#' @export
invpoly2.fun <- function(x, a, b, c) {
  x / (a + b * x + c * x^2)
}

#' Third-Order Inverse Polynomial Model
#'
#' @description
#' These functions provide Third-Order Inverse Polynomial Model equation (\code{invpoly3.fun()}), as well as the self-starters for the [nls()] (\code{SSinvpoly3()}) and [drc::drm()] functions (\code{DRCinvpoly3()}).
#'
#' @param x A numeric vector of non-zero positive values (e.g., dose, time).
#' @param a The coefficient of the \eqn{1/x} term in the linearised form.
#'   Controls the initial rise of the curve near the origin.
#' @param b The coefficient of the linear term in the denominator. Along with
#'   \code{c} and \code{d}, determines the shape and location of any peaks or
#'   troughs in the response.
#' @param c The coefficient of the quadratic term in the denominator. Contributes
#'   to the curvature and can produce a unimodal response.
#' @param d The coefficient of the cubic term in the denominator. Allows for
#'   additional inflection points, enabling bimodal or more complex response
#'   shapes.
#'
#' @details
#' The model is defined as:
#'
#' \deqn{y = \frac{x}{a + b x + c x^2 + d x^3}}
#'
#' where \code{a} controls the initial rise near the origin, \code{b} is the
#' linear term, \code{c} is the quadratic term, and \code{d} is the cubic
#' term in the denominator. The cubic term extends it by allowing
#' for additional inflection points, enabling bimodal or more complex response
#' shapes.
#'
#' @return \code{invpoly3.fun()} and \code{SSinvpoly3()} return a numeric value, while \code{DRCinvpoly3()} returns a list containing the nonlinear function and the self starter function.
#'
#' @rdname invpoly3
#' @order 2
#' @examples
#' x <- seq(1, 20, length.out = 50)
#' y <- invpoly3.fun(x, a = 1, b = 0.5, c = 0.02, d = 0.001) + rnorm(50, sd = 0.05)
#' df <- data.frame(x = x, y = y)
#' mod = nls(y ~ SSinvpoly3(x, a, b, c, d), data = df)
#' summary(mod)
#' plot(x, y, cex = 0.8)
#' lines(x, predict(mod), col = 'blue')
#'
#' @export
SSinvpoly3 <- selfStart(
  function(x, a, b, c, d) {
    x / (a + b * x + c * x^2 + d * x^3)
  },
  function(mCall, LHS, data, ...) {
    xy <- sortedXyData(mCall[["x"]], LHS, data)
    x <- xy[, "x"]
    y <- xy[, "y"]

    x = x[which(y != 0)]
    y = y[which(y != 0)]

    pseudox = 1 / x
    pseudoy = 1 / y

    ## Linear regression on pseudo-y and pseudo-x
    coefs <- abs(coef(lm(pseudoy ~ pseudox + x + I(x^2))))
    b <- coefs[1]
    a = coefs[2]
    c = coefs[3]
    d = coefs[4]
    start <- c(a, b, c, d)
    names(start) <- mCall[c("a", "b", "c", "d")]
    start
  },
  parameters = c("a", "b", "c", "d")
)

#' @rdname invpoly3
#' @order 1
#' @family non-linear functions, self-starters
#' @export
invpoly3.fun <- function(x, a, b, c, d) {
  x / (a + b * x + c * x^2 + d * x^3)
}


#' Modified Kostiakov Infiltration Model
#'
#' @description
#' These functions provide the Modified Kostiakov Infiltration Model equation (\code{kostmod.fun()}), as well as the self-starters for the [nls()] (\code{SSkostmod()}) and [drc::drm()] functions (\code{DRCkostmod()}).
#'
#' @param x A numeric vector (e.g., time, distance).
#' @param alfa The parameter controlling the shape of the curve. Must be non-zero.
#' @param beta The exponent applied to \code{x}. Affects the curvature of the
#'   model.
#' @param fc The value of the curve at the final observation.
#'
#' @details
#' The model is defined as:
#'
#' \deqn{y = \alpha x^{\beta} + f_c}
#'
#' where \code{alfa} (\eqn{\alpha}) controls the shape of the curve,
#' \code{beta} (\eqn{\beta}) is the exponent applied to \code{x} and affects
#' curvature, and \code{fc} (\eqn{f_c}) is the value of the curve at the final
#' observation (an additive constant).
#'
#' @return \code{kostmod.fun()} and \code{SSkostmod()} return a numeric value, while \code{DRCkostmod()} returns a list containing the nonlinear function and the self starter function.
#'
#' @rdname kostmod
#' @order 2
#' @examples
#' x <- seq(0.5, 40)
#' y <- kostmod.fun(x, alfa = 0.3, beta = -0.8, fc = 0.1) + rnorm(40, sd = 0.01)
#' df <- data.frame(x = x, y = y)
#' mod = nls(y ~ SSkostmod(x, alfa, beta, fc), data = df)
#' summary(mod)
#' plot(x, y, cex = 0.8)
#' lines(x, predict(mod), col = 'blue')
#'
#' @export
SSkostmod <- selfStart(
  function(x, alfa, beta, fc) {
    alfa * x^(beta) + fc
  },
  function(mCall, LHS, data, ...) {
    xy <- sortedXyData(mCall[["x"]], LHS, data)
    x <- xy[, "x"]
    y <- xy[, "y"]

    fc <- min(y) * 0.999 # slight offset to keep log(y - fc) finite
    yadj <- y - fc
    yadj <- pmax(yadj, .Machine$double.eps) # guard against log(0)

    coefs <- coef(lm(log(yadj) ~ log(x)))
    alfa <- exp(coefs[1])
    beta <- coefs[2]

    start <- c(alfa, beta, fc)
    names(start) <- mCall[c("alfa", "beta", "fc")]
    start
  },
  parameters = c("alfa", "beta", "fc")
)

#' @rdname kostmod
#' @order 1
#' @family non-linear functions, self-starters
#' @export
kostmod.fun <- function(x, alfa, beta, fc) {
  alfa * x^(beta) + fc
}
