#' @rdname chaprich
#' @order 3
#' @examples
#' \dontrun{
#' mod = drc::drm(y ~ x, data = df, fct = DRCchaprich())
#' summary(mod)
#' plot(mod, log = "")
#' }
#'
#' @export
DRCchaprich <- function() {
  chaprich.fct <- function(x, parm) {
    chaprich.fun(x, parm[, 1], parm[, 2], parm[, 3])
    # parm[, 1] * (1 - exp(-parm[, 2] * x))^parm[, 3]
  }

  chaprich.ssfct <- function(data) {
    x <- data[, 1]
    y <- data[, 2]

    a <- max(y) * 1.001
    c <- 1
    pseudoy <- log(1 - (y / a)^(1 / c))
    b <- -stats::coef(stats::lm(pseudoy ~ x - 1))[1]

    c(a, b, c)
  }

  chaprich.init <- function(mCall, LHS, data) {
    data <- stats::sortedXyData(mCall[["x"]], LHS, data)
    chaprich.ssfct(data)
  }
  structure(
    list(
      chaprich.fct,
      chaprich.ssfct,
      c("a", "b", "c"),
      "Chapman-Richards growth model"
    ),
    names = c("fct", "ssfct", "names", "text"),
    class = "drcMean"
  )
}

#' @rdname exp3
#' @order 3
#' @examples
#' \dontrun{
#' mod = drc::drm(y ~ x, data = df, fct = DRCexp3())
#' summary(mod)
#' plot(mod, log = "")
#' }
#'
#' @export
DRCexp3 <- function() {
  exp3.fct <- function(x, parm) {
    exp3.fun(x, parm[, 1], parm[, 2])
    # parm[, 1] * exp(parm[, 2] / x)
  }

  exp3.ssfct <- function(data) {
    x <- data[, 1]
    y <- data[, 2]

    pseudox <- 1 / x
    pseudoy <- log(y)

    coefs <- stats::coef(stats::lm(pseudoy ~ pseudox))
    b <- coefs[2]
    a <- exp(coefs[1])

    c(a, b)
  }

  exp3.init <- function(mCall, LHS, data) {
    data <- stats::sortedXyData(mCall[["x"]], LHS, data)
    exp3.ssfct(data)
  }

  structure(
    list(
      exp3.fct,
      exp3.ssfct,
      c("a", "b"),
      "Exponential model with inverse predictor (Type 3)"
    ),
    names = c("fct", "ssfct", "names", "text"),
    class = "drcMean"
  )
}


#' @rdname expneg
#' @order 3
#' @examples
#' \dontrun{
#' mod = drc::drm(y ~ x, data = df, fct = DRCexpneg())
#' summary(mod)
#' plot(mod, log = "")
#' }
#'
#' @export
DRCexpneg <- function() {
  expneg.fct <- function(x, parm) {
    expneg.fun(x, parm[, 1], parm[, 2], parm[, 3])
    # parm[, 1] + parm[, 2] * exp(parm[, 3] * x)
  }

  expneg.ssfct <- function(data) {
    x <- data[, 1]
    y <- data[, 2]

    a <- max(y)
    b <- min(y) - a
    c <- -0.5

    c(a, b, c)
  }

  expneg.init <- function(mCall, LHS, data) {
    data <- stats::sortedXyData(mCall[["x"]], LHS, data)
    expneg.ssfct(data)
  }

  structure(
    list(
      expneg.fct,
      expneg.ssfct,
      c("a", "b", "c"),
      "Negative exponential model"
    ),
    names = c("fct", "ssfct", "names", "text"),
    class = "drcMean"
  )
}


#' @rdname expneg2
#' @order 3
#' @examples
#' \dontrun{
#' mod = drc::drm(y ~ x, data = df, fct = DRCexpneg2())
#' summary(mod)
#' plot(mod, log = "")
#' }
#'
#' @export
DRCexpneg2 <- function() {
  expneg2.fct <- function(x, parm) {
    expneg2.fun(x, parm[, 1], parm[, 2], parm[, 3])
    # parm[, 1] * exp(parm[, 3] * x) + parm[, 2] * (1 - exp(parm[, 3] * x))
  }

  expneg2.ssfct <- function(data) {
    x <- data[, 1]
    y <- data[, 2]

    a <- min(y)
    b <- max(y)
    c <- -0.5

    c(a, b, c)
  }

  expneg2.init <- function(mCall, LHS, data) {
    data <- stats::sortedXyData(mCall[["x"]], LHS, data)
    expneg2.ssfct(data)
  }

  structure(
    list(
      expneg2.fct,
      expneg2.ssfct,
      c("a", "b", "c"),
      "Two-asymptote exponential model"
    ),
    names = c("fct", "ssfct", "names", "text"),
    class = "drcMean"
  )
}


#' @rdname varexp
#' @order 3
#' @examples
#' \dontrun{
#' mod = drc::drm(y ~ x, data = df, fct = DRCvarexp())
#' summary(mod)
#' plot(mod, log = "")
#' }
#'
#' @export
DRCvarexp <- function() {
  varexp.fct <- function(x, parm) {
    varexp.fun(x, parm[, 1], parm[, 2], parm[, 3])
    # parm[, 1] + parm[, 2] * (1 - exp(-x / parm[, 3]))
  }

  varexp.ssfct <- function(data) {
    x <- data[, 1]
    y <- data[, 2]

    nug <- min(y)
    psill <- max(y) - nug
    range <- x[which.max(y)] * 1 / 3

    c(nug, psill, range)
  }

  varexp.init <- function(mCall, LHS, data) {
    data <- stats::sortedXyData(mCall[["x"]], LHS, data)
    varexp.ssfct(data)
  }

  structure(
    list(
      varexp.fct,
      varexp.ssfct,
      c("nug", "psill", "range"),
      "Exponential variogram model"
    ),
    names = c("fct", "ssfct", "names", "text"),
    class = "drcMean"
  )
}

#' @rdname vargauss
#' @order 3
#' @examples
#' \dontrun{
#' mod = drc::drm(y ~ x, data = df, fct = DRCvargauss())
#' summary(mod)
#' plot(mod, log = "")
#' }
#'
#' @export
DRCvargauss <- function() {
  vargauss.fct <- function(x, parm) {
    vargauss.fun(x, parm[, 1], parm[, 2], parm[, 3])
    # parm[, 1] + parm[, 2] * (1 - exp(-(x / parm[, 3])^2))
  }

  vargauss.ssfct <- function(data) {
    x <- data[, 1]
    y <- data[, 2]

    nug <- min(y)
    psill <- max(y) - nug
    range <- x[which.max(y)] * 1 / sqrt(3)

    c(nug, psill, range)
  }

  vargauss.init <- function(mCall, LHS, data) {
    data <- stats::sortedXyData(mCall[["x"]], LHS, data)
    vargauss.ssfct(data)
  }

  structure(
    list(
      vargauss.fct,
      vargauss.ssfct,
      c("nug", "psill", "range"),
      "Gaussian variogram model"
    ),
    names = c("fct", "ssfct", "names", "text"),
    class = "drcMean"
  )
}


#' @rdname varsph
#' @order 3
#' @examples
#' \dontrun{
#' mod = drc::drm(y ~ x, data = df, fct = DRCvarsph())
#' summary(mod)
#' plot(mod, log = "")
#' }
#'
#' @export
DRCvarsph <- function() {
  varsph.fct <- function(x, parm) {
    varsph.fun(x, parm[, 1], parm[, 2], parm[, 3])
    # ifelse(
    #   x <= parm[, 3],
    #   parm[, 1] +
    #     parm[, 2] * (3 / 2 * (x / parm[, 3]) - 1 / 2 * (x / parm[, 3])^3),
    #   parm[, 1] + parm[, 2]
    # )
  }

  varsph.ssfct <- function(data) {
    x <- data[, 1]
    y <- data[, 2]

    nug <- min(y)
    psill <- max(y) - nug
    range <- x[which.max(y)] * 2 / 3

    c(nug, psill, range)
  }

  varsph.init <- function(mCall, LHS, data) {
    data <- stats::sortedXyData(mCall[["x"]], LHS, data)
    varsph.ssfct(data)
  }

  structure(
    list(
      varsph.fct,
      varsph.ssfct,
      c("nug", "psill", "range"),
      "Spherical variogram model"
    ),
    names = c("fct", "ssfct", "names", "text"),
    class = "drcMean"
  )
}

#' @rdname invpoly1
#' @order 3
#' @examples
#' \dontrun{
#' mod = drc::drm(y ~ x, data = df, fct = DRCinvpoly1())
#' summary(mod)
#' plot(mod, log = "")
#' }
#'
#' @export
DRCinvpoly1 <- function() {
  invpoly1.fct <- function(x, parm) {
    invpoly1.fun(x, parm[, 1], parm[, 2])
    # x / (parm[, 1] + parm[, 2] * x)
  }

  invpoly1.ssfct <- function(data) {
    x <- data[, 1]
    y <- data[, 2]

    idx <- y != 0
    x <- x[idx]
    y <- y[idx]

    pseudox <- 1 / x
    pseudoy <- 1 / y

    coefs <- abs(stats::coef(stats::lm(pseudoy ~ pseudox)))
    b <- coefs[1]
    a <- coefs[2]

    c(a, b)
  }

  invpoly1.init <- function(mCall, LHS, data) {
    data <- stats::sortedXyData(mCall[["x"]], LHS, data)
    invpoly1.ssfct(data)
  }

  structure(
    list(
      invpoly1.fct,
      invpoly1.ssfct,
      c("a", "b"),
      "First-order inverse polynomial model"
    ),
    names = c("fct", "ssfct", "names", "text"),
    class = "drcMean"
  )
}


#' @rdname invpoly2
#' @order 3
#' @examples
#' \dontrun{
#' mod = drc::drm(y ~ x, data = df, fct = DRCinvpoly2())
#' summary(mod)
#' plot(mod, log = "")
#' }
#'
#' @export
DRCinvpoly2 <- function() {
  invpoly2.fct <- function(x, parm) {
    invpoly2.fun(x, parm[, 1], parm[, 2], parm[, 3])
    # x / (parm[, 1] + parm[, 2] * x + parm[, 3] * x^2)
  }

  invpoly2.ssfct <- function(data) {
    x <- data[, 1]
    y <- data[, 2]

    idx <- y != 0
    x <- x[idx]
    y <- y[idx]

    pseudox <- 1 / x
    pseudoy <- 1 / y

    coefs <- abs(stats::coef(stats::lm(pseudoy ~ pseudox + x)))
    b <- coefs[1]
    a <- coefs[2]
    c <- coefs[3]

    c(a, b, c)
  }

  invpoly2.init <- function(mCall, LHS, data) {
    data <- stats::sortedXyData(mCall[["x"]], LHS, data)
    invpoly2.ssfct(data)
  }

  structure(
    list(
      invpoly2.fct,
      invpoly2.ssfct,
      c("a", "b", "c"),
      "Second-order inverse polynomial model"
    ),
    names = c("fct", "ssfct", "names", "text"),
    class = "drcMean"
  )
}


#' @rdname invpoly3
#' @order 3
#' @examples
#' \dontrun{
#' mod = drc::drm(y ~ x, data = df, fct = DRCinvpoly3())
#' summary(mod)
#' plot(mod, log = "")
#' }
#'
#' @export
DRCinvpoly3 <- function() {
  invpoly3.fct <- function(x, parm) {
    invpoly3.fun(x, parm[, 1], parm[, 2], parm[, 3], parm[, 4])
    # x / (parm[, 1] + parm[, 2] * x + parm[, 3] * x^2 + parm[, 4] * x^3)
  }

  invpoly3.ssfct <- function(data) {
    x <- data[, 1]
    y <- data[, 2]

    idx <- y != 0
    x <- x[idx]
    y <- y[idx]

    pseudox <- 1 / x
    pseudoy <- 1 / y

    coefs <- abs(stats::coef(stats::lm(pseudoy ~ pseudox + x + I(x^2))))
    b <- coefs[1]
    a <- coefs[2]
    c <- coefs[3]
    d <- coefs[4]

    c(a, b, c, d)
  }

  invpoly3.init <- function(mCall, LHS, data) {
    data <- stats::sortedXyData(mCall[["x"]], LHS, data)
    invpoly3.ssfct(data)
  }

  structure(
    list(
      invpoly3.fct,
      invpoly3.ssfct,
      c("a", "b", "c", "d"),
      "Third-order inverse polynomial model"
    ),
    names = c("fct", "ssfct", "names", "text"),
    class = "drcMean"
  )
}


#' @rdname kostmod
#' @order 3
#' @examples
#' \dontrun{
#' mod = drc::drm(y ~ x, data = df, fct = DRCkostmod())
#' summary(mod)
#' plot(mod, log = "")
#' }
#'
#' @export
DRCkostmod <- function() {
  kostmod.fct <- function(x, parm) {
    kostmod.fun(x, parm[, 1], parm[, 2], parm[, 3])
    # parm[, 1] * x^parm[, 2] + parm[, 3]
  }

  kostmod.ssfct <- function(data) {
    x <- data[, 1]
    y <- data[, 2]

    fc <- min(y) * 0.999
    yadj <- pmax(y - fc, .Machine$double.eps)

    coefs <- stats::coef(stats::lm(log(yadj) ~ log(x)))
    alfa <- exp(coefs[1])
    beta <- coefs[2]

    c(alfa, beta, fc)
  }

  kostmod.init <- function(mCall, LHS, data) {
    data <- stats::sortedXyData(mCall[["x"]], LHS, data)
    kostmod.ssfct(data)
  }

  structure(
    list(
      kostmod.fct,
      kostmod.ssfct,
      c("alfa", "beta", "fc"),
      "Modified Kostiakov infiltration model"
    ),
    names = c("fct", "ssfct", "names", "text"),
    class = "drcMean"
  )
}
