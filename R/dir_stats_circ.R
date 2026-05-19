#' Circular (directional) descriptive statistics
#'
#' Computes descriptive statistics for a sample of angular measurements
#' (azimuths or orientations) using circular statistics. Optionally pulls the
#' data column from a data frame. Handles both vectorial data (azimuths,
#' 0°–360°) and axial data (strikes or lineations, 0°–180°) via the \code{dir}
#' argument, and returns the mean direction, resultant length, circular
#' variance, circular standard deviation, circular dispersion, concentration
#' parameter, and a confidence cone for the mean direction.
#'
#' @param x Numeric vector of angles in degrees, or an unquoted column name
#'   when `data` is supplied.
#' @param data A data frame (or tibble) containing the column referenced by
#'   `x`. Default \code{NULL} (use \code{x} directly as a vector).
#' @param dir Integer. Data type: \code{1} for vectorial data (full 360° range,
#'   default); \code{0} for axial data (180° range, e.g. strike or foliation—
#'   angles are doubled internally and the mean is halved back).
#' @param conf.level Numeric. Confidence level for the mean-direction cone of
#'   confidence. Default \code{0.95}.
#'
#' @returns A one-row data frame with the following columns:
#'   \describe{
#'     \item{`theta.hat`}{Mean direction in degrees (0°–360°).}
#'     \item{`N`}{Sample size.}
#'     \item{`R`}{Length of the resultant vector.}
#'     \item{`Rbar`}{Mean resultant length (R / N); ranges from 0 (uniform) to
#'       1 (perfectly concentrated).}
#'     \item{`circ.var`}{Circular variance (1 − Rbar).}
#'     \item{`circ.sd`}{Circular standard deviation.}
#'     \item{`circ.disp`}{Circular dispersion.}
#'     \item{`kappa`}{Estimated concentration parameter of the von Mises
#'       distribution.}
#'     \item{`cone`}{Half-width of the \code{conf.level} confidence cone around the
#'       mean direction (degrees).}
#'   }
#'
#' @references Fisher, N. I. (1993). Statistical analysis of circular data. Cambridge University Press.
#' @references Davis, J. C. (2002). Statistics and data analysis in geology (3rd ed). J. Wiley.
#' @references Swan, A. R. H., & Sandilands, M. (1995). Introduction to geological data analysis. Blackwell Science.
#'
#' @family directional statistics
#' @export
#' @examples
#' angles = c(255, 239, 222, 231, 199, 271, 222, 274, 228, 246,
#'            177, 199, 257, 201, 237, 209, 216, 180, 182, 250,
#'            219, 196, 197, 246, 218, 235, 232, 243, 232, 180,
#'            231, 254, 242, 149, 212, 210, 230, 205, 220, 268)
#' # Vectorial data supplied as a plain vector
#' dir_stats_circ(angles)
#'
#' # Axial data (strikes)
#' dir_stats_circ(angles, dir = 0)
#'
#' # Using tidy column selection from a data frame
#' df <- data.frame(azi = angles)
#' dir_stats_circ(azi, data = df, conf.level = 0.90)
dir_stats_circ = function(x, data = NULL, dir = 1, conf.level = 0.95) {
  if (is.null(data)) {
    theta = x
  } else {
    theta = data %>% dplyr::pull({{ x }})
  }

  N = length(theta)

  circ_disp = function(theta) {
    thetarad = theta * pi / 180
    tm1 = CircStats::trig.moment(thetarad, p = 1)
    tm2 = CircStats::trig.moment(thetarad, p = 2)
    delta = (1 - tm2[[2]]) / (2 * tm1[[2]]^2)
    delta
  }

  if (dir == 0) {
    theta = theta * 2
  }
  thetarad = theta * pi / 180
  x = sum(sin(thetarad))
  y = sum(cos(thetarad))
  meanrad = atan(x / y)
  meandeg = meanrad * 180 / pi
  meantrue = dplyr::case_when(
    x > 0 & y > 0 ~ meandeg,
    x < 0 & y > 0 ~ meandeg + 360,
    .default = meandeg + 180
  )
  meantrue = ifelse(dir == 0, meantrue / 2, meantrue)

  R = sqrt(x^2 + y^2)
  Rbar = R / N
  V = 1 - Rbar
  v = sqrt(-2 * log(Rbar))
  delta = circ_disp(theta)
  k = dplyr::case_when(
    Rbar < .53 ~ 2 * Rbar + Rbar^3 + 5 / 6 * Rbar^5,
    Rbar < .85 ~ -.4 + 1.39 * Rbar + .43 / (1 - Rbar),
    .default = (Rbar^3 - 4 * Rbar^2 + 3 * Rbar)^(-1)
  )

  a = 1 - conf.level
  se = (1 / sqrt(N * Rbar * k))
  se = ifelse(dir == 0, se / 2, se)
  cono = suppressWarnings(asin(se * stats::qnorm(1 - a / 2)) * 180 / pi)
  if (is.na(cono)) {
    se = (1 / sqrt(N * Rbar * k)) * 180 / pi
    se = ifelse(dir == 0, se / 2, se)
    cono = (se * stats::qnorm(1 - a / 2))
  }

  sigma_c = sqrt(delta / N)
  cone_c = asin(sigma_c * stats::qnorm(1 - a / 2)) * 180 / pi

  cono_sup = meantrue + cono
  cono_inf = meantrue - cono
  cono_sup = ifelse(cono_sup > 360, cono_sup - 360, cono_sup)
  cono_inf = ifelse(cono_inf < 0, 360 + cono_inf, cono_inf)

  dir.stats = data.frame(
    theta.hat = round(meantrue, 1),
    N = N,
    R = signif(R, 4),
    Rbar = signif(Rbar, 4),
    circ.var = signif(V, 4),
    circ.sd = signif(v, 4),
    circ.disp = signif(delta, 4),
    kappa = signif(k, 4),
    # lower = round(cono_inf, 1),
    # upper = round(cono_sup, 1),
    cone = round(cono, 2)
  )
  return(dir.stats)
}
