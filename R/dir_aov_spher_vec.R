#' Test for common mean direction (vectorial)
#'
#' Tests whether two or more groups of 3-D orientations share a common mean
#' direction applicable to spherical data with multiple groups. The test
#' compares the within-group mean vectors via a weighted sum of squared
#' discrepancies and returns a conclusion statement, per-group vector statistics, the pooled test result, and an equal-area stereonet.
#'
#' When the null hypothesis is not rejected (samples share a common mean), the pooled mean direction and its confidence cone are overlaid in gold on the
#' stereonet. Per-group mean directions and their confidence cones are always
#' shown in distinct colours.
#'
#' @param trd Numeric vector of trend values in degrees, or an unquoted column
#'   name when \code{data} is supplied. For lines: trend of the lineation. For
#'   planes: dip direction or strike depending on \code{type}.
#' @param plg Numeric vector of plunge values in degrees, or an unquoted column
#'   name when \code{data} is supplied. For lines: plunge. For planes: dip angle.
#' @param grp A vector (factor or character) of group labels, or an unquoted
#'   column name when \code{data} is supplied. Must be the same length as `trd` and
#'   `plg`.
#' @param data A data frame (or tibble) containing the columns referenced by
#'   \code{trd}, \code{plg}, and \code{grp}. Default \code{NULL} (use vectors directly).
#' @param conf.level Numeric. Confidence level for per-group and pooled mean
#'   direction confidence cones. Default \code{0.95}.
#' @param type Character. How to interpret the input orientations:
#'   \describe{
#'     \item{`"line"`}{(default) Trend and plunge of a lineation.}
#'     \item{`"dir"`}{Dip direction and dip angle of a plane (pole used
#'       internally).}
#'     \item{`"strike"`}{Strike (right-hand rule) and dip angle of a plane
#'       (pole used internally).}
#'   }
#'
#' @returns A named list with four components:
#'   \describe{
#'     \item{`statement`}{Character string summarising the test conclusion.}
#'     \item{`vector_g`}{Tibble with per-group statistics: group label, sample
#'       size \code{N}, resultant length \code{R}, mean resultant length \code{Rbar},
#'       concentration parameter \code{kappa}, mean trend \code{trd}, mean plunge \code{plg},
#'       and confidence cone half-width \code{cone} (degrees).}
#'     \item{`vector_res`}{Tibble with the test statistic \code{Nr}, degrees of
#'       freedom \code{df}, and p-value \code{p.value}. When the null hypothesis is not
#'       rejected, the pooled mean direction statistics are appended as additional columns.}
#'     \item{`stereoplot`}{A ggplot equal-area stereonet showing the data
#'       points, per-group mean directions (coloured triangles) with confidence
#'       cones, and—when the null is not rejected—the pooled mean direction
#'       (gold triangle) with its confidence cone.}
#'   }
#'
#' @references Fisher, N. I., Lewis, T., & Embleton, B. J. J. (1987). Statistical analysis of spherical data. Cambridge University Press.
#'
#' @family directional statistics
#' @export
#' @examples
#' set.seed(123)
#' trds <- c(runif(10, 50, 70), runif(10, 70, 90))
#' plgs <- c(runif(10, 20, 40), runif(10, 30, 50))
#' grps <- rep(c("A","B"), each=10)
#' res <- dir_aov_spher_vec(trds, plgs, grps)
#' res$statement
#' res$vector_res
#' res$stereoplot
#'
#' # From a data frame
#' csv_path <- system.file("spherical_data_vec_aov.csv", package = "GMisc")
#' df <- utils::read.csv(csv_path)
#' dir_aov_spher_vec(trend, plunge, type, data = df)
dir_aov_spher_vec = function(
  trd,
  plg,
  grp,
  data = NULL,
  conf.level = .95,
  type = c('line', 'dir', 'strike')
) {
  a = 1 - conf.level

  if (is.null(data)) {
    trd = trd
    plg = plg
    grp = grp
  } else {
    trd = data %>% dplyr::pull({{ trd }})
    plg = data %>% dplyr::pull({{ plg }})
    grp = data %>% dplyr::pull({{ grp }})
  }

  if (any(type == 'line')) {
    d = trd
    i = plg
  } else if (type == 'dir') {
    d = trd + 180
    i = 90 - plg
  } else if (type == 'strike') {
    d = trd - 90
    i = 90 - plg
  }

  d.plot = ifelse(i < 0, ifelse(d + 180 > 360, d - 180, d + 180), d)
  i.plot = ifelse(i < 0, -i, i)

  N = length(d)

  dat.plot = StCoordLineD(d.plot, i.plot) %>%
    dplyr::mutate(grp = grp)

  dat.df = data.frame(d, i, grp = grp)

  vector_pooled = dir_stats_spher(d, i, dat.df, conf.level = conf.level)

  dat.vec.aov = dat.df %>%
    dplyr::nest_by(.data$grp) %>%
    dplyr::mutate(
      cosines = list(with(
        data,
        SphToCartD(d, i, 0) %>%
          tibble::as_tibble()
      )),
      N = nrow(data),
      R = with(.data$cosines, sqrt(sum(cn)^2 + sum(ce)^2 + sum(cd)^2)),
      Rbar = .data$R / N,
      kappa = ifelse(
        N < 16,
        (N / (N - .data$R)) * (1.0 - (1.0 / N))^2,
        (N - 1.0) / (N - .data$R)
      ),
      xh = with(.data$cosines, sum(cn) / .data$R),
      yh = with(.data$cosines, sum(ce) / .data$R),
      zh = with(.data$cosines, sum(cd) / .data$R),
      sigc = sqrt(
        (1 -
          ((as.matrix(.data$cosines) %*% c(.data$xh, .data$yh, .data$zh))^2 %>%
            sum()) /
            N) /
          (N * (.data$R / N)^2)
      ),
      mean = list(CartToSphD(.data$xh, .data$yh, .data$zh)),
      cone = asin(sqrt(-log(a)) * .data$sigc) * 180 / pi
    ) %>%
    tidyr::unnest_wider(mean) %>%
    dplyr::select(-c(data, .data$cosines)) %>%
    dplyr::ungroup()

  vec_res = dat.vec.aov %>%
    dplyr::summarise(
      x = sum(.data$xh / .data$sigc^2),
      y = sum(.data$yh / .data$sigc^2),
      z = sum(.data$zh / .data$sigc^2),
      s = sum(1 / .data$sigc^2),
      v = 2 * nrow(dat.vec.aov) - 2 # degrees of freedom
    ) %>%
    dplyr::summarise(
      Nr = abs(4 * (.data$s - sqrt(.data$x^2 + .data$y^2 + .data$z^2))),
      df = .data$v,
      p.value = stats::pchisq(.data$Nr, .data$v, lower.tail = F)
    )

  vec.plot = dat.vec.aov %>%
    dplyr::mutate(
      trd = ifelse(plg < 0, ifelse(trd + 180 > 360, trd - 180, trd + 180), trd),
      plg = ifelse(plg < 0, -plg, plg)
    ) %>%
    with(StCoordLineD(trd, plg)) %>%
    dplyr::mutate(grp = dat.vec.aov$grp)

  vec.cone.plot = dat.vec.aov %>%
    dplyr::mutate(
      trd = ifelse(plg < 0, ifelse(trd + 180 > 360, trd - 180, trd + 180), trd),
      plg = ifelse(plg < 0, -plg, plg)
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      sc = list(
        SmallCircleD(trd, plg, .data$cone) %>%
          dplyr::bind_rows(.id = 'path')
      )
    ) %>%
    tidyr::unnest(.data$sc) %>%
    dplyr::mutate(path = paste0(.data$path, grp))

  v = 2 * nrow(dat.vec.aov) - 2
  chi.crit = stats::qchisq(1 - a, v)

  if (vec_res$Nr < chi.crit) {
    q = vector_pooled$vector_stats
    q2 = q %>%
      dplyr::mutate(
        trd = ifelse(
          plg < 0,
          ifelse(trd + 180 > 360, trd - 180, trd + 180),
          trd
        ),
        plg = ifelse(plg < 0, -plg, plg)
      )
    statement = 'Samples come from a distribution with common mean direction'
  } else {
    statement = 'Mean direction differs between samples'
  }

  mean.shape = 'triangle' # 8

  if (vec_res$Nr < chi.crit) {
    stereoplot = ggstereo() +
      ggplot2::geom_point(
        ggplot2::aes(.data$xp, .data$yp, col = .data$grp, shape = .data$grp),
        dat.plot,
        size = 2,
        alpha = .5
      ) +
      ggplot2::geom_path(
        ggplot2::aes(.data$xp, .data$yp, col = .data$grp, group = .data$path),
        vec.cone.plot
      ) +
      ggplot2::geom_point(
        ggplot2::aes(.data$xp, .data$yp, col = .data$grp, shape = .data$grp),
        vec.plot,
        size = 4,
        alpha = .7
      ) +
      ggplot2::geom_path(
        ggplot2::aes(.data$xp, .data$yp, group = .data$path),
        SmallCircleD(q2$trd, q2$plg, q2$cone, closed = T) %>%
          dplyr::bind_rows(.id = 'path'),
        col = 'gold'
      ) +
      ggplot2::geom_point(
        ggplot2::aes(.data$xp, .data$yp),
        StCoordLineD(q2$trd, q2$plg),
        size = 4,
        alpha = .7,
        col = 'gold',
        shape = mean.shape,
        stroke = .75
      ) +
      ggplot2::scale_color_brewer(palette = 'Dark2')
  } else {
    stereoplot = ggstereo() +
      ggplot2::geom_point(
        ggplot2::aes(.data$xp, .data$yp, col = .data$grp, shape = .data$grp),
        dat.plot,
        size = 2,
        alpha = .5
      ) +
      ggplot2::geom_path(
        ggplot2::aes(.data$xp, .data$yp, col = .data$grp, group = .data$path),
        vec.cone.plot
      ) +
      ggplot2::geom_point(
        ggplot2::aes(.data$xp, .data$yp, col = .data$grp, shape = .data$grp),
        vec.plot,
        size = 4,
        alpha = .7
      ) +
      ggplot2::scale_color_brewer(palette = 'Dark2')
  }

  if (vec_res$Nr < chi.crit) {
    res.lst = list(
      statement = statement,
      vector_g = dat.vec.aov %>% dplyr::select(-dplyr::ends_with('h')),
      vector_res = dplyr::bind_cols(vec_res, q),
      stereoplot = stereoplot
    )
  } else {
    res.lst = list(
      statement = statement,
      vector_g = dat.vec.aov %>% dplyr::select(-dplyr::ends_with('h')),
      vector_res = vec_res,
      stereoplot = stereoplot
    )
  }

  return(res.lst)
}
