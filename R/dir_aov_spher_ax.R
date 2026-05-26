#' Test for common axis direction (axial)
#'
#' Tests whether two or more groups of 3-D orientations share a common
#' principal or polar axis. The dominant fabric type—cluster (point) or
#' girdle—is determined from the pooled PGR indices: if the point index
#' dominates the *principal* eigenvector (V1) is tested; if the girdle index
#' dominates the *polar* eigenvector (V3) is tested. When the pooled data are
#' essentially random no formal test is performed. If the null hypothesis is
#' not rejected the pooled axis and its confidence cone are overlaid in the
#' stereonet. Per-group axes and their confidence cones are always shown in
#' distinct colours.
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
#' @param conf.level Numeric. Confidence level for per-group and pooled axis
#'   confidence cones. Default \code{0.95}.
#' @param type Character. How to interpret the input orientations:
#'   \describe{
#'     \item{`"line"`}{(default) Trend and plunge of a lineation.}
#'     \item{`"dir"`}{Dip direction and dip angle of a plane (pole used
#'       internally).}
#'     \item{`"strike"`}{Strike (right-hand rule) and dip angle of a plane
#'       (pole used internally).}
#'   }
#'
#' @returns A named list whose components depend on the pooled fabric type:
#'   \describe{
#'     \item{`PGR`}{Tibble of pooled fabric indices: \code{P} (point), \code{G} (girdle),
#'       \code{R} (random), \code{B} (= 1 − R).}
#'     \item{`statement`}{Character string summarising the test conclusion.
#'       One of: `"Samples come from a distribution with common principal/polar
#'       axis"`, `"Principal/Polar axis differs between samples"`, or
#'       `"Pooled data follows a random distribution"` (when R dominates).}
#'     \item{`axial_g`}{Tibble with per-group axis statistics: group label \code{grp},
#'       sample size \code{n}, trend \code{trd}, plunge \code{plg}, confidence cone half-width
#'       \code{cone} (degrees), concentration parameter \code{kappa}, eigenvalue \code{lambda},
#'       and normalised eigenvalue \code{S}. Present only when the data are not
#'       random.}
#'     \item{`axial_res`}{Tibble with the test statistic \code{Nr}, degrees of
#'       freedom \code{df}, and p-value \code{p.value}. When the null hypothesis is not
#'       rejected, also includes the pooled axis trend \code{trd}, plunge \code{plg},
#'       confidence cone \code{cone}, \code{kappa}, sample size \code{N}, \code{lambda}, and \code{S}.
#'       Present only when the data are not random.}
#'     \item{`pgrplot`}{A ggtern Point–Girdle–Random ternary diagram.
#'       Present only when the null hypothesis is not rejected.}
#'     \item{`stereoplot`}{A ggplot equal-area stereonet showing data points
#'       per group, per-group axes (coloured squares) with confidence cones,
#'       and—when the null is not rejected—the pooled axis with its confidence
#'       cone (red for principal, blue for polar axis).}
#'   }
#'
#' @references Fisher, N. I., Lewis, T., & Embleton, B. J. J. (1987). Statistical analysis of spherical data. Cambridge University Press.
#'
#' @family directional statistics
#' @export
#' @examples
#' # From a data frame
#' csv_path <- system.file("spherical_data_ax_bp_aov.csv", package = "GMisc")
#' df <- utils::read.csv(csv_path)
#' dir_aov_spher_ax(trend, plunge, gr = type, data = df)
dir_aov_spher_ax = function(
  trd,
  plg,
  grp,
  data = NULL,
  conf.level = .95,
  type = c('line', 'dir', 'strike')
) {
  type = match.arg(type)

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

  if (type == 'line') {
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
  dat.plot = StCoordLineD(d.plot, i.plot) %>%
    dplyr::mutate(grp = grp)

  N = length(d)
  drad = d * pi / 180
  irad = i * pi / 180
  x = cos(irad) * cos(drad)
  y = cos(irad) * sin(drad)
  z = sin(irad)
  cosines = data.frame(x, y, z) %>% as.matrix()
  dat.df = data.frame(d, i, grp = grp)

  axial_pooled = dir_stats_spher(d, i, dat.df, conf.level = conf.level)

  dat.df.g = dat.df %>%
    dplyr::nest_by(.data$grp) %>%
    dplyr::mutate(
      N = nrow(data),
      tau = list(dir_stats_spher(
        d,
        i,
        data,
        conf.level = conf.level
      )$eigen_values[1, ]),
      tauh = list(dir_stats_spher(
        d,
        i,
        data,
        conf.level = conf.level
      )$eigen_values[2, ]),
      tp = list(
        dir_stats_spher(d, i, data, conf.level = conf.level)$trend_plunge
      ),
      axc = list(
        dir_stats_spher(d, i, data, conf.level = conf.level)$axis_stats_watson
      )
    )

  a11 = sum(x^2)
  a12 = sum(x * y)
  a13 = sum(x * z)
  a22 = sum(y^2)
  a33 = sum(z^2)
  a23 = sum(y * z)
  D = matrix(c(a11, a12, a13, a12, a22, a23, a13, a23, a33), nrow = 3)
  stress = eigen(D)
  eigval = as.data.frame(stress[1])
  names(eigval) = "lambda"
  eigval$S = sapply(1:3, function(x) eigval[x, 1] / sum(eigval))
  s = eigval$S
  row.names(eigval) = c("V1", "V2", "V3")
  eigvec = as.data.frame(stress[2])
  correctedvec = ifelse(eigvec[3, ] < 0, -eigvec[1:3, ], eigvec[1:3, ])
  eigvec = as.data.frame(correctedvec)
  names(eigvec) = c("V1", "V2", "V3")
  row.names(eigvec) = c("x", "y", "z")

  PGR = tibble::tibble(
    P = s[1] - s[2],
    G = 2 * (s[2] - s[3]),
    R = 3 * s[3],
    B = 1 - .data$R
  )

  if (PGR$P > PGR$G) {
    sig_c = ((cosines %*% eigvec[, 1])^4 %>% sum()) / nrow(cosines)
    g = 2 * (s[1] - sig_c) / (3 * s[1] - 1)
    Nr = (((dat.df.g$tau %>%
      dplyr::bind_rows() %>%
      dplyr::pull(.data$V1) %>%
      sum()) -
      eigval$lambda[1]) /
      g) %>%
      abs()
    tp.g = dat.df.g$tp %>%
      purrr::map(~ .x[, 1]) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(
        cone = dat.df.g$axc %>%
          purrr::map(~ .x[, 1]) %>%
          dplyr::bind_rows() %>%
          dplyr::pull(),
        kappa = dat.df.g$axc %>%
          purrr::map(~ .x[, 4]) %>%
          dplyr::bind_rows() %>%
          dplyr::pull(),
        lambda = dat.df.g$tau %>% dplyr::bind_rows() %>% dplyr::pull(.data$V1),
        S = dat.df.g$tauh %>% dplyr::bind_rows() %>% dplyr::pull(.data$V1),
        grp = dat.df.g$grp,
        n = dat.df.g$N
      ) %>%
      dplyr::relocate(.data$grp, .data$n)
    axist = 'Principal'
  } else {
    sig_c = ((cosines %*% eigvec[, 3])^4 %>% sum()) / nrow(cosines)
    g = 2 * (s[3] - sig_c) / (1 - 3 * s[3])
    Nr = (((dat.df.g$tau %>%
      dplyr::bind_rows() %>%
      dplyr::pull(.data$V3) %>%
      sum()) -
      eigval$lambda[3]) /
      g) %>%
      abs()
    tp.g = dat.df.g$tp %>%
      purrr::map(~ .x[, 3]) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(
        cone = dat.df.g$axc %>%
          purrr::map(~ .x[, 3]) %>%
          dplyr::bind_rows() %>%
          dplyr::pull(),
        kappa = dat.df.g$axc %>%
          purrr::map(~ .x[, 5]) %>%
          dplyr::bind_rows() %>%
          dplyr::pull(),
        lambda = dat.df.g$tau %>% dplyr::bind_rows() %>% dplyr::pull(.data$V3),
        S = dat.df.g$tauh %>% dplyr::bind_rows() %>% dplyr::pull(.data$V3),
        grp = dat.df.g$grp,
        n = dat.df.g$N
      ) %>%
      dplyr::relocate(.data$grp, .data$n)
    axist = 'Polar'
  }

  v = 2 * nrow(dat.df.g) - 2
  chi.crit = stats::qchisq(1 - a, v)

  if (Nr < chi.crit) {
    q = asin(sqrt(chi.crit) * sqrt(g / (2 * N))) * 180 / pi
    statement = paste(
      'Samples come from a distribution with common',
      tolower(axist),
      'axis'
    )
  } else {
    statement = paste(axist, 'axis differs between samples')
  }

  if (PGR$P > PGR$G) {
    axial_axis = axial_pooled$trend_plunge[, 1]
    axial_col = 'red'
    tau = axial_pooled$eigen_values[1, 1]
    tauh = axial_pooled$eigen_values[2, 1]
    k = axial_pooled$axis_stats_watson[[4]]
  } else {
    axial_axis = axial_pooled$trend_plunge[, 3]
    axial_col = 'blue'
    tau = axial_pooled$eigen_values[1, 3]
    tauh = axial_pooled$eigen_values[2, 3]
    k = axial_pooled$axis_stats_watson[[5]]
  }

  eig.plot = tp.g %>%
    dplyr::mutate(
      trd = ifelse(plg < 0, ifelse(trd + 180 > 360, trd - 180, trd + 180), trd),
      plg = ifelse(plg < 0, -plg, plg)
    ) %>%
    with(StCoordLineD(trd, plg)) %>%
    dplyr::mutate(grp = tp.g$grp)

  eig.cone.plot = tp.g %>%
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

  pgrplot = ggtern::ggtern(PGR, ggtern::aes(.data$R, .data$P, .data$G)) +
    ggplot2::geom_point(col = 'dodgerblue', size = 2) +
    ggplot2::theme_bw() +
    ggtern::theme_tropical() +
    ggtern::theme_arrowdefault() +
    ggtern::theme_showgrid() +
    ggtern::theme_clockwise() +
    ggtern::theme_rotate() +
    ggtern::theme_hidemask()

  eigen.shape = 'square' # 8

  if (all(PGR$R > c(PGR$P, PGR$G))) {
    stereoplot = ggstereo() +
      ggplot2::geom_point(
        ggplot2::aes(.data$xp, .data$yp, col = .data$grp, shape = .data$grp),
        dat.plot,
        size = 2,
        alpha = .5
      ) +
      ggplot2::scale_color_brewer(palette = 'Dark2')
  } else if (Nr < chi.crit) {
    stereoplot = ggstereo() +
      ggplot2::geom_point(
        ggplot2::aes(.data$xp, .data$yp, col = .data$grp, shape = .data$grp),
        dat.plot,
        size = 2,
        alpha = .5
      ) +
      ggplot2::geom_path(
        ggplot2::aes(.data$xp, .data$yp, col = .data$grp, group = .data$path),
        eig.cone.plot
      ) +
      ggplot2::geom_point(
        ggplot2::aes(.data$xp, .data$yp, col = .data$grp, shape = .data$grp),
        eig.plot,
        size = 4,
        alpha = .7
      ) +
      ggplot2::geom_path(
        ggplot2::aes(.data$xp, .data$yp, group = .data$path),
        SmallCircleD(axial_axis[[1]], axial_axis[[2]], q, closed = T) %>%
          dplyr::bind_rows(.id = 'path'),
        col = axial_col
      ) +
      ggplot2::geom_point(
        ggplot2::aes(.data$xp, .data$yp),
        StCoordLineD(axial_axis[[1]], axial_axis[[2]]),
        size = 4,
        alpha = .7,
        col = axial_col,
        shape = eigen.shape,
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
        eig.cone.plot
      ) +
      ggplot2::geom_point(
        ggplot2::aes(.data$xp, .data$yp, col = .data$grp, shape = .data$grp),
        eig.plot,
        size = 4,
        alpha = .7
      ) +
      ggplot2::scale_color_brewer(palette = 'Dark2')
  }

  if (all(PGR$R > c(PGR$P, PGR$G))) {
    res.lst = list(
      PGR = PGR,
      statement = 'Pooled data follows a random distribution',
      pgrplot = pgrplot,
      stereoplot = stereoplot
    )
  } else if (Nr < chi.crit) {
    res.lst = list(
      PGR = PGR,
      statement = statement,
      axial_g = tp.g,
      axial_res = tibble::tibble(
        sigma = sig_c,
        g = g,
        Nr = Nr,
        df = v,
        p.value = stats::pchisq(Nr, v, lower.tail = F),
        trd = axial_axis[[1]],
        plg = axial_axis[[2]],
        cone = q,
        kappa = k,
        N = N,
        lambda = tau,
        S = tauh
      ),
      pgrplot = pgrplot,
      stereoplot = stereoplot
    )
  } else {
    res.lst = list(
      PGR = PGR,
      statement = statement,
      axial_g = tp.g,
      axial_res = tibble::tibble(
        sigma = sig_c,
        g = g,
        Nr = Nr,
        df = v,
        p.value = stats::pchisq(Nr, v, lower.tail = F),
        # k = k,
        lambda = tau,
        S = tauh
      ),
      # pgrplot = pgrplot,
      stereoplot = stereoplot
    )
  }

  return(res.lst)
}
