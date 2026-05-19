#' Spherical (3-D directional) descriptive statistics
#'
#' Computes descriptive statistics for a sample of 3-D orientations (lines or
#' planes) using spherical statistics. The function calculates the mean
#' resultant vector, concentration parameter, and confidence cone for the mean
#' direction, performs an eigenvalue / eigenvector decomposition of the
#' orientation matrix, estimates distribution parameters for Watson bipolar,
#' Watson girdle, and Bingham distributions, and returns several diagnostic
#' plots (stereonet, eigenvalue ratio plot, and PGR ternary diagram).
#'
#' @param trd Numeric vector of trend values in degrees, or an unquoted column
#'   name when \code{data} is supplied. For lines: trend of the lineation. For
#'   planes: dip direction or strike depending on \code{type}.
#' @param plg Numeric vector of plunge values in degrees, or an unquoted column
#'   name when \code{data} is supplied. For lines: plunge of the lineation. For
#'   planes: dip angle.
#' @param data A data frame (or tibble) containing the columns referenced by
#'   \code{trd} and \code{plg}. Default \code{NULL} (use vectors directly).
#' @param conf.level Numeric. Confidence level for mean-direction and
#'   eigenvector confidence cones/ellipses. Default \code{0.95}.
#' @param type Character. How to interpret the input orientations:
#'   \describe{
#'     \item{`"line"`}{(default) Trend and plunge of a lineation.}
#'     \item{`"dir"`}{Dip direction and dip angle of a plane (converted
#'       internally to the pole).}
#'     \item{`"strike"`}{Strike (right-hand rule) and dip angle of a plane
#'       (converted internally to the pole).}
#'   }
#' @param plot Character. Which stereonet to include in the output:
#'   \describe{
#'     \item{`"all"`}{(default) Data points, mean vector with confidence cone
#'       (gold), and all three eigenvectors with their confidence cones (red,
#'       green, blue).}
#'     \item{`"eig"`}{Data points and eigenvectors with confidence cones only.}
#'     \item{`"mean"`}{Data points and mean vector with confidence cone only.}
#'   }
#'
#' @returns A named list with the following components:
#'   \describe{
#'     \item{`eigen_values`}{1 × 3 matrix of normalised eigenvalues
#'       \eqn{S_1 \ge S_2 \ge S_3} of the orientation matrix.}
#'     \item{`eigen_vectors`}{3 × 3 data frame of eigenvectors (direction
#'       cosines), columns V1–V3.}
#'     \item{`orientation_matrix`}{3 × 3 orientation (scatter) matrix.}
#'     \item{`trend_plunge`}{2 × 3 matrix with trend (row 1) and plunge (row 2)
#'       of each eigenvector in degrees.}
#'     \item{`dir_dip`}{2 × 3 matrix with dip direction (row 1) and dip (row 2)
#'       of each eigenvector in degrees.}
#'     \item{`vector_stats`}{Tibble with mean direction statistics: \code{trd},
#'       \code{plg}, \code{strike}, \code{dir}, \code{dip}, resultant length \code{R}, mean resultant
#'       length \code{Rbar}, concentration parameter \code{kappa}, confidence cone
#'       half-width \code{cone} (degrees), and sample size \code{N}.}
#'     \item{`axis_stats_watson`}{Tibble with confidence cone half-widths for
#'       each eigenvector (\code{V1_cone}, \code{V2_cone}, \code{V3_cone}) and estimated
#'       concentration parameters for the Watson bipolar (\code{kappa_bipolar}) and
#'       Watson girdle (\code{kappa_girdle}) distributions.}
#'     \item{`axis_stats_aniso`}{Data frame (3 × 3) with Bingham κ values and
#'       Watson confidence ellipse semi-axes (degrees) for each eigenvector.}
#'     \item{`PointGirdleRandom`}{Tibble of fabric indices: \code{P} (point/cluster),
#'       \code{G} (girdle), \code{R} (random), \code{B} (= 1 − R), eigenvalue ratios
#'       \eqn{S_1/S_2}, \eqn{S_1/S_3}, \eqn{S_2/S_3}, log-ratios \code{r1}
#'       and \code{r2}, Woodcock's shape \code{K} and strength \code{C} parameters, intensity index \code{I}, and
#'       distance index \code{D}.}
#'     \item{`eigenplot`}{A ggplot object: log-ratio eigenvalue plot
#'       (\eqn{\ln(S_2/S_3)} vs \eqn{\ln(S_1/S_2)}) with K-lines and
#'       C-isolines.}
#'     \item{`pgrplot`}{A ggtern object: Point–Girdle–Random ternary diagram.}
#'     \item{`stereoplot`}{A ggplot equal-area stereonet according to the
#'       \code{plot} argument.}
#'   }
#'
#'
#' @references Fisher, N. I., Lewis, T., & Embleton, B. J. J. (1987). Statistical analysis of spherical data. Cambridge University Press.
#' @references Mardia, K. V., & Jupp, P. E. (2000). Directional Statistics. John Wiley & Sons.
#' @references Woodcock, N. H. (1977). Specification of fabric shapes using an eigenvalue method. Geological Society of America Bulletin, 88(9), 1231–1236. https://doi.org/10.1130/0016-7606(1977)88%253C1231:SOFSUA%253E2.0.CO;2
#' @references Vollmer, F. W. (1990). An application of eigenvalue methods to structural domain analysis. Geological Society of America Bulletin, 102(6), 786–791. https://doi.org/10.1130/0016-7606(1990)102%253C0786:AAOEMT%253E2.3.CO;2
#' @references Lisle, R. J. (1985). The use of the orientation tensor for the description and statistical testing of fabrics. Journal of Structural Geology, 7(1), 115–117. https://doi.org/10.1016/0191-8141(85)90119-1
#'
#' @family directional statistics
#' @export
#' @examples
#' # Lines supplied as plain vectors
#' set.seed(123)
#' trds <- runif(50, 40, 80)
#' plgs <- runif(50, 10, 50)
#' res <- dir_stats_spher(trds, plgs)
#' res$vector_stats
#' res$stereoplot
#'
#' # Planes given as dip direction / dip, eigenvectors only on stereonet
#' dir_stats_spher(trds, plgs, type = "dir", plot = "eig")
#'
#' # From a data frame
#' csv_path <- system.file("spherical_data_ax_bp.csv", package = "GMisc")
#' df <- utils::read.csv(csv_path)
#' res2 <- dir_stats_spher(dip_direction, dip, data = df,
#'                         conf.level = 0.90, type = "dir")
#' res2$axis_stats_watson
#' res2$eigenplot
dir_stats_spher = function(
  trd,
  plg,
  data = NULL,
  conf.level = .95,
  type = c('line', 'dir', 'strike'),
  plot = c('all', 'eig', 'mean')
) {
  a = 1 - conf.level

  if (is.null(data)) {
    trd = trd
    plg = plg
  } else {
    trd = data %>% dplyr::pull({{ trd }})
    plg = data %>% dplyr::pull({{ plg }})
  }

  watson_k1 = utils::read.csv(
    system.file("spherical_watson_kappas_bipolar.csv", package = "GMisc")
    # '../inst/spherical_watson_kappas_bipolar.csv'
  )
  watson_k3 = utils::read.csv(
    system.file("spherical_watson_kappas_girdle.csv", package = "GMisc")
  )
  bingham_kappas = utils::read.csv(
    system.file("spherical_bingham_kappas.csv", package = "GMisc")
  )

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
  drad = d * pi / 180
  irad = i * pi / 180
  x = cos(irad) * cos(drad)
  y = cos(irad) * sin(drad)
  z = sin(irad)
  cosines = data.frame(x, y, z) %>% as.matrix()
  Sx = sum(x)
  Sy = sum(y)
  Sz = sum(z)
  R0 = sqrt(Sx^2 + Sy^2 + Sz^2)
  # R0 = sqrt(sum(colSums(cosines)^2))
  Rbar = R0 / N

  Rbar0 = sqrt(
    sum(cos(i.plot * pi / 180) * cos(d.plot * pi / 180))^2 +
      sum(cos(i.plot * pi / 180) * sin(d.plot * pi / 180))^2 +
      sum(sin(i.plot * pi / 180))^2
  ) /
    N

  if (all(i >= 0) & Sz < 0) {
    Sx <- -Sx
    Sy <- -Sy
    Sz <- -Sz
  }

  # h = colSums(cosines)/R0
  xh = Sx / R0
  yh = Sy / R0
  zh = Sz / R0
  res <- CartToSphD(xh, yh, zh)

  d.sig = 1 - ((cosines %*% c(xh, yh, zh))^2 %>% sum()) / N
  sig_s = sqrt(d.sig / (N * Rbar^2))
  cone_s = suppressWarnings(asin(sqrt(-log(a)) * sig_s) * 180 / pi)
  cone_s = ifelse(is.na(cone_s), sqrt(-log(a)) * sig_s * 180 / pi, cone_s)

  if (N < 16) {
    afact <- 1.0 - (1.0 / N)
    conc <- (N / (N - R0)) * afact^2
  } else {
    conc <- (N - 1.0) / (N - R0)
    # conc = Rbar * (3 - Rbar^2)/(1 - Rbar^2)
  }

  mean_dir = ifelse(res[1] + 180 > 360, res[1] - 180, res[1] + 180)
  mean_strike = d2s(mean_dir)
  mean_dip = 90 - res[2]

  # condifence cone for mean vector
  cono = (acos(1 - ((N - R0) / R0) * ((1 / a)^(1 / (N - 1)) - 1))) * 180 / pi

  # eigen
  a11 = sum(x^2)
  a12 = sum(x * y)
  a13 = sum(x * z)
  a22 = sum(y^2)
  a33 = sum(z^2)
  a23 = sum(y * z)
  O = matrix(c(a11, a12, a13, a12, a22, a23, a13, a23, a33), nrow = 3)
  stress = eigen(O)
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

  # confidence cone and ellipse for principal axis of symmetric bipolar distr
  sig_c1 = ((cosines %*% eigvec[, 1])^4 %>% sum()) / nrow(cosines)

  cone.1 = suppressWarnings(
    (asin(
      (sqrt((s[1] - sig_c1) / nrow(cosines)) / (s[1] - s[2])) *
        sqrt(-log(a))
    )) *
      180 /
      pi
  )
  cone.1 = ifelse(is.na(cone.1), 0, cone.1)

  e11.1 = (nrow(cosines) * (s[3] - s[1])^2)^(-1) *
    sum(((cosines %*% eigvec[, 3])^2) * ((cosines %*% eigvec[, 1])^2))
  e22.1 = (nrow(cosines) * (s[2] - s[1])^2)^(-1) *
    sum(((cosines %*% eigvec[, 2])^2) * ((cosines %*% eigvec[, 1])^2))
  e12.1 = (nrow(cosines) * (s[3] - s[1]) * (s[2] - s[1]))^(-1) *
    sum(
      (cosines %*% eigvec[, 3]) *
        (cosines %*% eigvec[, 2]) *
        ((cosines %*% eigvec[, 1])^2)
    )

  E.1 = matrix(c(e11.1, e12.1, e12.1, e22.1), nrow = 2)
  F.1 = solve(E.1)
  A.1 = F.1[1, 1]
  B.1 = F.1[1, 2]
  C.1 = F.1[2, 2]
  D.1 = -2 * log(a) / nrow(cosines)

  Z.1 = matrix(c(A.1, B.1, B.1, C.1), nrow = 2)

  g.1 = sort(sqrt(D.1 / eigen(Z.1)$values), T)

  betas.v1 = suppressWarnings(asin(g.1) * 180 / pi)

  if (anyNA(betas.v1)) {
    betas.v1 = c(0, 0)
  }

  # confidence cone and ellipse for polar axis of symmetric girdle distr
  sig_c3 = ((cosines %*% eigvec[, 3])^4 %>% sum()) / nrow(cosines)

  cone.3 = suppressWarnings(
    (asin(
      (sqrt((s[3] - sig_c3) / nrow(cosines)) / (s[2] - s[3])) *
        sqrt(-log(a))
    )) *
      180 /
      pi
  )
  cone.3 = ifelse(is.na(cone.3), 0, cone.3)

  e11.3 = (nrow(cosines) * (s[3] - s[1])^2)^(-1) *
    sum(((cosines %*% eigvec[, 3])^2) * ((cosines %*% eigvec[, 1])^2))
  e22.3 = (nrow(cosines) * (s[3] - s[2])^2)^(-1) *
    sum(((cosines %*% eigvec[, 3])^2) * ((cosines %*% eigvec[, 2])^2))
  e12.3 = (nrow(cosines) * (s[3] - s[2]) * (s[3] - s[1]))^(-1) *
    sum(
      (cosines %*% eigvec[, 3])^2 *
        (cosines %*% eigvec[, 2]) *
        ((cosines %*% eigvec[, 1]))
    )

  E.3 = matrix(c(e11.3, e12.3, e12.3, e22.3), nrow = 2)
  F.3 = solve(E.3)
  A.3 = F.3[1, 1]
  B.3 = F.3[1, 2]
  C.3 = F.3[2, 2]
  D.3 = -2 * log(a) / nrow(cosines)

  Z.3 = matrix(c(A.3, B.3, B.3, C.3), nrow = 2)

  g.3 = sort(sqrt(D.3 / eigen(Z.3)$values), T)

  betas.v3 = suppressWarnings(asin(g.3) * 180 / pi)

  if (anyNA(betas.v3)) {
    betas.v3 = c(0, 0)
  }

  # confidence cone and ellipse for V2
  sig_c2 = ((cosines %*% eigvec[, 2])^4 %>% sum()) / nrow(cosines)

  cone.2 = suppressWarnings(
    asin(
      (sqrt((s[2] - sig_c2) / nrow(cosines)) / (s[1] - s[2])) *
        sqrt(-log(a))
    ) *
      180 /
      pi
  )
  cone.2 = ifelse(is.na(cone.2), 0, cone.2)

  e11.2 = (nrow(cosines) * (s[2] - s[1])^2)^(-1) *
    sum(((cosines %*% eigvec[, 2])^2) * ((cosines %*% eigvec[, 1])^2))
  e22.2 = (nrow(cosines) * (s[2] - s[3])^2)^(-1) *
    sum(((cosines %*% eigvec[, 2])^2) * ((cosines %*% eigvec[, 3])^2))
  e12.2 = (nrow(cosines) * (s[1] - s[2]) * (s[2] - s[3]))^(-1) *
    sum(
      (cosines %*% eigvec[, 3]) *
        (cosines %*% eigvec[, 1]) *
        ((cosines %*% eigvec[, 2])^2)
    )

  E.2 = matrix(c(e11.2, e12.2, e12.2, e22.2), nrow = 2)
  F.2 = solve(E.2)
  A.2 = F.2[1, 1]
  B.2 = F.2[1, 2]
  C.2 = F.2[2, 2]
  D.2 = -2 * log(a) / nrow(cosines)

  Z.2 = matrix(c(A.2, B.2, B.2, C.2), nrow = 2)

  g.2 = sort(sqrt(D.2 / eigen(Z.2)$values), T)

  betas.v2 = suppressWarnings(asin(g.2) * 180 / pi)

  if (anyNA(betas.v2)) {
    betas.v2 = c(0, 0)
  }

  # kappa Watson bipolar distr
  if (1 / 3 <= s[1] & s[1] <= .34) {
    conc.wb = 3.75 * (3 * s[1] - 1)
  } else if (.34 < s[1] & s[1] <= .64) {
    conc.wb = -5.95 + 14.9 * s[1] + 1.48 * (1 - s[1])^(-1) - 11.05 * s[1]^2
  } else if (s[1] > .64) {
    conc.wb = -7.96 + 21.5 * s[1] + (1 - s[1])^(-1) - 13.25 * s[1]^2
  }

  # kappa Watson girdle distr
  if (0 <= s[3] & s[3] <= .06) {
    conc.wg = -((2 * s[3])^(-1))
  } else if (.06 < s[3] & s[3] <= .32) {
    conc.wg = -(.961 - 7.08 * s[3] + .466 / s[3])
  } else if (.32 < s[3] & s[3] <= 1 / 3) {
    conc.wg = -(3.75 * (1 - 3 * s[3]))
  }

  # bingham kappas
  k1 = 0
  if (s[2] < .02 & s[3] < .02) {
    k2 = -(2 * s[2])^-1
    k3 = -(2 * s[3])^-1
  } else if (s[3] < .02) {
    k2 = with(
      bingham_kappas,
      pracma::interp2(
        seq(.02, .32, .02),
        seq(.02, .48, .02),
        matrix(k2, nrow = 24, ncol = 16),
        .02,
        s[2]
      )
    )
    k3 = -(2 * s[3])^-1
  } else {
    k2 = with(
      bingham_kappas,
      pracma::interp2(
        seq(.02, .32, .02),
        seq(.02, .48, .02),
        matrix(k2, nrow = 24, ncol = 16),
        s[3],
        s[2]
      )
    )
    k3 = with(
      bingham_kappas,
      pracma::interp2(
        seq(.02, .32, .02),
        seq(.02, .48, .02),
        matrix(k3, nrow = 24, ncol = 16),
        s[3],
        s[2]
      )
    )
  }

  #
  TP = matrix(0, nrow = 2, ncol = 3)
  colnames(TP) = c("V1", "V2", "V3")
  row.names(TP) = c("trd", "plg")

  DD = matrix(0, nrow = 2, ncol = 3)
  colnames(DD) = c("V1", "V2", "V3")
  row.names(DD) = c("dir", "dip")

  TP[, 1] = CartToSphD(eigvec[1, 1], eigvec[2, 1], eigvec[3, 1])
  TP[, 2] = CartToSphD(eigvec[1, 2], eigvec[2, 2], eigvec[3, 2])
  TP[, 3] = CartToSphD(eigvec[1, 3], eigvec[2, 3], eigvec[3, 3])

  TP = round(TP, 2)

  for (r in 1:3) {
    DD[1, r] = TP[1, r] + 180
    if (DD[1, r] > 360) {
      DD[1, r] = DD[1, r] - 360
    }
    DD[2, r] = 90 - TP[2, r]
  }

  STATS = tibble::tibble(
    trd = res[1],
    plg = res[2],
    strike = mean_strike,
    dir = mean_dir,
    dip = mean_dip,
    R = R0,
    Rbar = Rbar,
    kappa = conc,
    cone = cono,
    N = N
  )

  trd.plot = ifelse(
    STATS$plg < 0,
    ifelse(STATS$trd > 180, STATS$trd - 180, STATS$trd + 180),
    STATS$trd
  )
  plg.plot = ifelse(STATS$plg < 0, -STATS$plg, STATS$plg)

  PGR = tibble::tibble(
    P = s[1] - s[2],
    G = 2 * (s[2] - s[3]),
    R = 3 * s[3],
    B = 1 - .data$R,
    s1.s2 = s[1] / s[2],
    s1.s3 = s[1] / s[3],
    s2.s3 = s[2] / s[3],
    r1 = log(.data$s2.s3),
    r2 = log(.data$s1.s2),
    K = .data$r2 / .data$r1,
    C = log(.data$s1.s3),
    I = sum((s - 1 / 3)^2) * 15 / 2,
    D = sqrt(sum((s - 1 / 3)^2) * 3 / 2)
  ) %>%
    dplyr::relocate(.data$I, .data$D, .data$K, .data$C, .after = .data$B)

  STATS2 = tibble::tibble(
    V1_cone = cone.1,
    V2_cone = cone.2,
    V3_cone = cone.3,
    kappa_bipolar = conc.wb,
    kappa_girdle = conc.wg
  )

  STATS3 = data.frame(
    V1 = c(k1, betas.v1),
    V2 = c(k2, betas.v2),
    V3 = c(k3, betas.v3)
  ) %>%
    round(3) %>%
    `row.names<-`(c(
      'bingham kappa',
      'watson max ellipse',
      'watson min ellipse'
    ))

  eigenplot = ggplot2::ggplot(
    tibble::tibble(r1 = PGR$r1, r2 = PGR$r2),
    ggplot2::aes(.data$r1, .data$r2)
  ) +
    ggplot2::geom_abline(
      slope = c(.2, .5, .8, 2, 5),
      intercept = 0,
      col = 'purple',
      linewidth = .2,
      linetype = 2
    ) +
    ggplot2::geom_abline(
      slope = -1,
      intercept = c(2, 4, 6),
      col = 'darkgreen',
      linewidth = .2,
      linetype = 2
    ) +
    ggplot2::geom_abline(slope = 1, intercept = 0, col = 'red') +
    ggplot2::geom_point(col = 'dodgerblue', size = 2) +
    ggplot2::labs(
      x = expression(log(s[2] / s[3])),
      y = expression(log(s[1] / s[2]))
    ) +
    ggplot2::coord_fixed(
      xlim = c(0, ceiling(max(PGR[, c('r1', 'r2')]) * 1.1)),
      ylim = c(0, ceiling(max(PGR[, c('r1', 'r2')]) * 1.1)),
      expand = F
    ) +
    ggplot2::theme_bw()

  pgrplot = ggtern::ggtern(PGR, ggtern::aes(.data$R, .data$P, .data$G)) +
    ggplot2::geom_point(col = 'dodgerblue', size = 2) +
    ggplot2::theme_bw() +
    ggtern::theme_tropical() +
    ggtern::theme_arrowdefault() +
    ggtern::theme_showgrid() +
    ggtern::theme_clockwise() +
    ggtern::theme_rotate() +
    ggtern::theme_hidemask()

  mean.shape = 'triangle' # 8
  eigen.shape = 'square'

  if (any(plot == 'all')) {
    stereoplot = ggstereo() +
      # geom_point(ggplot2::aes(x,y),StPointD(d,90-i),size=2,alpha=.5) +
      ggplot2::geom_point(
        ggplot2::aes(.data$xp, .data$yp),
        StCoordLineD(d.plot, i.plot),
        size = 2,
        alpha = .5
      ) +
      ggplot2::geom_path(
        ggplot2::aes(.data$xp, .data$yp, group = .data$path),
        SmallCircleD(trd.plot, plg.plot, cono, closed = T) %>%
          dplyr::bind_rows(.id = 'path'),
        col = 'gold'
      ) +
      ggplot2::geom_point(
        ggplot2::aes(.data$x, .data$y),
        StPointD(trd.plot, 90 - plg.plot),
        col = 'gold',
        size = 3,
        alpha = .7,
        shape = mean.shape,
        stroke = .75
      ) +
      ggplot2::geom_path(
        ggplot2::aes(.data$xp, .data$yp, group = .data$path),
        SmallCircleD(TP[1, 1], TP[2, 1], cone.1, closed = T) %>%
          dplyr::bind_rows(.id = 'path'),
        col = 'red'
      ) +
      ggplot2::geom_point(
        ggplot2::aes(.data$x, .data$y),
        StPointD(TP[1, 1], 90 - TP[2, 1]),
        col = 'red',
        size = 3,
        alpha = .7,
        shape = eigen.shape,
        stroke = .75
      ) +
      ggplot2::geom_path(
        ggplot2::aes(.data$xp, .data$yp, group = .data$path),
        SmallCircleD(TP[1, 2], TP[2, 2], cone.2, closed = T) %>%
          dplyr::bind_rows(.id = 'path'),
        col = 'green3'
      ) +
      ggplot2::geom_point(
        ggplot2::aes(.data$x, .data$y),
        StPointD(TP[1, 2], 90 - TP[2, 2]),
        col = 'green3',
        size = 3,
        alpha = .7,
        shape = eigen.shape,
        stroke = .75
      ) +
      ggplot2::geom_path(
        ggplot2::aes(.data$xp, .data$yp, group = .data$path),
        SmallCircleD(TP[1, 3], TP[2, 3], cone.3, closed = T) %>%
          dplyr::bind_rows(.id = 'path'),
        col = 'blue'
      ) +
      ggplot2::geom_point(
        ggplot2::aes(.data$x, .data$y),
        StPointD(TP[1, 3], 90 - TP[2, 3]),
        col = 'blue',
        size = 3,
        alpha = .7,
        shape = eigen.shape,
        stroke = .75
      )
  } else if (plot == 'eig') {
    stereoplot = ggstereo() +
      ggplot2::geom_point(
        ggplot2::aes(.data$xp, .data$yp),
        StCoordLineD(d.plot, i.plot),
        size = 2,
        alpha = .5
      ) +
      ggplot2::geom_path(
        ggplot2::aes(.data$xp, .data$yp, group = .data$path),
        SmallCircleD(TP[1, 1], TP[2, 1], cone.1, closed = T) %>%
          dplyr::bind_rows(.id = 'path'),
        col = 'red'
      ) +
      ggplot2::geom_point(
        ggplot2::aes(.data$x, .data$y),
        StPointD(TP[1, 1], 90 - TP[2, 1]),
        col = 'red',
        size = 3,
        alpha = .7,
        shape = eigen.shape,
        stroke = .75
      ) +
      ggplot2::geom_path(
        ggplot2::aes(.data$xp, .data$yp, group = .data$path),
        SmallCircleD(TP[1, 2], TP[2, 2], cone.2, closed = T) %>%
          dplyr::bind_rows(.id = 'path'),
        col = 'green3'
      ) +
      ggplot2::geom_point(
        ggplot2::aes(.data$x, .data$y),
        StPointD(TP[1, 2], 90 - TP[2, 2]),
        col = 'green3',
        size = 3,
        alpha = .7,
        shape = eigen.shape,
        stroke = .75
      ) +
      ggplot2::geom_path(
        ggplot2::aes(.data$xp, .data$yp, group = .data$path),
        SmallCircleD(TP[1, 3], TP[2, 3], cone.3, closed = T) %>%
          dplyr::bind_rows(.id = 'path'),
        col = 'blue'
      ) +
      ggplot2::geom_point(
        ggplot2::aes(.data$x, .data$y),
        StPointD(TP[1, 3], 90 - TP[2, 3]),
        col = 'blue',
        size = 3,
        alpha = .7,
        shape = eigen.shape,
        stroke = .75
      )
  } else if (plot == 'mean') {
    stereoplot = ggstereo() +
      ggplot2::geom_point(
        ggplot2::aes(.data$xp, .data$yp),
        StCoordLineD(d.plot, i.plot),
        size = 2,
        alpha = .5
      ) +
      ggplot2::geom_path(
        ggplot2::aes(.data$xp, .data$yp, group = .data$path),
        SmallCircleD(trd.plot, plg.plot, cono, closed = T) %>%
          dplyr::bind_rows(.id = 'path'),
        col = 'gold'
      ) +
      ggplot2::geom_point(
        ggplot2::aes(.data$x, .data$y),
        StPointD(trd.plot, 90 - plg.plot),
        col = 'gold',
        size = 3,
        alpha = .7,
        shape = mean.shape,
        stroke = .75
      )
  }

  res.lst = list(
    eigen_values = t(round(eigval, 4)),
    eigen_vectors = eigvec,
    orientation_matrix = O,
    trend_plunge = TP,
    dir_dip = DD,
    vector_stats = dplyr::bind_cols(STATS, x = xh, y = yh, z = zh),
    axis_stats_watson = STATS2,
    axis_stats_aniso = STATS3,
    PointGirdleRandom = PGR,
    eigenplot = eigenplot,
    pgrplot = pgrplot,
    stereoplot = stereoplot
  )

  return(res.lst)
}
