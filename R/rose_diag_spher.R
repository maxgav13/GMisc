#' Rose diagrams for spherical (3-D directional) data
#'
#' Draws a set of rose diagrams for a sample of 3-D orientations (lines or
#' planes) using ggplot2. Petal lengths are scaled to the square root of
#' frequency so that petal area is proportional to count. Depending on the
#' \code{show} argument, up to five diagrams are returned: strike (bimodal, 0°–360°
#' mirrored), dip direction (0°–360°), dip angle (0°–90°), trend (0°–360°),
#' and plunge (0°–90°). Optionally overlays the mean orientation; the dominant eigenvector (V1 for clustered data, V3 for
#' girdle distributions) is used as the mean.
#'
#' @param trd Numeric vector of trend / dip-direction / strike values in
#'   degrees, or an unquoted column name when \code{data} is supplied.
#' @param plg Numeric vector of plunge / dip values in degrees, or an unquoted
#'   column name when \code{data} is supplied.
#' @param data A data frame (or tibble) containing the columns referenced by
#'   \code{trd} and \code{plg}. Default \code{NULL} (use vectors directly).
#' @param conf.level Numeric. Confidence level
#'   for mean direction statistics shown in the subtitles. Default \code{0.95}.
#' @param width1 Numeric. Bin width in degrees for the azimuthal diagrams
#'   (strike, dip direction, trend). Default \code{20}.
#' @param width2 Numeric. Bin width in degrees for the inclination diagrams
#'   (dip angle, plunge). Default \code{5}.
#' @param alpha Numeric. Transparency of the petal fill
#'   (0 = fully transparent, 1 = fully opaque). Default \code{0.7}.
#' @param fill.col Character. Fill colour of the petals. Default \code{"blue"}.
#' @param mean.col Character. Colour of the mean-direction line(s). Default
#'   \code{"red"}.
#' @param type Character. How to interpret the input orientations:
#'   \describe{
#'     \item{`"line"`}{(default) Trend and plunge of a lineation.}
#'     \item{`"dir"`}{Dip direction and dip angle of a plane (converted
#'       internally to the pole before computing the mean).}
#'     \item{`"strike"`}{Strike (right-hand rule) and dip angle of a plane
#'       (converted internally to the pole before computing the mean).}
#'   }
#' @param show Character vector selecting which diagrams to return. Use
#'   \code{"all"} (default) for all five, or any subset of \code{c("strike", "dir",
#'   "dip", "trend", "plunge")}.
#' @param show.mean Logical. If \code{TRUE} (default) a radial line is drawn at the
#'   mean orientation and the mean value is shown in the subtitle of each
#'   diagram; if \code{FALSE} neither is added. When the distribution is random
#'   (R index dominant) the mean line is suppressed regardless.
#'
#' @returns A named list of ggplot objects. Names correspond to the selected
#'   diagrams: \code{"strike"}, \code{"dir"} (dip direction), \code{"dip"} (dip angle),
#'   \code{"trend"}, and/or \code{"plunge"}. Each plot caption reports the maximum bin
#'   percentage and the sample size \code{N}.
#'
#' @import ggplot2
#'
#' @family directional statistics
#' @export
#' @examples
#' # Lines supplied as plain vectors — all five diagrams
#' set.seed(42)
#' trds <- runif(50, 40, 80)
#' plgs <- runif(50, 10, 50)
#' plots <- rose_diag_spher(trds, plgs)
#' plots$trend
#' plots$plunge
#'
#' # Planes as dip direction / dip, only strike and dip-angle diagrams
#' rose_diag_spher(trds, plgs, type = "dir",
#'                 show = c("strike", "dip"), fill.col = "steelblue")
#'
#' # From a data frame, no mean line
#' df <- data.frame(dip_direction = trds, dip = plgs)
#' rose_diag_spher(dip_direction, dip, data = df,
#'                 type = "dir", show.mean = FALSE)
rose_diag_spher = function(
  trd,
  plg,
  data = NULL,
  conf.level = .95,
  width1 = 20,
  width2 = 5,
  alpha = .7,
  fill.col = 'blue',
  mean.col = 'red',
  type = c('line', 'dir', 'strike'),
  show = 'all',
  show.mean = T
) {
  if (is.null(data)) {
    trd = trd
    plg = plg
  } else {
    trd = data %>% dplyr::pull({{ trd }})
    plg = data %>% dplyr::pull({{ plg }})
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

  d = ifelse(i < 0, ifelse(d + 180 > 360, d - 180, d + 180), d)
  i = ifelse(i < 0, -i, i)
  d = ifelse(d > 360, d - 360, d)

  dat.rose = purrr::map2(
    d,
    i,
    ~ (Pole(.x * pi / 180, .y * pi / 180, 0) * 180 / pi) %>%
      tibble::as_tibble_row()
  ) %>%
    purrr::list_rbind() %>%
    dplyr::mutate(dir = s2d(strike), trd = d, plg = i)

  strike2 = dat.rose$strike + 180
  strike2 = ifelse(strike2 > 360, strike2 - 360, strike2)
  strike = c(dat.rose$strike, strike2)

  max.strike = DescTools::Freq(strike, seq(0, 180, width1)) %>%
    tibble::tibble() %>%
    dplyr::slice_max(.data$perc) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::pull(.data$perc, .data$freq) %>%
    round(3) *
    100

  max.dir = DescTools::Freq(dat.rose$dir, seq(0, 360, width1)) %>%
    tibble::tibble() %>%
    dplyr::slice_max(.data$perc) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::pull(.data$perc, .data$freq) %>%
    round(3) *
    100

  max.dip = DescTools::Freq(dat.rose$dip, seq(0, 90, width2)) %>%
    tibble::tibble() %>%
    dplyr::slice_max(.data$perc) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::pull(.data$perc, .data$freq) %>%
    round(3) *
    100

  max.trd = DescTools::Freq(dat.rose$trd, seq(0, 360, width1)) %>%
    tibble::tibble() %>%
    dplyr::slice_max(.data$perc) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::pull(.data$perc, .data$freq) %>%
    round(3) *
    100

  max.plg = DescTools::Freq(dat.rose$plg, seq(0, 90, width2)) %>%
    tibble::tibble() %>%
    dplyr::slice_max(.data$perc) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::pull(.data$perc, .data$freq) %>%
    round(3) *
    100

  N = length(d)

  r = dir_stats_spher(d, i, conf.level = conf.level)

  PGR = r$PointGirdleRandom

  if (all(PGR$R > c(PGR$P, PGR$G))) {
    mean.strike = NA
    mean.dip = NA
    mean.dir = NA
    mean.trd = NA
    mean.plg = NA
  } else if (PGR$P > PGR$G) {
    mean.dip = r$dir_dip[2, 1]
    mean.dir = r$dir_dip[1, 1]
    mean.strike = d2s(mean.dir)
    mean.trd = r$trend_plunge[1, 1]
    mean.plg = r$trend_plunge[2, 1]
  } else if (PGR$G > PGR$P) {
    mean.strike = r$dir_dip[1, 3]
    mean.dip = r$dir_dip[2, 3]
    mean.dir = s2d(mean.strike)
    mean.trd = r$vector_stats$trd
    mean.plg = r$vector_stats$plg
  }

  labelmeandir = bquote(
    "Mean dip direction =" ~ .(format(round(mean.dir, 1))) *
      # degree * "" %+-% "" * .(format(round(r$cone, 1))) *
      degree
    # ~ ", N =" ~ .(N)
  )

  labelmeanstrike = bquote(
    "Mean strike =" ~ .(format(round(mean.strike, 1))) *
      degree
    # ~ ", N =" ~ .(N)
  )

  labelmeandip = bquote(
    "Mean dip angle =" ~ .(format(round(mean.dip, 1))) *
      degree
    # ~ ", N =" ~ .(N)
  )

  labelmeantrd = bquote(
    "Mean trend =" ~ .(format(round(mean.trd, 1))) *
      degree
    # ~ ", N =" ~ .(N)
  )

  labelmeanplg = bquote(
    "Mean plunge =" ~ .(format(round(mean.plg, 1))) *
      degree
    # ~ ", N =" ~ .(N)
  )

  theme_rose = theme_bw() +
    theme(
      title = element_text(size = 14),
      axis.title.x = element_blank(),
      panel.border = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(
        size = 12,
        face = "plain",
        hjust = 0.9,
        vjust = 1.3
      ),
      panel.grid.major = element_line(linewidth = 0.3, colour = "grey60"),
      panel.grid.minor = element_line(linewidth = 0.1, colour = "grey60")
    )

  strike.180 = ifelse(mean.strike > 180, mean.strike - 180, mean.strike + 180)

  plt.dirrose <- ggplot(dat.rose, aes(.data$dir)) +
    stat_bin(
      aes(
        y = sqrt(after_stat(.data$count) / max(after_stat(.data$count)) * 100^2)
      ),
      breaks = seq(0, 360, width1),
      colour = "black",
      alpha = alpha,
      fill = fill.col,
      closed = "left"
    ) +
    scale_x_continuous(
      breaks = seq(0, 360, 30),
      minor_breaks = seq(0, 360, 10),
      limits = c(0, 360)
    ) +
    scale_y_continuous(NULL, breaks = seq(0, 100, 20), labels = NULL) +
    labs(
      subtitle = 'Dip direction',
      caption = paste0('Max. = ', max.dir, '%', ', N = ', N)
    ) +
    coord_radial(expand = F) +
    theme_rose

  plt.strikerose <- ggplot(data.frame(strike), aes(.data$strike)) +
    stat_bin(
      aes(
        y = sqrt(after_stat(.data$count) / max(after_stat(.data$count)) * 100^2)
      ),
      breaks = seq(0, 360, width1),
      colour = "black",
      alpha = alpha,
      fill = fill.col,
      closed = "left"
    ) +
    scale_x_continuous(
      breaks = seq(0, 360, 30),
      minor_breaks = seq(0, 360, 10),
      limits = c(0, 360)
    ) +
    scale_y_continuous(NULL, breaks = seq(0, 100, 20), labels = NULL) +
    labs(
      subtitle = 'Strike',
      caption = paste0('Max. = ', max.strike, '%', ', N = ', N)
    ) +
    coord_radial(expand = F) +
    theme_rose

  plt.diprose = ggplot(dat.rose, aes(.data$dip)) +
    stat_bin(
      aes(
        y = sqrt(after_stat(.data$count) / max(after_stat(.data$count)) * 100^2)
      ),
      breaks = seq(0, 90, width2),
      colour = "black",
      alpha = alpha,
      fill = fill.col,
      closed = "left"
    ) +
    scale_x_continuous(breaks = seq(0, 90, 10), limits = c(0, 90)) +
    scale_y_continuous(NULL, breaks = seq(0, 100, 20), labels = NULL) +
    labs(
      subtitle = 'Dip angle',
      caption = paste0('Max. = ', max.dip, '%', ', N = ', N)
    ) +
    coord_radial(start = pi / 2, end = pi, expand = F) +
    theme_rose

  plt.trdrose <- ggplot(dat.rose, aes(.data$trd)) +
    stat_bin(
      aes(
        y = sqrt(after_stat(.data$count) / max(after_stat(.data$count)) * 100^2)
      ),
      breaks = seq(0, 360, width1),
      colour = "black",
      alpha = alpha,
      fill = fill.col,
      closed = "left"
    ) +
    scale_x_continuous(
      breaks = seq(0, 360, 30),
      minor_breaks = seq(0, 360, 10),
      limits = c(0, 360)
    ) +
    scale_y_continuous(NULL, breaks = seq(0, 100, 20), labels = NULL) +
    labs(
      subtitle = 'Trend',
      caption = paste0('Max. = ', max.trd, '%', ', N = ', N)
    ) +
    coord_radial(expand = F) +
    theme_rose

  plt.plgrose = ggplot(dat.rose, aes(.data$plg)) +
    stat_bin(
      aes(
        y = sqrt(after_stat(.data$count) / max(after_stat(.data$count)) * 100^2)
      ),
      breaks = seq(0, 90, width2),
      colour = "black",
      alpha = alpha,
      fill = fill.col,
      closed = "left"
    ) +
    scale_x_continuous(breaks = seq(0, 90, 10), limits = c(0, 90)) +
    scale_y_continuous(NULL, breaks = seq(0, 100, 20), labels = NULL) +
    labs(
      subtitle = 'Plunge',
      caption = paste0('Max. = ', max.plg, '%', ', N = ', N)
    ) +
    coord_radial(start = pi / 2, end = pi, expand = F) +
    theme_rose

  if (show.mean) {
    plt.dirrose = plt.dirrose +
      geom_vline(xintercept = mean.dir, col = mean.col, linewidth = .5) +
      labs(subtitle = labelmeandir)

    plt.strikerose = plt.strikerose +
      geom_vline(
        xintercept = c(mean.strike, strike.180),
        col = mean.col,
        linewidth = .5
      ) +
      labs(subtitle = labelmeanstrike)

    plt.diprose = plt.diprose +
      geom_vline(xintercept = mean.dip, col = mean.col, linewidth = .5) +
      labs(subtitle = labelmeandip)

    plt.trdrose = plt.trdrose +
      geom_vline(xintercept = mean.trd, col = mean.col, linewidth = .5) +
      labs(subtitle = labelmeantrd)

    plt.plgrose = plt.plgrose +
      geom_vline(xintercept = mean.plg, col = mean.col, linewidth = .5) +
      labs(subtitle = labelmeanplg)
  }

  l.all = list(
    strike = plt.strikerose,
    dir = plt.dirrose,
    dip = plt.diprose,
    trend = plt.trdrose,
    plunge = plt.plgrose
  )

  if (any(show == 'all')) {
    l.res = l.all
  } else {
    l.res = purrr::map(show, ~ l.all[[.x]]) %>%
      purrr::set_names(show)
  }

  return(l.res)
}
