#' @title Create a stereographic projection base plot
#'
#' @description Builds an empty stereonet (equal-angle or equal-area) using ggplot2, with an
#' optional great-circle and small-circle grid. The returned ggplot object can
#' be extended with additional layers.
#'
#' @param wulff Logical. If \code{TRUE} an equal-angle (Wulff) net is used; if
#'   \code{FALSE} (default) an equal-area (Schmidt) net is used.
#' @param show.grid Logical. If \code{TRUE} (default) a great-circle and
#'   small-circle grid is drawn.
#' @param grid.col Character. Colour of the grid lines. Default \code{"grey50"}.
#' @param tl Numeric. Line width (\code{linewidth}) of the grid lines. Default
#'   \code{0.05}.
#'
#' @returns A ggplot object containing the stereonet base plot.
#'
#' @family structural algorithms, directional statistics
#' @export
#' @examples
#' ggstereo()
#' ggstereo(wulff = TRUE, grid.col = "grey70", tl = 0.1)
#' ggstereo(show.grid = FALSE)
ggstereo = function(
  wulff = F,
  show.grid = TRUE,
  grid.col = "grey50",
  tl = 0.05
) {
  intrad <- 10 * pi / 180
  east <- pi / 2
  west <- 3 * east
  TH <- (0:360) * pi / 180
  nCircles <- pi / (intrad * 2)
  paths = vector('list', nCircles) %>% purrr::set_names(1:nCircles)
  p = vector('list', nCircles * 2 + 1) %>%
    purrr::set_names(1:(nCircles * 2 + 1))

  if (show.grid) {
    nCircles <- pi / (intrad * 2)
    trd <- 0
    plg <- 0
    for (i in 1:nCircles) {
      coneAngle <- i * intrad
      paths[[i]] <- SmallCircle(trd, plg, coneAngle, wulff = wulff)
    }
    for (i in 0:(nCircles * 2)) {
      if (i > nCircles) {
        trd <- east
        plg <- (i - nCircles) * intrad
      } else {
        trd <- west
        plg <- i * intrad
      }
      if (plg == east) {
        plg <- plg * (1 - .Machine$double.min)
      }
      sd <- Pole(trd, plg, 0)
      p[[i + 1]] <- GreatCircle(sd[1], sd[2], wulff = wulff)
    }

    sc = seq(0, 90, 10)
    gc = tidyr::expand_grid(strike = c(0, 180), dip = seq(0, 90, 10))
    gc = gc[c(-1, -20), ]

    smallc = purrr::map(
      sc,
      ~ SmallCircleD(0, 0, .x, wulff = wulff) %>%
        dplyr::bind_rows(.id = 'path')
    ) %>%
      dplyr::bind_rows(.id = 'c') %>%
      dplyr::mutate(c = paste0(c, .data$path))
    greatc = purrr::map2(
      gc$strike,
      gc$dip,
      ~ GreatCircleD(.x, .y, wulff = wulff)
    ) %>%
      dplyr::bind_rows(.id = 'c')
  }

  outer = tibble::tibble(x = cos(TH), y = sin(TH))

  etiquetas = tibble::tibble(
    x = c(0, 1.1, 0, -1.1),
    y = c(1.075, 0, -1.075, 0),
    lab1 = c('0', '90', '180', '270'),
    lab2 = c('N', 'E', 'S', 'W')
  )

  if (show.grid) {
    ggstereo = ggplot2::ggplot() +
      ggplot2::geom_path(ggplot2::aes(.data$x, .data$y), outer) +
      ggplot2::geom_path(
        ggplot2::aes(.data$xp, .data$yp, group = .data$c),
        smallc,
        linewidth = tl,
        col = grid.col
      ) +
      ggplot2::geom_path(
        ggplot2::aes(.data$xp, .data$yp, group = .data$c),
        greatc,
        linewidth = tl,
        col = grid.col
      ) +
      ggplot2::geom_point(ggplot2::aes(0, 0), shape = 3, size = 3) +
      ggplot2::geom_text(
        ggplot2::aes(.data$x, .data$y, label = .data$lab1),
        etiquetas,
        size = 5
      ) +
      ggplot2::coord_fixed() +
      ggplot2::theme_void()
  } else {
    ggstereo = ggplot2::ggplot() +
      ggplot2::geom_path(ggplot2::aes(.data$x, .data$y), outer) +
      ggplot2::geom_point(ggplot2::aes(0, 0), shape = 3, size = 3) +
      ggplot2::geom_text(
        ggplot2::aes(.data$x, .data$y, label = .data$lab1),
        etiquetas,
        size = 5
      ) +
      ggplot2::coord_fixed() +
      ggplot2::theme_void()
  }

  ggstereo
}
