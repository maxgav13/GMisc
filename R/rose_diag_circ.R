#' Rose diagram for circular (directional) data
#'
#' Draws a rose diagram (circular histogram) for a sample of angular
#' measurements using ggplot2. Petal lengths are scaled to the square root of
#' frequency so that petal area is proportional to count, reducing the visual
#' dominance of the most frequent bin. Supports both vectorial data (0°–360°)
#' and axial data (0°–180°), and optionally overlays the mean direction.
#'
#' @param x Numeric vector of angles in degrees, or an unquoted column name
#'   when \code{data} is supplied.
#' @param data A data frame (or tibble) containing the column referenced by
#'   `x`. Default \code{NULL} (use \code{x} directly as a vector).
#' @param width Numeric. Bin width in degrees. Default \code{20}.
#' @param dir Integer. Data type: \code{1} for vectorial data (full 0°–360° range,
#'   default); \code{0} for axial data (0°–180° range, e.g. strike or foliation—
#'   each measurement is mirrored to produce a symmetric bimodal diagram).
#' @param conf.level Numeric. Confidence level passed to [dir_stats_circ()] for
#'   the mean-direction confidence cone shown in the subtitle. Default \code{0.95}.
#' @param alpha Numeric. Transparency of the petal fill
#'   (0 = fully transparent, 1 = fully opaque). Default \code{0.7}.
#' @param fill.col Character. Fill colour of the petals. Default \code{"blue"}.
#' @param mean.col Character. Colour of the mean-direction line(s). Default
#'   \code{"red"}.
#' @param show.mean Logical. If \code{TRUE} (default) a radial line is drawn at the
#'   mean direction and the mean ± confidence cone is shown in the plot
#'   subtitle; if \code{FALSE} neither is shown.
#'
#' @returns A ggplot object containing the rose diagram. The plot caption
#'   reports the maximum bin percentage and the sample size; the subtitle (when
#'   \code{show.mean = TRUE}) reports the mean direction and its confidence cone in
#'   degrees.
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
#' # Vectorial rose diagram with default settings
#' rose_diag_circ(angles)
#'
#' # Axial data (strikes), wider bins, custom colours
#' rose_diag_circ(angles, dir = 0, width = 30,
#'                fill.col = "steelblue", mean.col = "magenta")
#'
#' # From a data frame column, no mean line
#' df <- data.frame(azi = angles)
#' rose_diag_circ(azi, data = df, show.mean = FALSE)
rose_diag_circ = function(
  x,
  data = NULL,
  width = 20,
  dir = 1,
  conf.level = 0.95,
  alpha = .7,
  fill.col = 'blue',
  mean.col = 'red',
  show.mean = T
) {
  if (is.null(data)) {
    x = x
  } else {
    x = data %>% dplyr::pull({{ x }})
  }

  x = ifelse(x > 360, x - 360, x)

  if (dir == 0) {
    y = x + 180
    # y = ifelse(y > 360, y - 360, y)
    z = c(x, y)
  } else {
    z = x
  }

  z = ifelse(z > 360, z - 360, z)

  if (dir == 0) {
    max.dir = DescTools::Freq(z, seq(0, 180, width)) %>%
      tibble::tibble() %>%
      dplyr::slice_max(.data$perc) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::pull(.data$perc, .data$freq) %>%
      round(3) *
      100
  } else {
    max.dir = DescTools::Freq(x, seq(0, 360, width)) %>%
      tibble::tibble() %>%
      dplyr::slice_max(.data$perc) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::pull(.data$perc, .data$freq) %>%
      round(3) *
      100
  }

  N = length(x)
  r = suppressWarnings(dir_stats_circ(x, dir = dir, conf.level = conf.level))

  labelmeandir = bquote(
    "Mean direction =" ~ .(format(round(r$theta.hat, 1))) *
      degree *
      "" %+-% "" *
      .(format(round(r$cone, 1))) *
      degree
    # ~ ", N =" ~ .(r$N)
  )

  labelmeandir2 = paste0(
    "Mean direction =",
    format(r$theta.hat, 1),
    " (",
    format(round(r$cone, 1)),
    ")",
    ", N =",
    r$N
  )

  theta.180 = ifelse(r$theta.hat > 180, r$theta.hat - 180, r$theta.hat + 180)

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

  if (dir == 1) {
    plt.dirrose <- ggplot(data.frame(z), aes(.data$z)) +
      stat_bin(
        aes(
          y = sqrt(
            after_stat(.data$count) / max(after_stat(.data$count)) * 100^2
          )
        ),
        breaks = seq(0, 360, width),
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
      labs(caption = paste0('Max. = ', max.dir, '%', ', N = ', N)) +
      coord_radial(expand = F) +
      theme_rose

    if (show.mean) {
      plt.dirrose = plt.dirrose +
        geom_vline(xintercept = r$theta.hat, col = mean.col, linewidth = .5) +
        labs(subtitle = labelmeandir)
    }
  } else {
    plt.dirrose <- ggplot(data.frame(z), aes(.data$z)) +
      stat_bin(
        aes(
          y = sqrt(
            after_stat(.data$count) / max(after_stat(.data$count)) * 100^2
          )
        ),
        breaks = seq(0, 360, width),
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
      labs(caption = paste0('Max. = ', max.dir, '%', ', N = ', N)) +
      coord_radial(expand = F) +
      theme_rose

    if (show.mean) {
      plt.dirrose = plt.dirrose +
        geom_vline(
          xintercept = c(r$theta.hat, theta.180),
          col = mean.col,
          linewidth = .5
        ) +
        labs(subtitle = labelmeandir)
    }
  }

  return(plt.dirrose)
}
