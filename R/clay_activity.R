#' Clay activity chart
#'
#' Draws a clay activity chart—plasticity index vs clay percentage—following. Reference lines for eight activity values are fitted by linear regression on built-in data and plotted as
#' styled lines. Activity values are labelled directly on the chart and/or shown
#' in a legend.
#'
#' @param show.legend Logical. If \code{TRUE} (default) a legend mapping line type
#'   to activity value is shown; if \code{FALSE} the legend is suppressed.
#' @param legend.pos Character. Position of the legend passed to
#'   \code{theme(legend.position = ...)}. Default \code{"top"}.
#' @param show.labels Logical. If \code{TRUE} (default) activity values are
#'   annotated directly on the reference lines; if \code{FALSE}`
#'   labels are omitted.
#' @param labels.pos Character. Position of the direct labels: \code{"high"}
#'   (default) places labels near the top of the plot; \code{"low"} places
#'   labels near the bottom.
#' @param lang Character. Language for axis titles: \code{"en"} (default) for
#'   English, \code{"es"} for Spanish.
#'
#' @returns A ggplot object with clay percentage on the x-axis (0–100) and
#'   plasticity index on the y-axis (0–500), containing the eight reference
#'   activity lines and optional direct labels.
#'
#' @references Das, B. M., & Sobhan, K. (2018). Principles of geotechnical engineering (Ninth edition). Cengage Learning.
#'
#' @import ggplot2
#'
#' @family classification plots, soil mechanics
#' @export
#' @examples
#' # Reference chart only (no sample data)
#' clay_activity()
#'
#' # Spanish axis labels, no legend
#' clay_activity(show.legend = FALSE, lang = "es")
clay_activity = function(
  show.legend = T,
  legend.pos = "top",
  show.labels = T,
  labels.pos = "high",
  lang = "en"
) {
  dat = tibble::tribble(
    ~clay , ~pi    , ~soil      , ~A   ,
    23.87 ,  83.51 , "B"        , 5.4  ,
    48.44 , 210.75 , "B"        , 5.4  ,
    59.32 , 267.30 , "B"        , 5.4  ,
    72.92 , 331.52 , "B"        , 5.4  ,
    97.70 , 474.67 , "B"        , 5.4  ,
    21.57 ,  62.29 , "BK 4:1"   , 4.85 ,
    42.87 , 165.98 , "BK 4:1"   , 4.85 ,
    64.91 , 268.48 , "BK 4:1"   , 4.85 ,
    87.36 , 376.88 , "BK 4:1"   , 4.85 ,
    20.21 ,  47.57 , "BK 1.5:1" , 4.43 ,
    37.13 , 123.56 , "BK 1.5:1" , 4.43 ,
    56.76 , 214.87 , "BK 1.5:1" , 4.43 ,
    75.67 , 280.27 , "BK 1.5:1" , 4.43 ,
    23.45 ,  45.80 , "KB 1.5:1" , 3.25 ,
    32.85 ,  79.38 , "KB 1.5:1" , 3.25 ,
    49.56 , 129.46 , "KB 1.5:1" , 3.25 ,
    67.10 , 184.24 , "KB 1.5:1" , 3.25 ,
    16.76 ,  17.53 , "KB 4:1"   , 1.95 ,
    27.83 ,  38.73 , "KB 4:1"   , 1.95 ,
    41.41 ,  62.89 , "KB 4:1"   , 1.95 ,
    54.67 ,  89.99 , "KB 4:1"   , 1.95 ,
    36.50 ,  41.09 , "KB 9:1"   , 1.52 ,
    49.03 ,  62.30 , "KB 9:1"   , 1.52 ,
    25.01 ,  16.94 , "KB 19:1"  ,  .96 ,
    37.55 ,  29.31 , "KB 19:1"  ,  .96 ,
    48.83 ,  42.27 , "KB 19:1"  ,  .96 ,
    23.55 ,   9.87 , "K"        ,  .36 ,
    35.35 ,  12.81 , "K"        ,  .36 ,
    48.09 ,  16.94 , "K"        ,  .36
  ) %>%
    dplyr::mutate(
      soil = forcats::fct_inorder(.data$soil),
      A = forcats::as_factor(.data$A) %>% forcats::fct_rev()
    )

  clay_lab = if (labels.pos == "low") {
    c(24, 21, 20, 23, 30, 45, 40, 40)
  } else {
    c(100, 91, 79, 70, 58, 52, 51, 51)
  }
  pi_lab = if (labels.pos == "low") {
    c(82, 65, 52, 48, 45, 53, 33, 15)
  } else {
    c(475, 377, 290, 184, 90, 62.3, 42.5, 17)
  }
  dat_labels = tibble::tibble(
    clay = clay_lab,
    pi = pi_lab,
    soil = c(
      'B',
      'BK 4:1',
      'BK 1.5:1',
      'KB 1.5:1',
      'KB 4:1',
      'KB 9:1',
      'KB 19:1',
      'K'
    ),
    A = c(5.4, 4.85, 4.43, 3.25, 1.95, 1.52, .96, .36) %>%
      forcats::as_factor() %>%
      forcats::fct_rev()
  )

  if (lang == "es") {
    xlab = "Porcentaje de arcilla"
    ylab = "\u00cdndice de plasticidad, IP"
    linetype = "Actividad"
  } else {
    xlab = "Clay percentage"
    ylab = "Plasticity index, PI"
    linetype = "Activity"
  }
  g1 = dat %>%
    ggplot(
      aes(
        .data$clay,
        .data$pi,
        linetype = .data$A
      )
    ) +
    geom_smooth(
      method = 'lm',
      formula = y ~ x,
      linewidth = .5,
      se = F,
      show.legend = show.legend
    ) +
    scale_x_continuous(
      limits = c(0, 100),
      breaks = seq(0, 100, 10)
    ) +
    scale_y_continuous(
      limits = c(0, 500),
      breaks = seq(0, 500, 50)
    ) +
    labs(
      x = xlab,
      y = ylab,
      linetype = linetype
    ) +
    list(
      if (show.labels) {
        geom_label(
          aes(.data$clay, .data$pi, label = .data$A),
          dat_labels,
          inherit.aes = F,
          size = 2.5,
          # alpha = .7,
          label.padding = unit(0.15, "lines"),
          show.legend = F
        )
      } else {
        NULL
      }
    ) +
    theme_bw(base_size = 14) +
    theme(
      legend.position = legend.pos
    )
  return(g1)
}
