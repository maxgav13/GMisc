#' Soil collapse and expansion potential chart
#'
#' Draws a soil collapse and expansion potential chart—dry density vs liquid
#' limit—with two boundary curves separating zones of collapse and expansion
#' potential (low, medium, high, and very high). The boundary curves are drawn; three vertical dashed
#' reference lines are drawn at liquid limits of 40, 50, and 63. Each zone is
#' labelled directly on the chart.
#'
#' @param lang Character. Language for axis titles: \code{"en"} (default) for
#'   English, \code{"es"} for Spanish.
#' @param ll.max Numeric. Maximum value for the liquid limit axis. Default is 100.
#'
#' @returns A ggplot object with liquid limit on the x-axis and dry
#'   density on the y-axis, containing the two boundary
#'   curves, three dashed reference lines at LL = 40, 50, and 63, and zone
#'   labels (\code{"Collapse"} / \code{"Expansion"}).
#'
#' @references Holtz, R. D., Kovacs, W. D., & Sheahan, T. C. (2023). An Introduction to Geotechnical Engineering (3rd ed.). Pearson Education.
#'
#' @family classification plots, soil mechanics
#' @export
#' @examples
#' # English labels (default)
#' soil_collapse()
#'
#' dat = data.frame(
#'   ll = c(20, 40, 60, 80),
#'   dry = c(1.5, 1.15, 1.2, 1.0)
#' )
#'
#' # Spanish labels
#' soil_collapse(lang = "es") +
#'  ggplot2::geom_point(data = dat,
#'                      ggplot2::aes(x = ll, y = dry),
#'                      size = 3, inherit.aes = FALSE)
soil_collapse = function(lang = "en", ll.max = 100) {
  dat = tibble::tribble(
    ~ll    , ~dry  , ~gs ,
    14.280 , 1.999 , "a" ,
    14.292 , 1.992 , "a" ,
    14.352 , 1.968 , "a" ,
    14.593 , 1.946 , "a" ,
    14.904 , 1.921 , "a" ,
    15.335 , 1.895 , "a" ,
    15.783 , 1.869 , "a" ,
    16.27  , 1.846 , "a" ,
    16.645 , 1.825 , "a" ,
    17.114 , 1.803 , "a" ,
    17.691 , 1.782 , "a" ,
    18.334 , 1.761 , "a" ,
    18.938 , 1.74  , "a" ,
    19.601 , 1.717 , "a" ,
    20.325 , 1.693 , "a" ,
    20.989 , 1.673 , "a" ,
    21.713 , 1.651 , "a" ,
    22.498 , 1.628 , "a" ,
    23.222 , 1.608 , "a" ,
    24.006 , 1.588 , "a" ,
    24.851 , 1.565 , "a" ,
    25.695 , 1.543 , "a" ,
    26.54  , 1.523 , "a" ,
    27.385 , 1.503 , "a" ,
    28.29  , 1.482 , "a" ,
    29.255 , 1.46  , "a" ,
    30.221 , 1.439 , "a" ,
    31.186 , 1.419 , "a" ,
    32.151 , 1.399 , "a" ,
    33.177 , 1.378 , "a" ,
    34.263 , 1.358 , "a" ,
    35.349 , 1.338 , "a" ,
    36.496 , 1.317 , "a" ,
    37.642 , 1.296 , "a" ,
    38.788 , 1.277 , "a" ,
    39.995 , 1.256 , "a" ,
    41.202 , 1.236 , "a" ,
    42.469 , 1.216 , "a" ,
    43.796 , 1.197 , "a" ,
    45.124 , 1.178 , "a" ,
    46.451 , 1.16  , "a" ,
    47.779 , 1.144 , "a" ,
    49.106 , 1.127 , "a" ,
    50.433 , 1.112 , "a" ,
    51.761 , 1.097 , "a" ,
    53.088 , 1.082 , "a" ,
    54.416 , 1.069 , "a" ,
    55.743 , 1.056 , "a" ,
    57.07  , 1.042 , "a" ,
    58.398 , 1.029 , "a" ,
    59.725 , 1.017 , "a" ,
    61.053 , 1.004 , "a" ,
    62.38  , 0.992 , "a" ,
    63.708 , 0.98  , "a" ,
    65.035 , 0.969 , "a" ,
    66.362 , 0.957 , "a" ,
    67.69  , 0.946 , "a" ,
    69.017 , 0.935 , "a" ,
    70.345 , 0.924 , "a" ,
    71.672 , 0.913 , "a" ,
    72.999 , 0.903 , "a" ,
    74.327 , 0.892 , "a" ,
    75.654 , 0.882 , "a" ,
    76.982 , 0.871 , "a" ,
    78.309 , 0.861 , "a" ,
    79.636 , 0.851 , "a" ,
    80.964 , 0.841 , "a" ,
    82.291 , 0.831 , "a" ,
    83.619 , 0.821 , "a" ,
    84.946 , 0.81  , "a" ,
    85.972 , 0.803 , "a" ,
    15.7   , 1.999 , "b" ,
    15.8   , 1.985 , "b" ,
    15.904 , 1.96  , "b" ,
    16.266 , 1.936 , "b" ,
    16.597 , 1.912 , "b" ,
    17.037 , 1.882 , "b" ,
    17.438 , 1.86  , "b" ,
    17.912 , 1.838 , "b" ,
    18.49  , 1.817 , "b" ,
    19.058 , 1.794 , "b" ,
    19.782 , 1.771 , "b" ,
    20.446 , 1.747 , "b" ,
    21.11  , 1.726 , "b" ,
    21.774 , 1.705 , "b" ,
    22.498 , 1.682 , "b" ,
    23.222 , 1.661 , "b" ,
    23.946 , 1.641 , "b" ,
    24.73  , 1.619 , "b" ,
    25.575 , 1.596 , "b" ,
    26.419 , 1.576 , "b" ,
    27.264 , 1.554 , "b" ,
    28.109 , 1.534 , "b" ,
    28.954 , 1.514 , "b" ,
    29.859 , 1.493 , "b" ,
    30.764 , 1.474 , "b" ,
    31.669 , 1.454 , "b" ,
    32.694 , 1.432 , "b" ,
    33.781 , 1.41  , "b" ,
    34.927 , 1.387 , "b" ,
    36.073 , 1.365 , "b" ,
    37.22  , 1.345 , "b" ,
    38.366 , 1.324 , "b" ,
    39.513 , 1.303 , "b" ,
    40.719 , 1.283 , "b" ,
    41.986 , 1.261 , "b" ,
    43.253 , 1.24  , "b" ,
    44.52  , 1.221 , "b" ,
    45.848 , 1.201 , "b" ,
    47.175 , 1.183 , "b" ,
    48.503 , 1.166 , "b" ,
    49.83  , 1.15  , "b" ,
    51.157 , 1.134 , "b" ,
    52.485 , 1.118 , "b" ,
    53.812 , 1.104 , "b" ,
    55.14  , 1.091 , "b" ,
    56.467 , 1.077 , "b" ,
    57.795 , 1.063 , "b" ,
    59.122 , 1.05  , "b" ,
    60.449 , 1.037 , "b" ,
    61.777 , 1.025 , "b" ,
    63.104 , 1.013 , "b" ,
    64.432 , 1.001 , "b" ,
    65.759 , 0.989 , "b" ,
    67.086 , 0.978 , "b" ,
    68.414 , 0.966 , "b" ,
    69.741 , 0.955 , "b" ,
    71.069 , 0.944 , "b" ,
    72.396 , 0.933 , "b" ,
    73.723 , 0.923 , "b" ,
    75.051 , 0.912 , "b" ,
    76.378 , 0.902 , "b" ,
    77.706 , 0.892 , "b" ,
    79.033 , 0.881 , "b" ,
    80.36  , 0.871 , "b" ,
    81.688 , 0.861 , "b" ,
    83.015 , 0.851 , "b" ,
    84.343 , 0.841 , "b" ,
    85.67  , 0.831 , "b" ,
    86.997 , 0.821 , "b" ,
    88.325 , 0.811 , "b" ,
    89.532 , 0.805 , "b"
  ) |>
    dplyr::mutate(
      gs = ifelse(.data$gs == "a", "2.60", "2.70")
    )

  xlab = ifelse(lang == "es", "L\u00edmite l\u00edquido", "Liquid limit")
  ylab = ifelse(
    lang == "es",
    "Densidad seca, g/cm\u00b3",
    "Dry density, g/cm\u00b3"
  )

  dat.labels = data.frame(
    ll = c(75, 56, 45, 32, 20, 80),
    dry = c(rep(1.8, 4), 1.2, 1.2),
    expansion = if (lang == "en") {
      c("Very high", "High", "Med.", "Low", "Collapse", "Expansion")
    } else {
      c("Muy alta", "Alta", "Med.", "Baja", "Colapso", "Expansi\u00f3n")
    }
  )

  if (ll.max > 100) {
    dat2 = dat |>
      dplyr::nest_by(.data$gs) |>
      dplyr::mutate(
        mod = list(
          stats::nls(dry ~ SSinvpoly3(ll, a, b, c, d), data = .data$data)
        ),
        dry = list(broom::augment(
          .data$mod,
          newdata = data.frame(ll = seq(70, ll.max, length.out = 70))
        ))
      ) |>
      tidyr::unnest(cols = c(.data$dry)) |>
      dplyr::select(
        .data$ll,
        -c(.data$mod, .data$data),
        dry = .data$.fitted,
        .data$gs
      ) |>
      dplyr::ungroup()
    dat = dplyr::bind_rows(dat, dat2)
  }

  y.min = ifelse(ll.max > 100, (min(dat2$dry) %/% .1) * .1, 0.8)

  collapse = ggplot2::ggplot(
    dat,
    ggplot2::aes(
      .data$ll,
      .data$dry,
      linetype = .data$gs,
      group = .data$gs
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_text(
      ggplot2::aes(.data$ll, .data$dry, label = .data$expansion),
      dat.labels,
      size = 4,
      vjust = 0,
      show.legend = F,
      inherit.aes = F
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = 40,
        y = 1.9,
        xend = 40,
        yend = 1.4
      ),
      linewidth = 0.2,
      linetype = "dashed",
      col = 'black'
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = 50,
        y = 1.9,
        xend = 50,
        yend = 1.3
      ),
      linewidth = 0.2,
      linetype = "dashed",
      col = 'black'
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = 63,
        y = 1.9,
        xend = 63,
        yend = 1.1
      ),
      linewidth = 0.2,
      linetype = "dashed",
      col = 'black'
    ) +
    ggplot2::labs(x = xlab, y = ylab, linetype = expression(G[s])) +
    ggplot2::scale_x_continuous(
      limits = c(0, ll.max),
      breaks = seq(0, ll.max, 10)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(y.min, 2),
      breaks = seq(y.min, 2, 0.2)
    ) +
    ggplot2::coord_cartesian(
      expand = FALSE
    ) +
    ggplot2::theme_bw(base_size = 14)

  return(collapse)
}
