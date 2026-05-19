#' Soil swelling potential chart
#'
#' Draws a soil swelling potential chart—clay activity vs clay percentage—with
#' three boundary curves separating zones of low, medium, high, and very high
#' swelling potential. Boundary curves are drawn; each zone is labelled directly on the chart with.
#'
#' @param lang Character. Language for axis and legend titles: \code{"en"} (default)
#'   for English, \code{"es"} for Spanish.
#' @param clay.max Numeric. Maximum value for the clay percentage axis. Default is 70.
#'
#' @returns A ggplot object with clay percentage on the x-axis and
#'   clay activity on the y-axis, containing the three boundary curves
#'   coloured by expansion level (\code{"1.5\%"}, \code{"5\%"}, \code{"25\%"}) and their zone
#'   labels (\code{"Low"} / \code{"Medium"} / \code{"High"} / \code{"Very high"}).
#'
#' @references Holtz, R. D., Kovacs, W. D., & Sheahan, T. C. (2023). An Introduction to Geotechnical Engineering (3rd ed.). Pearson Education.
#'
#' @family classification plots, soil mechanics
#'
#' @export
#' @examples
#' # English labels (default)
#' soil_swelling()
#'
#' dat = data.frame(
#'   clay = c(20, 40, 60),
#'   activity = c(.5, 1, 2)
#' )
#' # Spanish labels
#' soil_swelling(lang = "es") +
#' ggplot2::geom_point(data = dat,
#'                     ggplot2::aes(x = clay, y = activity),
#'                     size = 3, inherit.aes = FALSE)
soil_swelling = function(lang = "en", clay.max = 70) {
  dat = tibble::tribble(
    ~clay  , ~activity , ~expansion ,
    15.622 , 4.938     , "25%"      ,
    15.893 , 4.863     , "25%"      ,
    16.083 , 4.781     , "25%"      ,
    16.417 , 4.687     , "25%"      ,
    16.708 , 4.596     , "25%"      ,
    17.013 , 4.521     , "25%"      ,
    17.228 , 4.438     , "25%"      ,
    17.5   , 4.353     , "25%"      ,
    17.772 , 4.277     , "25%"      ,
    18.147 , 4.19      , "25%"      ,
    18.527 , 4.09      , "25%"      ,
    18.88  , 3.998     , "25%"      ,
    19.084 , 3.924     , "25%"      ,
    19.478 , 3.84      , "25%"      ,
    19.831 , 3.742     , "25%"      ,
    20.184 , 3.658     , "25%"      ,
    20.564 , 3.564     , "25%"      ,
    20.917 , 3.478     , "25%"      ,
    21.27  , 3.399     , "25%"      ,
    21.65  , 3.304     , "25%"      ,
    22.003 , 3.217     , "25%"      ,
    22.411 , 3.138     , "25%"      ,
    22.818 , 3.062     , "25%"      ,
    23.167 , 2.99      , "25%"      ,
    23.633 , 2.915     , "25%"      ,
    24.04  , 2.838     , "25%"      ,
    24.467 , 2.77      , "25%"      ,
    24.971 , 2.688     , "25%"      ,
    25.511 , 2.618     , "25%"      ,
    26.212 , 2.526     , "25%"      ,
    26.823 , 2.449     , "25%"      ,
    27.434 , 2.373     , "25%"      ,
    28.045 , 2.301     , "25%"      ,
    28.656 , 2.234     , "25%"      ,
    29.403 , 2.155     , "25%"      ,
    30.15  , 2.081     , "25%"      ,
    30.897 , 2.013     , "25%"      ,
    31.779 , 1.937     , "25%"      ,
    32.73  , 1.858     , "25%"      ,
    33.68  , 1.785     , "25%"      ,
    34.698 , 1.713     , "25%"      ,
    35.784 , 1.641     , "25%"      ,
    36.939 , 1.569     , "25%"      ,
    38.161 , 1.498     , "25%"      ,
    39.45  , 1.428     , "25%"      ,
    40.876 , 1.359     , "25%"      ,
    42.464 , 1.292     , "25%"      ,
    43.863 , 1.226     , "25%"      ,
    45.357 , 1.168     , "25%"      ,
    46.85  , 1.114     , "25%"      ,
    48.344 , 1.063     , "25%"      ,
    49.837 , 1.016     , "25%"      ,
    51.331 , 0.972     , "25%"      ,
    52.824 , 0.932     , "25%"      ,
    54.318 , 0.893     , "25%"      ,
    55.223 , 0.863     , "25%"      ,
    56.438 , 0.848     , "25%"      ,
    57.984 , 0.81      , "25%"      ,
    59.477 , 0.782     , "25%"      ,
    60.971 , 0.754     , "25%"      ,
    62.464 , 0.728     , "25%"      ,
    63.958 , 0.703     , "25%"      ,
    65.452 , 0.68      , "25%"      ,
    66.945 , 0.659     , "25%"      ,
    68.439 , 0.639     , "25%"      ,
    70.000 , 0.625     , "25%"      ,
     9.897 , 4.924     , "5%"       ,
     9.987 , 4.821     , "5%"       ,
    10.123 , 4.741     , "5%"       ,
    10.259 , 4.665     , "5%"       ,
    10.462 , 4.57      , "5%"       ,
    10.564 , 4.481     , "5%"       ,
    10.788 , 4.376     , "5%"       ,
    11.005 , 4.292     , "5%"       ,
    11.033 , 4.219     , "5%"       ,
    11.277 , 4.122     , "5%"       ,
    11.548 , 4.022     , "5%"       ,
    11.739 , 3.929     , "5%"       ,
    12.024 , 3.826     , "5%"       ,
    12.159 , 3.747     , "5%"       ,
    12.295 , 3.677     , "5%"       ,
    12.615 , 3.587     , "5%"       ,
    12.825 , 3.485     , "5%"       ,
    13.096 , 3.393     , "5%"       ,
    13.341 , 3.308     , "5%"       ,
    13.563 , 3.231     , "5%"       ,
    13.857 , 3.137     , "5%"       ,
    14.196 , 3.045     , "5%"       ,
    14.468 , 2.97      , "5%"       ,
    14.739 , 2.901     , "5%"       ,
    15.024 , 2.817     , "5%"       ,
    15.405 , 2.711     , "5%"       ,
    15.758 , 2.621     , "5%"       ,
    16.097 , 2.532     , "5%"       ,
    16.369 , 2.458     , "5%"       ,
    16.708 , 2.382     , "5%"       ,
    17.061 , 2.303     , "5%"       ,
    17.503 , 2.215     , "5%"       ,
    17.93  , 2.14      , "5%"       ,
    18.337 , 2.073     , "5%"       ,
    19.016 , 1.987     , "5%"       ,
    19.695 , 1.893     , "5%"       ,
    20.306 , 1.821     , "5%"       ,
    20.985 , 1.743     , "5%"       ,
    21.664 , 1.667     , "5%"       ,
    22.343 , 1.601     , "5%"       ,
    23.225 , 1.523     , "5%"       ,
    24.176 , 1.448     , "5%"       ,
    25.194 , 1.374     , "5%"       ,
    26.348 , 1.298     , "5%"       ,
    27.57  , 1.224     , "5%"       ,
    28.86  , 1.154     , "5%"       ,
    30.286 , 1.083     , "5%"       ,
    31.779 , 1.016     , "5%"       ,
    33.273 , 0.955     , "5%"       ,
    34.766 , 0.9       , "5%"       ,
    36.26  , 0.849     , "5%"       ,
    37.753 , 0.801     , "5%"       ,
    39.247 , 0.758     , "5%"       ,
    40.74  , 0.719     , "5%"       ,
    42.161 , 0.692     , "5%"       ,
    43.727 , 0.648     , "5%"       ,
    45.221 , 0.617     , "5%"       ,
    46.714 , 0.587     , "5%"       ,
    48.208 , 0.553     , "5%"       ,
    51.195 , 0.51      , "5%"       ,
    52.689 , 0.488     , "5%"       ,
    54.182 , 0.467     , "5%"       ,
    55.676 , 0.449     , "5%"       ,
    57.169 , 0.43      , "5%"       ,
    58.663 , 0.414     , "5%"       ,
    60.156 , 0.4       , "5%"       ,
    61.65  , 0.384     , "5%"       ,
    63.143 , 0.371     , "5%"       ,
    64.637 , 0.359     , "5%"       ,
    66.13  , 0.346     , "5%"       ,
    67.624 , 0.335     , "5%"       ,
    69.117 , 0.325     , "5%"       ,
    70.000 , 0.31      , "5%"       ,
     7.543 , 4.962     , "1.5%"     ,
     7.679 , 4.882     , "1.5%"     ,
     7.815 , 4.793     , "1.5%"     ,
     7.95  , 4.559     , "1.5%"     ,
     8.086 , 4.407     , "1.5%"     ,
     8.045 , 4.331     , "1.5%"     ,
     8.232 , 4.232     , "1.5%"     ,
     8.358 , 4.144     , "1.5%"     ,
     8.471 , 3.963     , "1.5%"     ,
     8.561 , 3.886     , "1.5%"     ,
     8.589 , 3.808     , "1.5%"     ,
     8.765 , 3.706     , "1.5%"     ,
     8.914 , 3.592     , "1.5%"     ,
     8.996 , 3.495     , "1.5%"     ,
     9.202 , 3.398     , "1.5%"     ,
     9.399 , 3.296     , "1.5%"     ,
     9.512 , 3.206     , "1.5%"     ,
     9.702 , 3.113     , "1.5%"     ,
     9.942 , 3.002     , "1.5%"     ,
    10.168 , 2.91      , "1.5%"     ,
    10.385 , 2.819     , "1.5%"     ,
    10.652 , 2.718     , "1.5%"     ,
    10.937 , 2.627     , "1.5%"     ,
    11.119 , 2.545     , "1.5%"     ,
    11.413 , 2.456     , "1.5%"     ,
    11.684 , 2.378     , "1.5%"     ,
    11.956 , 2.301     , "1.5%"     ,
    12.309 , 2.209     , "1.5%"     ,
    12.703 , 2.115     , "1.5%"     ,
    13.096 , 2.024     , "1.5%"     ,
    13.449 , 1.937     , "1.5%"     ,
    13.857 , 1.863     , "1.5%"     ,
    14.264 , 1.784     , "1.5%"     ,
    14.739 , 1.713     , "1.5%"     ,
    15.282 , 1.636     , "1.5%"     ,
    15.961 , 1.564     , "1.5%"     ,
    16.64  , 1.482     , "1.5%"     ,
    17.455 , 1.4       , "1.5%"     ,
    18.415 , 1.324     , "1.5%"     ,
    19.152 , 1.25      , "1.5%"     ,
    20.102 , 1.177     , "1.5%"     ,
    21.121 , 1.105     , "1.5%"     ,
    22.275 , 1.028     , "1.5%"     ,
    23.633 , 0.952     , "1.5%"     ,
    25.126 , 0.88      , "1.5%"     ,
    26.62  , 0.813     , "1.5%"     ,
    28.113 , 0.754     , "1.5%"     ,
    29.607 , 0.7       , "1.5%"     ,
    31.1   , 0.653     , "1.5%"     ,
    32.594 , 0.61      , "1.5%"     ,
    34.087 , 0.569     , "1.5%"     ,
    35.581 , 0.534     , "1.5%"     ,
    37.074 , 0.501     , "1.5%"     ,
    38.568 , 0.471     , "1.5%"     ,
    40.061 , 0.443     , "1.5%"     ,
    41.555 , 0.418     , "1.5%"     ,
    43.048 , 0.396     , "1.5%"     ,
    44.542 , 0.375     , "1.5%"     ,
    46.067 , 0.36      , "1.5%"     ,
    47.529 , 0.338     , "1.5%"     ,
    49.023 , 0.322     , "1.5%"     ,
    50.516 , 0.308     , "1.5%"     ,
    52.01  , 0.295     , "1.5%"     ,
    53.503 , 0.283     , "1.5%"     ,
    54.997 , 0.272     , "1.5%"     ,
    56.49  , 0.262     , "1.5%"     ,
    57.984 , 0.254     , "1.5%"     ,
    59.477 , 0.247     , "1.5%"     ,
    60.971 , 0.24      , "1.5%"     ,
    62.464 , 0.235     , "1.5%"     ,
    63.958 , 0.23      , "1.5%"     ,
    65.452 , 0.224     , "1.5%"     ,
    66.945 , 0.221     , "1.5%"     ,
    68.439 , 0.217     , "1.5%"     ,
    70.000 , 0.214     , "1.5%"
  ) |>
    dplyr::mutate(
      expansion = forcats::as_factor(.data$expansion)
    )

  xlab = ifelse(lang == "es", "Porcentaje de arcilla", "Clay percentage")
  ylab = ifelse(
    lang == "es",
    "Actividad, A",
    "Activity, A"
  )
  col.lab = ifelse(lang == "es", "Expansi\u00f3n", "Expansion")

  dat.labels = data.frame(
    clay = c(40, 29, 15, 11),
    activity = c(2.2, 1.6, .65, 1.2),
    expansion = if (lang == "en") {
      c("Very high", "High", "Medium", "Low")
    } else {
      c("Muy alta", "Alta", "Media", "Baja")
    }
  )

  if (clay.max > 70) {
    last_obs = dat |>
      dplyr::slice_max(.data$clay, n = 1, by = .data$expansion) |>
      dplyr::select(
        .data$expansion,
        clay_last = .data$clay,
        activity_last = .data$activity
      )

    dat2 = dat |>
      dplyr::nest_by(.data$expansion) |>
      dplyr::mutate(
        mod = list(
          stats::nls(activity ~ SSinvpoly3(clay, a, b, c, d), data = .data$data)
        ),
        dry = list(broom::augment(
          .data$mod,
          newdata = data.frame(clay = seq(70, clay.max, length.out = 70))
        ))
      ) |>
      tidyr::unnest(cols = c(.data$dry)) |>
      dplyr::select(
        .data$clay,
        -c(.data$mod, .data$data),
        activity = .data$.fitted,
        .data$expansion
      ) |>
      dplyr::ungroup() |>
      dplyr::left_join(last_obs, by = "expansion") |>
      dplyr::mutate(
        shift = .data$activity_last - dplyr::first(.data$activity),
        activity = .data$activity + .data$shift,
        .by = .data$expansion
      ) |>
      dplyr::filter(.data$clay > .data$clay_last) |> # drop the duplicated join point
      dplyr::select(.data$clay, .data$activity, .data$expansion)
    dat = dplyr::bind_rows(dat, dat2)
  }

  expansion = ggplot2::ggplot(
    dat,
    ggplot2::aes(
      .data$clay,
      .data$activity,
      col = .data$expansion,
      group = .data$expansion
    )
  ) +
    ggplot2::geom_path() +
    ggplot2::geom_text(
      ggplot2::aes(.data$clay, .data$activity, label = .data$expansion),
      dat.labels,
      size = 4,
      vjust = 0,
      show.legend = F,
      inherit.aes = F
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = 15,
        y = .85,
        xend = 20,
        yend = 1.5
      ),
      arrow = ggplot2::arrow(length = ggplot2::unit(0.01, "npc")),
      col = 'black'
    ) +
    ggplot2::scale_color_manual(values = c('#BB2751', '#2F6D68', '#2F6498')) +
    ggplot2::labs(x = xlab, y = ylab, col = col.lab) +
    ggplot2::scale_x_continuous(
      limits = c(0, clay.max),
      breaks = seq(0, clay.max, 10)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 5),
      breaks = 0:5
    ) +
    ggplot2::coord_cartesian(
      expand = FALSE,
      ratio = 15
    ) +
    ggplot2::theme_bw(base_size = 14)

  return(expansion)
}
