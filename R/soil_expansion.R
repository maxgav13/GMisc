#' Soil expansion potential chart
#'
#' Draws a soil expansion potential chart—plasticity index vs clay
#' percentage—with four boundary curves that separate the zones of low,
#' medium, high, and very high expansion potential.
#'
#' @param lang Character. Language for axis titles: \code{"en"} (default) for
#'   English, \code{"es"} for Spanish.
#'
#' @returns A ggplot object with clay percentage on the x-axis and
#'   plasticity index on the y-axis, containing the four expansion
#'   potential boundary curves and their zone labels.
#'
#' @references Holtz, R. D., Kovacs, W. D., & Sheahan, T. C. (2023). An Introduction to Geotechnical Engineering (3rd ed.). Pearson Education.
#'
#' @family classification plots, soil mechanics
#' @export
#' @examples
#' # English labels (default)
#' soil_expansion()
#'
#' dat = data.frame(
#'   clay = c(20, 40, 60),
#'   pi = c(10, 23, 40),
#'   loc = c("A", "B", "C")
#' )
#' # Spanish labels
#' soil_expansion(lang = "es") +
#' ggplot2::geom_point(data = dat,
#'                      ggplot2::aes(x = clay, y = pi, color = loc),
#'                      size = 3,
#'                      inherit.aes = FALSE # needed
#'                     )
soil_expansion = function(lang = "en") {
  # dat = utils::read.csv(
  #   system.file("soil_expansion.csv", package = "GMisc")
  # )

  dat = tibble::tribble(
    ~clay   , ~pi     , ~expansion ,
     27.000 , 100.00  , "vh"       ,
     27.000 ,  31.000 , "vh"       ,
     62.000 ,  31.000 , "vh"       ,
     19.000 , 100.00  , "h"        ,
     19.000 ,  24.000 , "h"        ,
     48.000 ,  24.000 , "h"        ,
     13.000 , 100.00  , "m"        ,
     13.000 ,  12.000 , "m"        ,
     25.00  ,  12.000 , "m"        ,
     40.000 ,  20.000 , "m"        ,
     50.000 ,  25.000 , "m"        ,
     60.000 ,  30.000 , "m"        ,
     80     ,  40.000 , "m"        ,
    100     ,  50.000 , "m"
  )

  xlab = ifelse(lang == "es", "Porcentaje de arcilla", "Clay percentage")
  ylab = ifelse(
    lang == "es",
    "\u00cdndice de plasticidad, IP",
    "Plasticity index, PI"
  )

  dat.labels = data.frame(
    clay = c(45, 40, 25, 50),
    pi = c(35, 26, 18, 12),
    expansion = if (lang == "en") {
      c("Very high", "High", "Medium", "Low")
    } else {
      c("Muy alta", "Alta", "Media", "Baja")
    }
  )

  expansion = ggplot2::ggplot(
    dat,
    ggplot2::aes(
      .data$clay,
      .data$pi,
      group = .data$expansion
    )
  ) +
    ggplot2::geom_path() +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$expansion),
      dat.labels,
      size = 4,
      vjust = 0,
      show.legend = F
    ) +
    ggplot2::labs(x = xlab, y = ylab) +
    ggplot2::scale_x_continuous(
      limits = c(0, 100),
      breaks = seq(0, 100, 10)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 100),
      breaks = seq(0, 100, 10)
    ) +
    ggplot2::coord_cartesian(
      expand = FALSE,
      ratio = 1
    ) +
    ggplot2::theme_bw(base_size = 14)

  return(expansion)
}
