#' @title Particle size distribution graph template
#'
#' @description Create a template for particle size distribution graphs with logarithmic x-axis and standard vertical lines indicating common soil classification boundaries (clay/silt at 0.002 mm, silt/sand at 0.075 mm, sand/gravel at 4.75 mm). The graph is set up with appropriate axis labels and breaks for typical particle size ranges.
#'
#' @param lang character. Label language; \code{'es'} for Spanish or \code{'en'} for English. Default \code{'en'}.
#'
#' @returns A ggplot2 object with the particle size distribution graph.
#'
#' @export
#'
#' @family soil mechanics
#'
#' @examples
#' suelo = dplyr::tribble(
#'  ~m      , ~m.n    , ~d     , ~pp    , ~sample  ,
#'  "2"     ,   2     , 50.8   , 100    , "A"      ,
#'  "1 1/2" ,   1.5   , 38.1   ,  83.19 , "A"      ,
#'  "1"     ,   1     , 25.4   ,  66.95 , "A"      ,
#'  "1/2"   ,    .5   , 12.7   ,  61.08 , "A"      ,
#'  "3/8"   ,   0.375 ,  9.5   ,  56.75 , "A"      ,
#'  "4"     ,   4     ,  4.75  ,  49.67 , "A"      ,
#'  "10"    ,  10     ,  2     ,  41.69 , "A"      ,
#'  "20"    ,  20     ,  0.85  ,  36.11 , "A"      ,
#'  "40"    ,  40     ,  0.425 ,  30.92 , "A"      ,
#'  "60"    ,  60     ,  0.25  ,  28.25 , "A"      ,
#'  "100"   , 100     ,  0.15  ,  25.91 , "A"      ,
#'  "200"   , 200     ,  0.075 ,  21.74 , "A"      ,
#'  "3/4"   ,    .75  , 19     , 100    , "B"      ,
#'  "1/2"   ,    .5   , 12.7   ,  97.97 , "B"      ,
#'  "3/8"   ,   0.375 ,  9.5   ,  96.25 , "B"      ,
#'  "4"     ,   4     ,  4.75  ,  87.86 , "B"      ,
#'  "10"    ,  10     ,  2     ,  75.11 , "B"      ,
#'  "20"    ,  20     ,  0.85  ,  62.8  , "B"      ,
#'  "40"    ,  40     ,  0.425 ,  55.82 , "B"      ,
#'  "60"    ,  60     ,  0.25  ,  52.18 , "B"      ,
#'  "100"   , 100     ,  0.15  ,  47.79 , "B"      ,
#'  "200"   , 200     ,  0.075 ,  42.96 , "B"      ,
#'  "3/8"   ,   0.375 ,  9.5   , 100    , "C"      ,
#'  "4"     ,   4     ,  4.75  ,  99.27 , "C"      ,
#'  "10"    ,  10     ,  2     ,  98.91 , "C"      ,
#'  "20"    ,  20     ,  0.85  ,  91.23 , "C"      ,
#'  "40"    ,  40     ,  0.425 ,  86.83 , "C"      ,
#'  "60"    ,  60     ,  0.25  ,  81.62 , "C"      ,
#'  "100"   , 100     ,  0.15  ,  75.70 , "C"      ,
#'  "200"   , 200     ,  0.075 ,  69.48 , "C"
#' )
#' particle_size_graph() +
#'   ggplot2::geom_point(
#'      ggplot2::aes(.data$d, .data$pp,
#'                   shape = .data$sample,
#'                   color = .data$sample),
#'      suelo, size = 3) +
#'   ggplot2::geom_line(ggplot2::aes(.data$d, .data$pp,
#'                      color = .data$sample), suelo) +
#'   ggplot2::scale_shape_manual(values = 1:3)
#'
particle_size_graph = function(
  lang = 'en'
) {
  lab.x = ifelse(
    lang == 'es',
    'Di\u00e1metro de part\u00edcula, D (mm)',
    'Particle size, D (mm)'
  )
  lab.y = ifelse(lang == 'es', 'Porcentaje Pasa (%)', 'Percent Finer (%)')

  gg_granu = ggplot2::ggplot() +
    ggplot2::geom_vline(
      xintercept = c(.002, .075, 4.75),
      lty = 2,
      linewidth = .25
    ) +
    ggplot2::scale_x_continuous(
      breaks = c(.0001, .001, .01, .1, 1, 10, 100),
      labels = c('.0001', '.001', '.01', '.1', '1', '10', '100'),
      minor_breaks = c(
        seq(.0001, .0009, .0001),
        seq(.001, .009, .001),
        seq(.01, .09, .01),
        seq(.1, .9, .1),
        1:9,
        seq(10, 100, 10)
      ),
      trans = 'log10'
    ) +
    ggplot2::scale_y_continuous(breaks = seq(0, 100, 20)) +
    ggplot2::coord_cartesian(
      ylim = c(0, 100),
      xlim = c(.0001, 110),
      clip = "off",
      expand = F
    ) +
    ggplot2::labs(x = lab.x, y = lab.y) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 14),
      axis.text = ggplot2::element_text(size = 12)
    )

  return(gg_granu)
}
