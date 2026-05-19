#' Feldspar Ternary Diagram
#' @description \code{ternary_feldspars()} draws either a static or interactive ternary diagram, in english or spanish. It is a base diagram where data can be plotted.
#' @param output The output format: "ggplot" or "plotly" (default is "ggplot")
#' @param language The language to be displayed: "en" for english or "es" for spanish (deafult is "en")
#' @param opacity Transparency level (default is 0.5)
#'
#' @return Feldspar ternary diagram in the desired format (object)
#' @export
#' @importFrom ggplot2 .data
#'
#' @details For plotting data on the ggplot object it would be easier if the names of the dataframe are "p", "f", and "a", that way it gets mapped automatically, if not make sure to use "aes(x=Na,y=K,z=Ca)".
#' For plotting on the plotly object the mapping of the new data should be as shown in the example: \code{a = ~K, b = ~Na, c = ~Ca}, where \code{a} refers to the top ("K"), \code{b} refers to the bottom left ("Na"), and \code{c} refers to the bottom right ("Ca").
#' The examples show basic usage and how to add data, which can be more customizable.
#'
#' @family classification plots, petrology
#'
#' @examples
#' library(ggplot2)
#' library(plotly)
#'
#' d = data.frame(K=c(80,5,30),
#'                Na=c(15,60,65),
#'                Ca=c(5,35,5))
#'
#' # adding data to ggplot object
#' ternary_feldspars() + geom_point(data = d)
#'
#' # adding data to plotly object
#' ternary_feldspars('plotly') %>%
#'   add_trace(a = ~K, b = ~Na, c = ~Ca,
#'             data = d,
#'             name = 'My data',
#'             type = "scatterternary",
#'             mode = "markers",
#'             marker = list(size=8,color='coral',
#'                           symbol=3,opacity=.9),
#'             hovertemplate = paste0('K: %{a}<br>',
#'                                    'Na: %{b}<br>',
#'                                    'Ca: %{c}'))
#'
ternary_feldspars = function(
  output = c('ggplot', 'plotly'),
  language = c('en', 'es'),
  opacity = 0.5
) {
  tb.feld = tibble::tribble(
    ~K    , ~Na   , ~Ca   , ~Label                             , ~Label.es                         ,
    100   ,   0   ,   0   , "Sanidine / Orthoclase Microcline" , "Sanidina / Ortoclasa Microclina" ,
     37   ,  63   ,   0   , "Sanidine / Orthoclase Microcline" , "Sanidina / Ortoclasa Microclina" ,
     31   ,  59   ,  10   , "Sanidine / Orthoclase Microcline" , "Sanidina / Ortoclasa Microclina" ,
     90   ,   0   ,  10   , "Sanidine / Orthoclase Microcline" , "Sanidina / Ortoclasa Microclina" ,
    100   ,   0   ,   0   , "Sanidine / Orthoclase Microcline" , "Sanidina / Ortoclasa Microclina" ,
     37   ,  63   ,   0   , "Anorthoclase"                     , "Anortosita"                      ,
     31   ,  59   ,  10   , "Anorthoclase"                     , "Anortosita"                      ,
     30   ,  60   ,  10   , "Anorthoclase"                     , "Anortosita"                      ,
     28.3 ,  61.4 ,  10.3 , "Anorthoclase"                     , "Anortosita"                      ,
     26.8 ,  62.5 ,  10.7 , "Anorthoclase"                     , "Anortosita"                      ,
     25.0 ,  63.5 ,  11.5 , "Anorthoclase"                     , "Anortosita"                      ,
     23.7 ,  64.2 ,  12.1 , "Anorthoclase"                     , "Anortosita"                      ,
     22.5 ,  64.7 ,  12.9 , "Anorthoclase"                     , "Anortosita"                      ,
     20.9 ,  65.2 ,  13.8 , "Anorthoclase"                     , "Anortosita"                      ,
     20.1 ,  65.4 ,  14.5 , "Anorthoclase"                     , "Anortosita"                      ,
     18.4 ,  65.8 ,  15.8 , "Anorthoclase"                     , "Anortosita"                      ,
     17.4 ,  65.9 ,  16.7 , "Anorthoclase"                     , "Anortosita"                      ,
     16.4 ,  66.0 ,  17.6 , "Anorthoclase"                     , "Anortosita"                      ,
     15.6 ,  65.9 ,  18.5 , "Anorthoclase"                     , "Anortosita"                      ,
     14.6 ,  65.8 ,  19.6 , "Anorthoclase"                     , "Anortosita"                      ,
     10   ,  80   ,  10   , "Anorthoclase"                     , "Anortosita"                      ,
     10   ,  90   ,   0.0 , "Anorthoclase"                     , "Anortosita"                      ,
     37   ,  63   ,   0   , "Anorthoclase"                     , "Anortosita"                      ,
      0   , 100   ,   0   , "Albite"                           , "Albita"                          ,
     10   ,  90   ,   0   , "Albite"                           , "Albita"                          ,
     10   ,  80   ,  10   , "Albite"                           , "Albita"                          ,
      0   ,  90   ,  10   , "Albite"                           , "Albita"                          ,
      0   , 100   ,   0   , "Albite"                           , "Albita"                          ,
      0   ,  90   ,  10   , "Oligoclase"                       , "Oligoclasa"                      ,
     10   ,  80   ,  10   , "Oligoclase"                       , "Oligoclasa"                      ,
     14.6 ,  65.8 ,  19.6 , "Oligoclase"                       , "Oligoclasa"                      ,
     13.9 ,  65.6 ,  20.5 , "Oligoclase"                       , "Oligoclasa"                      ,
     13.1 ,  65.4 ,  21.5 , "Oligoclase"                       , "Oligoclasa"                      ,
     12.6 ,  65.0 ,  22.4 , "Oligoclase"                       , "Oligoclasa"                      ,
     11.8 ,  64.7 ,  23.4 , "Oligoclase"                       , "Oligoclasa"                      ,
     11.3 ,  64.3 ,  24.3 , "Oligoclase"                       , "Oligoclasa"                      ,
     10.8 ,  63.9 ,  25.2 , "Oligoclase"                       , "Oligoclasa"                      ,
     10.4 ,  63.5 ,  26.1 , "Oligoclase"                       , "Oligoclasa"                      ,
     10   ,  63   ,  27   , "Oligoclase"                       , "Oligoclasa"                      ,
      0   ,  70   ,  30   , "Oligoclase"                       , "Oligoclasa"                      ,
      0   ,  90   ,  10   , "Oligoclase"                       , "Oligoclasa"                      ,
      0   ,  70   ,  30   , "Andesine"                         , "Andesina"                        ,
     10   ,  63   ,  27   , "Andesine"                         , "Andesina"                        ,
     10   ,  45   ,  45   , "Andesine"                         , "Andesina"                        ,
      0   ,  50   ,  50   , "Andesine"                         , "Andesina"                        ,
      0   ,  70   ,  30   , "Andesine"                         , "Andesina"                        ,
      0   ,  50   ,  50   , "Labradorite"                      , "Labradorita"                     ,
     10   ,  45   ,  45   , "Labradorite"                      , "Labradorita"                     ,
     10   ,  27   ,  63   , "Labradorite"                      , "Labradorita"                     ,
      0   ,  30   ,  70   , "Labradorite"                      , "Labradorita"                     ,
      0   ,  50   ,  50   , "Labradorite"                      , "Labradorita"                     ,
      0   ,  30   ,  70   , "Bytownite"                        , "Bitownita"                       ,
     10   ,  27   ,  63   , "Bytownite"                        , "Bitownita"                       ,
     10   ,  10   ,  80   , "Bytownite"                        , "Bitownita"                       ,
      0   ,  10   ,  90   , "Bytownite"                        , "Bitownita"                       ,
      0   ,  30   ,  70   , "Bytownite"                        , "Bitownita"                       ,
      0   ,   0   , 100   , "Anorthite"                        , "Anortita"                        ,
      0   ,  10   ,  90   , "Anorthite"                        , "Anortita"                        ,
     10   ,  10   ,  80   , "Anorthite"                        , "Anortita"                        ,
     10   ,   0   ,  90   , "Anorthite"                        , "Anortita"                        ,
      0   ,   0   , 100   , "Anorthite"                        , "Anortita"
  ) %>%
    dplyr::mutate(dplyr::across(.data$Label:.data$Label.es, forcats::as_factor))

  # reusable function for creating annotation object
  label <- function(txt) {
    list(
      text = txt,
      x = 0.1,
      y = 1,
      ax = 0,
      ay = 0,
      xref = "paper",
      yref = "paper",
      align = "center",
      font = list(size = 15, color = "white"),
      bgcolor = "#b3b3b3",
      bordercolor = "black",
      borderwidth = 2
    )
  }

  # reusable function for axis formatting
  axis <- function(txt) {
    list(
      title = txt,
      tickformat = ".0%",
      tickfont = list(size = 10)
    )
  }

  feld.ternaryAxes.en <- list(
    aaxis = axis("K"),
    baxis = axis("Na"),
    caxis = axis("Ca")
  )

  feld.ternaryAxes.es <- list(
    aaxis = axis("K"),
    baxis = axis("Na"),
    caxis = axis("Ca")
  )

  feld.pal = c(
    "#EB483D",
    "#F6CEC9",
    "#C0A3DC",
    "#CAD7EF",
    "#97B0DE",
    "#648BCF",
    "#2C568F",
    "#14243F"
  )

  if (any(output == 'ggplot' & language == 'en')) {
    feld <- ggtern::ggtern(
      data = tb.feld,
      ggtern::aes(.data$Na, .data$K, .data$Ca)
    ) +
      ggplot2::geom_polygon(
        ggplot2::aes(
          fill = .data$Label,
          color = .data$Label,
          group = .data$Label
        ),
        alpha = opacity
      ) +
      ggtern::theme_bw() +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_manual(values = feld.pal) +
      ggplot2::scale_color_manual(values = feld.pal) +
      ggplot2::labs(
        fill = "Feldspars",
        color = "Feldspars",
        T = "K",
        L = "Na",
        R = "Ca"
      )
  } else if (any(output == 'ggplot' & language == 'es')) {
    feld <- ggtern::ggtern(
      data = tb.feld,
      ggtern::aes(.data$Na, .data$K, .data$Ca)
    ) +
      ggplot2::geom_polygon(
        ggplot2::aes(
          fill = .data$Label.es,
          color = .data$Label.es,
          group = .data$Label.es
        ),
        alpha = opacity
      ) +
      ggtern::theme_bw() +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_manual(values = feld.pal) +
      ggplot2::scale_color_manual(values = feld.pal) +
      ggplot2::labs(
        fill = "Feldespatos",
        color = "Feldespatos",
        T = "K",
        L = "Na",
        R = "Ca"
      )
  } else if (any(output == 'plotly' & language == 'en')) {
    feld = plotly::plot_ly() %>%
      plotly::add_trace(
        data = tb.feld,
        a = ~K,
        b = ~Na,
        c = ~Ca,
        color = ~Label,
        colors = feld.pal %>% purrr::set_names(levels(tb.feld$Label)),
        opacity = opacity * 2,
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        hoveron = 'fills'
      ) %>%
      plotly::layout(
        ternary = feld.ternaryAxes.en,
        legend = list(title = list(text = '<b> Feldspars </b>')),
        margin = list(autoexpand = T, t = 35)
      ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Feldspars',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  } else if (any(output == 'plotly' & language == 'es')) {
    feld = plotly::plot_ly() %>%
      plotly::add_trace(
        data = tb.feld,
        a = ~K,
        b = ~Na,
        c = ~Ca,
        color = ~Label.es,
        colors = feld.pal %>% purrr::set_names(levels(tb.feld$Label.es)),
        opacity = opacity * 2,
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        hoveron = 'fills'
      ) %>%
      plotly::layout(
        ternary = feld.ternaryAxes.es,
        legend = list(title = list(text = '<b> Feldespatos </b>')),
        margin = list(autoexpand = T, t = 35)
      ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Feldspars',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  }

  return(feld)
}
