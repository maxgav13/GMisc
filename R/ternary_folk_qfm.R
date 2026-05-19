#' Folk's Ternary Diagram for Sedimentary Rocks
#' @description \code{ternary_folk_qfm()} draws either a static or interactive ternary diagram, in english or spanish. It is a base diagram where data can be plotted.
#' @param output The output format: "ggplot" or "plotly" (default is "ggplot")
#' @param language The language to be displayed: "en" for english or "es" for spanish (deafult is "en")
#' @param opacity Transparency level (default is 0.5)
#'
#' @return Folk's ternary diagram for sandstones in the desired format (object)
#' @export
#' @importFrom ggplot2 .data
#'
#' @details For plotting data on the ggplot object it would be easier if the names of the dataframe are "f", "q", and "m", that way it gets mapped automatically, if not make sure to use "aes(x=f,y=q,z=m)".
#' For plotting on the plotly object the mapping of the new data should be as shown in the example: \code{a = ~q, b = ~f, c = ~m}, where \code{a} refers to the top ("q"), \code{b} refers to the bottom left ("f"), and \code{c} refers to the bottom right ("m").
#' The examples show basic usage and how to add data, which can be more customizable.
#'
#' @family classification plots, petrology
#'
#' @examples
#' library(ggplot2)
#' library(plotly)
#'
#' d = data.frame(q=c(23,40,60),
#'                f=c(27,50,30),
#'                m=c(50,10,10))
#'
#' # adding data to ggplot object
#' ternary_folk_qfm() + geom_point(data = d)
#'
#' # adding data to plotly object
#' ternary_folk_qfm('plotly') %>%
#'   add_trace(a = ~q, b = ~f, c = ~m,
#'             data = d,
#'             name = 'My data',
#'             type = "scatterternary",
#'             mode = "markers",
#'             marker = list(size=8,color='coral',
#'                           symbol=3,opacity=.9),
#'             hovertemplate = paste0('Q: %{a}<br>',
#'                                    'F: %{b}<br>',
#'                                    'M: %{c}'))
#'
ternary_folk_qfm = function(
  output = c('ggplot', 'plotly'),
  language = c('en', 'es'),
  opacity = .5
) {
  tb.Folk = tibble::tribble(
    ~q    , ~f     , ~m     , ~Label                     , ~Label.es                       ,
    100   ,   0    ,   0    , "Quartzarenite"            , "Quarzo arenita"                ,
     95   ,   5    ,   0    , "Quartzarenite"            , "Quarzo arenita"                ,
     91.6 ,   4.2  ,   4.2  , "Quartzarenite"            , "Quarzo arenita"                ,
     95   ,   0    ,   5    , "Quartzarenite"            , "Quarzo arenita"                ,
    100   ,   0    ,   0    , "Quartzarenite"            , "Quarzo arenita"                ,
     95   ,   5    ,   0    , "Subarkose"                , "Subarcosa"                     ,
     75   ,  25    ,   0    , "Subarkose"                , "Subarcosa"                     ,
     66   ,  24    ,  10    , "Subarkose"                , "Subarcosa"                     ,
     80   ,  10    ,  10    , "Subarkose"                , "Subarcosa"                     ,
     91.6 ,   4.2  ,   4.2  , "Subarkose"                , "Subarcosa"                     ,
     95   ,   5    ,   0    , "Subarkose"                , "Subarcosa"                     ,
     95   ,   0    ,   5    , "Subgraywacke"             , "Subgrauvaca"                   ,
     75   ,   0    ,  25    , "Subgraywacke"             , "Subgrauvaca"                   ,
     66   ,  10    ,  24    , "Subgraywacke"             , "Subgrauvaca"                   ,
     80   ,  10    ,  10    , "Subgraywacke"             , "Subgrauvaca"                   ,
     91.6 ,   4.2  ,   4.2  , "Subgraywacke"             , "Subgrauvaca"                   ,
     95   ,   0    ,   5    , "Subgraywacke"             , "Subgrauvaca"                   ,
     66   ,  24    ,  10    , "Feldspathic subgraywacke" , "Subgrauvaca feldesp\u00e1tica" ,
     80   ,  10    ,  10    , "Feldspathic subgraywacke" , "Subgrauvaca feldesp\u00e1tica" ,
     66   ,  10    ,  24    , "Feldspathic subgraywacke" , "Subgrauvaca feldesp\u00e1tica" ,
     52.5 ,  23.75 ,  23.75 , "Feldspathic subgraywacke" , "Subgrauvaca feldesp\u00e1tica" ,
     66   ,  24    ,  10    , "Feldspathic subgraywacke" , "Subgrauvaca feldesp\u00e1tica" ,
     75   ,  25    ,   0    , "Arkose"                   , "Arcosa"                        ,
      0   , 100    ,   0    , "Arkose"                   , "Arcosa"                        ,
      0   ,  90    ,  10    , "Arkose"                   , "Arcosa"                        ,
     66   ,  24    ,  10    , "Arkose"                   , "Arcosa"                        ,
     75   ,  25    ,   0    , "Arkose"                   , "Arcosa"                        ,
      0   ,  90    ,  10    , "Impure Arkose"            , "Arcosa impura"                 ,
     66   ,  24    ,  10    , "Impure Arkose"            , "Arcosa impura"                 ,
     52.5 ,  23.75 ,  23.75 , "Impure Arkose"            , "Arcosa impura"                 ,
      0   ,  50    ,  50    , "Impure Arkose"            , "Arcosa impura"                 ,
      0   ,  90    ,  10    , "Impure Arkose"            , "Arcosa impura"                 ,
     52.5 ,  23.75 ,  23.75 , "Feldspathic graywacke"    , "Grauvaca feldesp\u00e1tica"    ,
     66   ,  10    ,  24    , "Feldspathic graywacke"    , "Grauvaca feldesp\u00e1tica"    ,
      0   ,  10    ,  90    , "Feldspathic graywacke"    , "Grauvaca feldesp\u00e1tica"    ,
      0   ,  50    ,  50    , "Feldspathic graywacke"    , "Grauvaca feldesp\u00e1tica"    ,
     52.5 ,  23.75 ,  23.75 , "Feldspathic graywacke"    , "Grauvaca feldesp\u00e1tica"    ,
     66   ,  10    ,  24    , "Graywacke"                , "Grauvaca"                      ,
     75   ,   0    ,  25    , "Graywacke"                , "Grauvaca"                      ,
      0   ,   0    , 100    , "Graywacke"                , "Grauvaca"                      ,
      0   ,  10    ,  90    , "Graywacke"                , "Grauvaca"                      ,
     66   ,  10    ,  24    , "Graywacke"                , "Grauvaca"
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

  Folk.ternaryAxes <- list(
    aaxis = axis("Q"),
    baxis = axis("F"),
    caxis = axis("M")
  )

  Folk.pal = c(
    "#777777",
    "#AACDC9",
    "#C0CF98",
    "#B2CEAC",
    "#2F4996",
    "#D3C6E2",
    "#EBD1C0",
    "#564E37"
  )

  if (any(output == 'ggplot' & language == 'en')) {
    Folk <- ggtern::ggtern(
      data = tb.Folk,
      ggtern::aes(.data$f, .data$q, .data$m)
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
      ggplot2::scale_fill_manual(values = Folk.pal) +
      ggplot2::scale_color_manual(values = Folk.pal) +
      ggplot2::labs(
        fill = "Sedimentary rock",
        color = "Sedimentary rock",
        T = "Q",
        L = "F",
        R = "M"
      )
  } else if (any(output == 'ggplot' & language == 'es')) {
    Folk <- ggtern::ggtern(
      data = tb.Folk,
      ggtern::aes(.data$f, .data$q, .data$m)
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
      ggplot2::scale_fill_manual(values = Folk.pal) +
      ggplot2::scale_color_manual(values = Folk.pal) +
      ggplot2::labs(
        fill = "Roca sedimentaria",
        color = "Roca sedimentaria",
        T = "Q",
        L = "F",
        R = "M"
      )
  } else if (any(output == 'plotly' & language == 'en')) {
    Folk = plotly::plot_ly() %>%
      plotly::add_trace(
        data = tb.Folk,
        a = ~q,
        b = ~f,
        c = ~m,
        color = ~Label,
        colors = Folk.pal %>% purrr::set_names(levels(tb.Folk$Label)),
        opacity = opacity * 2,
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        hoveron = 'fills'
      ) %>%
      plotly::layout(
        ternary = Folk.ternaryAxes,
        legend = list(title = list(text = '<b> Sedimentary rock </b>')),
        margin = list(autoexpand = T, t = 35)
      ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Folk_qfm',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  } else if (any(output == 'plotly' & language == 'es')) {
    Folk = plotly::plot_ly() %>%
      plotly::add_trace(
        data = tb.Folk,
        a = ~q,
        b = ~f,
        c = ~m,
        color = ~Label.es,
        colors = Folk.pal %>% purrr::set_names(levels(tb.Folk$Label.es)),
        opacity = opacity * 2,
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        hoveron = 'fills'
      ) %>%
      plotly::layout(
        ternary = Folk.ternaryAxes,
        legend = list(title = list(text = '<b> Roca sedimentaria </b>')),
        margin = list(autoexpand = T, t = 35)
      ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Folk_qfm',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  }

  return(Folk)
}
