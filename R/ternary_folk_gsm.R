#' Folk's Ternary Diagram for Clastic Coarse-Grained Rocks
#' @description \code{ternary_folk_gsm()} draws either a static or interactive ternary diagram, in english or spanish. It is a base diagram where data can be plotted.
#' @param output The output format: "ggplot" or "plotly" (default is "ggplot")
#' @param language The language to be displayed: "en" for english or "es" for spanish (deafult is "en")
#' @param opacity Transparency level (default is 0.5)
#'
#' @return Folk's ternary diagram for clastic rocks in the desired format (object)
#' @export
#' @importFrom ggplot2 .data
#'
#' @details For plotting data on the ggplot object it would be easier if the names of the dataframe are "s", "g", and "m", that way it gets mapped automatically, if not make sure to use "aes(x=s,y=g,z=m)".
#' For plotting on the plotly object the mapping of the new data should be as shown in the example: \code{a = ~g, b = ~s, c = ~m}, where \code{a} refers to the top ("g"), \code{b} refers to the bottom left ("s"), and \code{c} refers to the bottom right ("m").
#' The examples show basic usage and how to add data, which can be more customizable.
#' For a more complete description on how to use this function and similar ones, see
#' \code{vignette("classification_diagrams_vig")}
#'
#' @family classification plots, petrology
#'
#' @examples
#' library(ggplot2)
#' library(plotly)
#'
#' d = data.frame(g=c(23,40,60),
#'                s=c(27,50,30),
#'                m=c(50,10,10),
#'                loc=c("A", "B", "C"))
#'
#' # adding data to ggplot object
#' ternary_folk_gsm() + geom_point(aes(col = loc), data = d)
#'
#' # adding data to plotly object
#' ternary_folk_gsm('plotly') %>%
#'   add_trace(a = ~g, b = ~s, c = ~m,
#'             data = d,
#'             name = 'My data',
#'             type = "scatterternary",
#'             mode = "markers",
#'             marker = list(size=8,color='coral',
#'                           symbol=3,opacity=.9),
#'             hovertemplate = paste0('Gravel: %{a}<br>',
#'                                    'Sand: %{b}<br>',
#'                                    'Mud: %{c}'))
#'
#' # adding data to plotly object with custom colors
#' my_pal    <- c("A" = "red",      "B" = "blue",   "C" = "green")
#' my_shapes <- c("A" = "circle",   "B" = "square",  "C" = "diamond")
#'
#' ternary_folk_gsm('plotly') %>%
#'   add_trace(a = ~g, b = ~s, c = ~m,
#'             data = d,
#'             type = "scatterternary",
#'             mode = "markers",
#'             split = ~loc,
#'             marker = list(size=8,
#'                           color  = ~my_pal[loc],
#'                           symbol = ~my_shapes[loc],
#'                           opacity=.9),
#'             hovertemplate = paste0('Gravel: %{a}<br>',
#'                                    'Sand: %{b}<br>',
#'                                    'Mud: %{c}'))
#'
ternary_folk_gsm = function(
  output = c('ggplot', 'plotly'),
  language = c('en', 'es'),
  opacity = .5
) {
  output <- match.arg(output)
  language <- match.arg(language)

  tb.Folk = tibble::tribble(
    ~g  , ~s    , ~m    , ~Label                                                  , ~Label.es                                                      ,
    100 ,   0   ,   0   , "Conglomerate \n(Gravel)"                               , "Conglomerado \n(Grava)"                                       ,
     80 ,  20   ,   0   , "Conglomerate \n(Gravel)"                               , "Conglomerado \n(Grava)"                                       ,
     80 ,   0   ,  20   , "Conglomerate \n(Gravel)"                               , "Conglomerado \n(Grava)"                                       ,
    100 ,   0   ,   0   , "Conglomerate \n(Gravel)"                               , "Conglomerado \n(Grava)"                                       ,
     80 ,  20   ,   0   , "Sandy conglomerate \n(Sandy gravel)"                   , "Conglomerado arenoso \n(Grava arenosa)"                       ,
     80 ,  18   ,   2   , "Sandy conglomerate \n(Sandy gravel)"                   , "Conglomerado arenoso \n(Grava arenosa)"                       ,
     30 ,  63   ,   7   , "Sandy conglomerate \n(Sandy gravel)"                   , "Conglomerado arenoso \n(Grava arenosa)"                       ,
     30 ,  70   ,   0   , "Sandy conglomerate \n(Sandy gravel)"                   , "Conglomerado arenoso \n(Grava arenosa)"                       ,
     80 ,  20   ,   0   , "Sandy conglomerate \n(Sandy gravel)"                   , "Conglomerado arenoso \n(Grava arenosa)"                       ,
     80 ,  18   ,   2   , "Muddy sandy conglomerate \n(Muddy sandy gravel)"       , "Conglomerado arenoso lodoso \n(Grava arenosa lodosa)"         ,
     80 ,  10   ,  10   , "Muddy sandy conglomerate \n(Muddy sandy gravel)"       , "Conglomerado arenoso lodoso \n(Grava arenosa lodosa)"         ,
     30 ,  35   ,  35   , "Muddy sandy conglomerate \n(Muddy sandy gravel)"       , "Conglomerado arenoso lodoso \n(Grava arenosa lodosa)"         ,
     30 ,  63   ,   7   , "Muddy sandy conglomerate \n(Muddy sandy gravel)"       , "Conglomerado arenoso lodoso \n(Grava arenosa lodosa)"         ,
     80 ,  18   ,   2   , "Muddy sandy conglomerate \n(Muddy sandy gravel)"       , "Conglomerado arenoso lodoso \n(Grava arenosa lodosa)"         ,
     80 ,  10   ,  10   , "Muddy conglomerate \n(Muddy gravel)"                   , "Conglomerado lodoso \n(Grava lodosa)"                         ,
     80 ,   0   ,  20   , "Muddy conglomerate \n(Muddy gravel)"                   , "Conglomerado lodoso \n(Grava lodosa)"                         ,
     30 ,   0   ,  70   , "Muddy conglomerate \n(Muddy gravel)"                   , "Conglomerado lodoso \n(Grava lodosa)"                         ,
     30 ,  35   ,  35   , "Muddy conglomerate \n(Muddy gravel)"                   , "Conglomerado lodoso \n(Grava lodosa)"                         ,
     80 ,  10   ,  10   , "Muddy conglomerate \n(Muddy gravel)"                   , "Conglomerado lodoso \n(Grava lodosa)"                         ,
     30 ,  70   ,   0   , "Conglomeratic sandstone \n(Gravelly sand)"             , "Arenisca conglomer\u00e1tica \n(Arena gravosa)"               ,
     30 ,  63   ,   7   , "Conglomeratic sandstone \n(Gravelly sand)"             , "Arenisca conglomer\u00e1tica \n(Arena gravosa)"               ,
      5 ,  85.5 ,   9.5 , "Conglomeratic sandstone \n(Gravelly sand)"             , "Arenisca conglomer\u00e1tica \n(Arena gravosa)"               ,
      5 ,  95   ,   0   , "Conglomeratic sandstone \n(Gravelly sand)"             , "Arenisca conglomer\u00e1tica \n(Arena gravosa)"               ,
     30 ,  70   ,   0   , "Conglomeratic sandstone \n(Gravelly sand)"             , "Arenisca conglomer\u00e1tica \n(Arena gravosa)"               ,
     30 ,  63   ,   7   , "Conglomeratic muddy sandstone \n(Gravelly muddy sand)" , "Arenisca lodosa conglomer\u00e1tica \n(Arena lodosa gravosa)" ,
     30 ,  35   ,  35   , "Conglomeratic muddy sandstone \n(Gravelly muddy sand)" , "Arenisca lodosa conglomer\u00e1tica \n(Arena lodosa gravosa)" ,
      5 ,  47.5 ,  47.5 , "Conglomeratic muddy sandstone \n(Gravelly muddy sand)" , "Arenisca lodosa conglomer\u00e1tica \n(Arena lodosa gravosa)" ,
      5 ,  85.5 ,   9.5 , "Conglomeratic muddy sandstone \n(Gravelly muddy sand)" , "Arenisca lodosa conglomer\u00e1tica \n(Arena lodosa gravosa)" ,
     30 ,  63   ,   7   , "Conglomeratic muddy sandstone \n(Gravelly muddy sand)" , "Arenisca lodosa conglomer\u00e1tica \n(Arena lodosa gravosa)" ,
     30 ,  35   ,  35   , "Conglomeratic mudstone \n(Gravelly mud)"               , "Lodolita conglomer\u00e1tica \n(Lodo gravoso)"                ,
     30 ,   0   ,  70   , "Conglomeratic mudstone \n(Gravelly mud)"               , "Lodolita conglomer\u00e1tica \n(Lodo gravoso)"                ,
      5 ,   0   ,  95   , "Conglomeratic mudstone \n(Gravelly mud)"               , "Lodolita conglomer\u00e1tica \n(Lodo gravoso)"                ,
      5 ,  47.5 ,  47.5 , "Conglomeratic mudstone \n(Gravelly mud)"               , "Lodolita conglomer\u00e1tica \n(Lodo gravoso)"                ,
     30 ,  35   ,  35   , "Conglomeratic mudstone \n(Gravelly mud)"               , "Lodolita conglomer\u00e1tica \n(Lodo gravoso)"                ,
      5 ,  85.5 ,   9.5 , "Sandstone \n(Sand)"                                    , "Arenisca \n(Arena)"                                           ,
      0 ,  90   ,  10   , "Sandstone \n(Sand)"                                    , "Arenisca \n(Arena)"                                           ,
      0 , 100   ,   0   , "Sandstone \n(Sand)"                                    , "Arenisca \n(Arena)"                                           ,
      5 ,  95   ,   0   , "Sandstone \n(Sand)"                                    , "Arenisca \n(Arena)"                                           ,
      5 ,  85.5 ,   9.5 , "Sandstone \n(Sand)"                                    , "Arenisca \n(Arena)"                                           ,
      5 ,  85.5 ,   9.5 , "Muddy sandstone \n(Muddy sand)"                        , "Arenisca lodosa \n(Arena lodosa)"                             ,
      5 ,  47.5 ,  47.5 , "Muddy sandstone \n(Muddy sand)"                        , "Arenisca lodosa \n(Arena lodosa)"                             ,
      0 ,  50   ,  50   , "Muddy sandstone \n(Muddy sand)"                        , "Arenisca lodosa \n(Arena lodosa)"                             ,
      0 ,  90   ,  10   , "Muddy sandstone \n(Muddy sand)"                        , "Arenisca lodosa \n(Arena lodosa)"                             ,
      5 ,  85.5 ,   9.5 , "Muddy sandstone \n(Muddy sand)"                        , "Arenisca lodosa \n(Arena lodosa)"                             ,
      5 ,  47.5 ,  47.5 , "Sandy mudstone \n(Sandy mud)"                          , "Lodolita arenosa \n(Lodo arenoso)"                            ,
      5 ,   9.5 ,  85.5 , "Sandy mudstone \n(Sandy mud)"                          , "Lodolita arenosa \n(Lodo arenoso)"                            ,
      0 ,  10   ,  90   , "Sandy mudstone \n(Sandy mud)"                          , "Lodolita arenosa \n(Lodo arenoso)"                            ,
      0 ,  50   ,  50   , "Sandy mudstone \n(Sandy mud)"                          , "Lodolita arenosa \n(Lodo arenoso)"                            ,
      5 ,  47.5 ,  47.5 , "Sandy mudstone \n(Sandy mud)"                          , "Lodolita arenosa \n(Lodo arenoso)"                            ,
      5 ,   9.5 ,  85.5 , "Mudstone \n(Mud)"                                      , "Lodolita \n(Lodo)"                                            ,
      5 ,   0   ,  95   , "Mudstone \n(Mud)"                                      , "Lodolita \n(Lodo)"                                            ,
      0 ,   0   , 100   , "Mudstone \n(Mud)"                                      , "Lodolita \n(Lodo)"                                            ,
      0 ,  10   ,  90   , "Mudstone \n(Mud)"                                      , "Lodolita \n(Lodo)"                                            ,
      5 ,   9.5 ,  85.5 , "Mudstone \n(Mud)"                                      , "Lodolita \n(Lodo)"
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

  Folk.ternaryAxes.en <- list(
    aaxis = axis("Gravel"),
    baxis = axis("Sand"),
    caxis = axis("Mud")
  )

  Folk.ternaryAxes.es <- list(
    aaxis = axis("Grava"),
    baxis = axis("Arena"),
    caxis = axis("Lodo")
  )

  Folk.pal = c(
    "#FFF7EC",
    "#FEEBCF",
    "#FDDCAE",
    "#FDCA93",
    "#FCB17B",
    "#FC8D59",
    "#F16C4B",
    "#E0452F",
    "#C81C12",
    "#A80000",
    "#7F0000"
  )

  if (output == 'ggplot' & language == 'en') {
    Folk <- ggtern::ggtern(
      data = tb.Folk,
      ggtern::aes(.data$s, .data$g, .data$m)
    ) +
      ggplot2::geom_polygon(
        ggplot2::aes(
          fill = .data$Label,
          # color = .data$Label,
          group = .data$Label
        ),
        color = "black",
        linewidth = 0.25,
        alpha = opacity
      ) +
      ggtern::theme_bw() +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_manual(values = Folk.pal) +
      # ggplot2::scale_color_manual(values = Folk.pal) +
      ggplot2::labs(
        fill = "Rock (sediment)",
        # color = "Rock (sediment)",
        T = "Gravel",
        L = "Sand",
        R = "Mud"
      )
  } else if (output == 'ggplot' & language == 'es') {
    Folk <- ggtern::ggtern(
      data = tb.Folk,
      ggtern::aes(.data$s, .data$g, .data$m)
    ) +
      ggplot2::geom_polygon(
        ggplot2::aes(
          fill = .data$Label.es,
          # color = .data$Label.es,
          group = .data$Label.es
        ),
        color = "black",
        linewidth = 0.25,
        alpha = opacity
      ) +
      ggtern::theme_bw() +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_manual(values = Folk.pal) +
      # ggplot2::scale_color_manual(values = Folk.pal) +
      ggplot2::labs(
        fill = "Roca (sedimento)",
        # color = "Roca (sedimento)",
        T = "Grava",
        L = "Arena",
        R = "Lodo"
      )
  } else if (output == 'plotly' & language == 'en') {
    Folk = plotly::plot_ly() %>%
      plotly::add_trace(
        data = tb.Folk,
        a = ~g,
        b = ~s,
        c = ~m,
        color = ~Label,
        colors = Folk.pal %>% purrr::set_names(levels(tb.Folk$Label)),
        opacity = opacity * 2,
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        line = list(color = "black", width = 0.5),
        hoverinfo = 'text',
        hoveron = 'fills'
      ) %>%
      plotly::layout(
        ternary = Folk.ternaryAxes.en,
        legend = list(title = list(text = '<b> Rock (sediment) </b>')),
        margin = list(autoexpand = T, t = 35)
      ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Folk_gsm',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  } else if (output == 'plotly' & language == 'es') {
    Folk = plotly::plot_ly() %>%
      plotly::add_trace(
        data = tb.Folk,
        a = ~g,
        b = ~s,
        c = ~m,
        color = ~Label.es,
        colors = Folk.pal %>% purrr::set_names(levels(tb.Folk$Label.es)),
        opacity = opacity * 2,
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        line = list(color = "black", width = 0.5),
        hoverinfo = 'text',
        hoveron = 'fills'
      ) %>%
      plotly::layout(
        ternary = Folk.ternaryAxes.es,
        legend = list(title = list(text = '<b> Roca (sedimento) </b>')),
        margin = list(autoexpand = T, t = 35)
      ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Folk_gsm',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  }

  return(Folk)
}
