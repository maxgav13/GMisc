#' Folk's Ternary Diagram for Sandstones
#' @description \code{ternary_folk_qfl()} draws either a static or interactive ternary diagram, in english or spanish. It is a base diagram where data can be plotted.
#' @param output The output format: "ggplot" or "plotly" (default is "ggplot")
#' @param language The language to be displayed: "en" for english or "es" for spanish (deafult is "en")
#' @param opacity Transparency level (default is 0.5)
#'
#' @return Folk's ternary diagram for sandstones in the desired format (object)
#' @export
#' @importFrom ggplot2 .data
#'
#' @details For plotting data on the ggplot object it would be easier if the names of the dataframe are "f", "q", and "l", that way it gets mapped automatically, if not make sure to use "aes(x=f,y=q,z=l)".
#' For plotting on the plotly object the mapping of the new data should be as shown in the example: \code{a = ~q, b = ~f, c = ~l}, where \code{a} refers to the top ("q"), \code{b} refers to the bottom left ("f"), and \code{c} refers to the bottom right ("l").
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
#' d = data.frame(q=c(23,40,60),
#'                f=c(27,50,30),
#'                l=c(50,10,10),
#'                loc=c("A", "B", "C"))
#'
#' # adding data to ggplot object
#' ternary_folk_qfl() + geom_point(aes(col = loc), data = d)
#'
#' # adding data to plotly object
#' ternary_folk_qfl('plotly') %>%
#'   add_trace(a = ~q, b = ~f, c = ~l,
#'             data = d,
#'             name = 'My data',
#'             type = "scatterternary",
#'             mode = "markers",
#'             marker = list(size=8,color='coral',
#'                           symbol=3,opacity=.9),
#'             hovertemplate = paste0('Q: %{a}<br>',
#'                                    'F: %{b}<br>',
#'                                    'L: %{c}'))
#'
#' # adding data to plotly object with custom colors
#' my_pal    <- c("A" = "red",      "B" = "blue",   "C" = "green")
#' my_shapes <- c("A" = "circle",   "B" = "square",  "C" = "diamond")
#'
#' ternary_folk_qfl('plotly') %>%
#'   add_trace(a = ~q, b = ~f, c = ~l,
#'             data = d,
#'             type = "scatterternary",
#'             mode = "markers",
#'             split = ~loc,
#'             marker = list(size=8,
#'                           color  = ~my_pal[loc],
#'                           symbol = ~my_shapes[loc],
#'                           opacity=.9),
#'             hovertemplate = paste0('Q: %{a}<br>',
#'                                    'F: %{b}<br>',
#'                                    'L: %{c}'))
#'
ternary_folk_qfl = function(
  output = c('ggplot', 'plotly'),
  language = c('en', 'es'),
  opacity = .5
) {
  tb.Folk = tibble::tribble(
    ~q  , ~f     , ~l     , ~Label                    , ~Label.es                      ,
    100 ,   0    ,   0    , "Quartzarenite"           , "Quarzo arenita"               ,
     95 ,   5    ,   0    , "Quartzarenite"           , "Quarzo arenita"               ,
     95 ,   2.5  ,   2.5  , "Quartzarenite"           , "Quarzo arenita"               ,
     95 ,   0    ,   5    , "Quartzarenite"           , "Quarzo arenita"               ,
     95 ,   5    ,   0    , "Subarkose"               , "Subarcosa"                    ,
     75 ,  25    ,   0    , "Subarkose"               , "Subarcosa"                    ,
     75 ,  12.5  ,  12.5  , "Subarkose"               , "Subarcosa"                    ,
     95 ,   2.5  ,   2.5  , "Subarkose"               , "Subarcosa"                    ,
     95 ,   2.5  ,   2.5  , "Sublitharenite"          , "Sublitarenita"                ,
     75 ,  12.5  ,  12.5  , "Sublitharenite"          , "Sublitarenita"                ,
     75 ,   0    ,  25    , "Sublitharenite"          , "Sublitarenita"                ,
     95 ,   0    ,   5    , "Sublitharenite"          , "Sublitarenita"                ,
     75 ,  25    ,   0    , "Arkose"                  , "Arcosa"                       ,
      0 , 100    ,   0    , "Arkose"                  , "Arcosa"                       ,
      0 ,  75    ,  25    , "Arkose"                  , "Arcosa"                       ,
     75 ,  18.75 ,   6.25 , "Arkose"                  , "Arcosa"                       ,
     75 ,  18.75 ,   6.25 , "Lithik Arkose"           , "Arcosa l\u00edtica"           ,
      0 ,  75    ,  25    , "Lithik Arkose"           , "Arcosa l\u00edtica"           ,
      0 ,  50    ,  50    , "Lithik Arkose"           , "Arcosa l\u00edtica"           ,
     75 ,  12.5  ,  12.5  , "Lithik Arkose"           , "Arcosa l\u00edtica"           ,
     75 ,  12.5  ,  12.5  , "Feldspathic Litharenite" , "Litarenita feldesp\u00e1tica" ,
      0 ,  50    ,  50    , "Feldspathic Litharenite" , "Litarenita feldesp\u00e1tica" ,
      0 ,  27    ,  75    , "Feldspathic Litharenite" , "Litarenita feldesp\u00e1tica" ,
     75 ,   6.25 ,  18.75 , "Feldspathic Litharenite" , "Litarenita feldesp\u00e1tica" ,
     75 ,   6.25 ,  18.75 , "Litharenite"             , "Litarenita"                   ,
      0 ,  27    ,  75    , "Litharenite"             , "Litarenita"                   ,
      0 ,   0    , 100    , "Litharenite"             , "Litarenita"                   ,
     75 ,   0    ,  25    , "Litharenite"             , "Litarenita"
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
      font = list(family = "serif", size = 15, color = "white"),
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
    caxis = axis("L")
  )

  Folk.pal = c(
    "#777777",
    "#AACDC9",
    "#C0CF98",
    "#2F4996",
    "#D3C6E2",
    "#EBD1C0",
    "#564E37"
  )

  if (any(output == 'ggplot' & language == 'en')) {
    Folk <- ggtern::ggtern(
      data = tb.Folk,
      ggtern::aes(.data$f, .data$q, .data$l)
    ) +
      ggplot2::geom_polygon(
        ggplot2::aes(
          fill = .data$Label,
          # color = .data$Label,
          group = .data$Label
        ),
        alpha = opacity
      ) +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_manual(values = Folk.pal) +
      # ggplot2::scale_color_manual(values = Folk.pal) +
      ggplot2::labs(
        title = "Folk",
        fill = "Sandstone",
        # color = "Sandstone",
        T = "Q",
        L = "F",
        R = "L"
      )
  } else if (any(output == 'ggplot' & language == 'es')) {
    Folk <- ggtern::ggtern(
      data = tb.Folk,
      ggtern::aes(.data$f, .data$q, .data$l)
    ) +
      ggplot2::geom_polygon(
        ggplot2::aes(
          fill = .data$Label.es,
          # color = .data$Label.es,
          group = .data$Label.es
        ),
        alpha = opacity
      ) +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_manual(values = Folk.pal) +
      # ggplot2::scale_color_manual(values = Folk.pal) +
      ggplot2::labs(
        title = "Folk",
        fill = "Arenisca",
        # color = "Arenisca",
        T = "Q",
        L = "F",
        R = "L"
      )
  } else if (any(output == 'plotly' & language == 'en')) {
    Folk = plotly::plot_ly() %>%
      plotly::add_trace(
        data = tb.Folk,
        a = ~q,
        b = ~f,
        c = ~l,
        color = ~Label,
        colors = Folk.pal %>% purrr::set_names(levels(tb.Folk$Label)),
        opacity = opacity * 2,
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        hoveron = 'fills'
      ) %>%
      plotly::layout(
        annotations = label("Folk"),
        ternary = Folk.ternaryAxes,
        legend = list(title = list(text = '<b> Sandstone </b>'))
      ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Folk_qfl',
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
        c = ~l,
        color = ~Label.es,
        colors = Folk.pal %>% purrr::set_names(levels(tb.Folk$Label.es)),
        opacity = opacity * 2,
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        hoveron = 'fills'
      ) %>%
      plotly::layout(
        annotations = label("Folk"),
        ternary = Folk.ternaryAxes,
        legend = list(title = list(text = '<b> Arenisca </b>'))
      ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Folk_qfl',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  }

  return(Folk)
}
