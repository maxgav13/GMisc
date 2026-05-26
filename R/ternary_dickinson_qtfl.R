#' QtFL Ternary Diagram for Provenance Analysis
#' @description \code{ternary_dickinson_qtfl()} draws either a static or interactive ternary diagram, in english or spanish. It is a base diagram where data can be plotted.
#' @param output The output format: "ggplot" or "plotly" (default is "ggplot")
#' @param language The language to be displayed: "en" for english or "es" for spanish (deafult is "en")
#' @param opacity Transparency level (default is 0.5)
#'
#' @return QtFL ternary diagram for provenance analysis in the desired format (object)
#' @export
#' @importFrom ggplot2 .data
#'
#' @details For plotting data on the ggplot object it would be easier if the names of the dataframe are "f", "qt", and "l", that way it gets mapped automatically, if not make sure to use "aes(x=f,y=qt,z=l)".
#' For plotting on the plotly object the mapping of the new data should be as shown in the example: \code{a = ~qt, b = ~f, c = ~l}, where \code{a} refers to the top ("qt"), \code{b} refers to the bottom left ("f"), and \code{c} refers to the bottom right ("l").
#' The examples show basic usage and how to add data, which can be more customizable.
#' For a more complete description on how to use this function and similar ones, see
#' \code{vignette("classification_diagrams_vig")}
#'
#' The QtFL diagram is used in sedimentary petrology to classify sandstones based on their detrital composition, specifically the relative proportions of quartz (Qt), feldspar (F), and lithic fragments (L). The diagram helps to infer the tectonic setting and provenance of the sedimentary rocks, as different source areas and geological processes produce characteristic detrital assemblages.
#'
#' The QtFL diagram was proposed by Dickinson (1985) as a modification of the traditional QFL diagram, which only considers quartz, feldspar, and lithic fragments. The QtFL diagram includes a fourth component, "Qt", which represents the total quartz content (including both monocrystalline and polycrystalline quartz). This allows for a more accurate classification of sandstones, especially those with high quartz content, and provides better insights into the sedimentary processes and source areas.
#'
#' @family classification plots, petrology
#'
#' @examples
#' library(ggplot2)
#' library(plotly)
#'
#' d = data.frame(qt=c(23,40,60),
#'                f=c(27,50,30),
#'                l=c(50,10,10),
#'                loc=c("A", "B", "C"))
#'
#' # adding data to ggplot object
#' ternary_dickinson_qtfl() + geom_point(aes(col = loc), data = d)
#'
#' # adding data to plotly object
#' ternary_dickinson_qtfl('plotly') %>%
#'   add_trace(a = ~qt, b = ~f, c = ~l,
#'             data = d,
#'             name = 'My data',
#'             type = "scatterternary",
#'             mode = "markers",
#'             marker = list(size=8,color='coral',symbol=3,opacity=.9),
#'             hovertemplate = paste0('Qt: %{a}<br>',
#'                                    'F: %{b}<br>',
#'                                    'L: %{c}'))
#'
#' # adding data to plotly object with custom colors
#' my_pal    <- c("A" = "red",      "B" = "blue",   "C" = "green")
#' my_shapes <- c("A" = "circle",   "B" = "square",  "C" = "diamond")
#'
#' ternary_dickinson_qtfl('plotly') %>%
#'   add_trace(a = ~qt, b = ~f, c = ~l,
#'             data = d,
#'             type = "scatterternary",
#'             mode = "markers",
#'             split = ~loc,
#'             marker = list(size=8,
#'                           color  = ~my_pal[loc],
#'                           symbol = ~my_shapes[loc],
#'                           opacity=.9),
#'             hovertemplate = paste0('Qt: %{a}<br>',
#'                                    'F: %{b}<br>',
#'                                    'L: %{c}'))
#'
ternary_dickinson_qtfl = function(
  output = c('ggplot', 'plotly'),
  language = c('en', 'es'),
  opacity = 0.5
) {
  output <- match.arg(output)
  language <- match.arg(language)

  tb.QFL = tibble::tribble(
    ~qt     , ~f      , ~l      , ~Label                     , ~Label.es                     ,
    100     ,   0     ,   0     , "Craton interior"          , "Crat\u00f3n interior"        ,
     82     ,  18     ,   0     , "Craton interior"          , "Crat\u00f3n interior"        ,
     80     ,  15     ,   5     , "Craton interior"          , "Crat\u00f3n interior"        ,
     97     ,   0     ,   3     , "Craton interior"          , "Crat\u00f3n interior"        ,
     82     ,  18     ,   0     , "Transitional Continental" , "Transici\u00f3n continental" ,
     55     ,  45     ,   0     , "Transitional Continental" , "Transici\u00f3n continental" ,
     52     ,  40     ,   8     , "Transitional Continental" , "Transici\u00f3n continental" ,
     80     ,  15     ,   5     , "Transitional Continental" , "Transici\u00f3n continental" ,
     55     ,  45     ,   0     , "Basement Uplift"          , "Basamento elevado"           ,
      0     , 100     ,   0     , "Basement Uplift"          , "Basamento elevado"           ,
      0     ,  85     ,  15     , "Basement Uplift"          , "Basamento elevado"           ,
     20     ,  67.7   ,  12.3   , "Basement Uplift"          , "Basamento elevado"           ,
     52     ,  40     ,   8     , "Basement Uplift"          , "Basamento elevado"           ,
     97     ,   0     ,   3     , "Recycled Orogenic"        , "Or\u00f3geno reciclado"      ,
     80     ,  15     ,   5     , "Recycled Orogenic"        , "Or\u00f3geno reciclado"      ,
     52     ,  40     ,   8     , "Recycled Orogenic"        , "Or\u00f3geno reciclado"      ,
     33.781 ,  12.897 ,  53.322 , "Recycled Orogenic"        , "Or\u00f3geno reciclado"      ,
     25     ,   0     ,  75     , "Recycled Orogenic"        , "Or\u00f3geno reciclado"      ,
     52     ,  40     ,   8     , "Dissected Arc"            , "Arco disectado"              ,
     20     ,  67.7   ,  12.3   , "Dissected Arc"            , "Arco disectado"              ,
     33.781 ,  12.897 ,  53.322 , "Dissected Arc"            , "Arco disectado"              ,
     20     ,  67.7   ,  12.3   , "Transitional Arc"         , "Arco transicional"           ,
      0     ,  85     ,  15     , "Transitional Arc"         , "Arco transicional"           ,
      0     ,  50     ,  50     , "Transitional Arc"         , "Arco transicional"           ,
     25     ,   0     ,  75     , "Transitional Arc"         , "Arco transicional"           ,
     25     ,   0     ,  75     , "Transitional Arc"         , "Arco transicional"           ,
     33.781 ,  12.897 ,  53.322 , "Transitional Arc"         , "Arco transicional"           ,
     25     ,   0     ,  75     , "Undissected Arc"          , "Arco no disectado"           ,
      0     ,  50     ,  50     , "Undissected Arc"          , "Arco no disectado"           ,
      0     ,   0     , 100     , "Undissected Arc"          , "Arco no disectado"
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

  QFL.ternaryAxes <- list(
    aaxis = axis("Qt"),
    baxis = axis("F"),
    caxis = axis("L")
  )

  QFL.pal = c(
    "#989FA7",
    "#5A9AE1",
    "#2F4996",
    "#6B6943",
    "#BFAED2",
    "#DAB7A3",
    "#B21C3F"
  )

  if (output == 'ggplot' & language == 'en') {
    QFL <- ggtern::ggtern(
      data = tb.QFL,
      ggtern::aes(.data$f, .data$qt, .data$l)
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
      ggplot2::theme_bw() +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_manual(values = QFL.pal) +
      # ggplot2::scale_color_manual(values = QFL.pal) +
      ggplot2::labs(
        title = "QtFL",
        fill = "Provenance",
        # color = "Provenance",
        T = "Qt",
        L = "F",
        R = "L"
      )
  } else if (output == 'ggplot' & language == 'es') {
    QFL <- ggtern::ggtern(
      data = tb.QFL,
      ggtern::aes(.data$f, .data$qt, .data$l)
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
      ggplot2::theme_bw() +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_manual(values = QFL.pal) +
      # ggplot2::scale_color_manual(values = QFL.pal) +
      ggplot2::labs(
        title = "QtFL",
        fill = "Proveniencia",
        # color = "Proveniencia",
        T = "Qt",
        L = "F",
        R = "L"
      )
  } else if (output == 'plotly' & language == 'en') {
    QFL = plotly::plot_ly() %>%
      plotly::add_trace(
        data = tb.QFL,
        a = ~qt,
        b = ~f,
        c = ~l,
        color = ~Label,
        colors = QFL.pal %>% purrr::set_names(levels(tb.QFL$Label)),
        opacity = opacity * 2,
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        line = list(color = "black", width = 0.5),
        hoverinfo = 'text',
        hoveron = 'fills'
      ) %>%
      plotly::layout(
        annotations = label("QtFL"),
        ternary = QFL.ternaryAxes,
        legend = list(title = list(text = '<b> Provenance </b>'))
      ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Dickinson_QtFL',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  } else if (output == 'plotly' & language == 'es') {
    QFL = plotly::plot_ly() %>%
      plotly::add_trace(
        data = tb.QFL,
        a = ~qt,
        b = ~f,
        c = ~l,
        color = ~Label.es,
        colors = QFL.pal %>% purrr::set_names(levels(tb.QFL$Label.es)),
        opacity = opacity * 2,
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        line = list(color = "black", width = 0.5),
        hoverinfo = 'text',
        hoveron = 'fills'
      ) %>%
      plotly::layout(
        annotations = label("QtFL"),
        ternary = QFL.ternaryAxes,
        legend = list(title = list(text = '<b> Proveniencia </b>'))
      ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Dickinson_QtFL',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  }

  return(QFL)
}
