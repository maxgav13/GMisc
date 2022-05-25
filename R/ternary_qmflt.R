#' QmFLt Ternary Diagram for Provenance Analysis
#' @description \code{ternary_qmflt()} draws either a static or interactive ternary diagram, in english or spanish. It is a base diagram where data can be plotted.
#' @param output The output format: "ggplot" or "plotly" (default is "ggplot")
#' @param language The language to be displayed: "en" for english or "es" for spanish (deafult is "en")
#' @param opacity Transparency level (default is 0.5)
#'
#' @return QmFLt ternary diagram for provenance analysis in the desired format (object)
#' @export
#' @importFrom ggplot2 .data
#'
#' @details For plotting data on the ggplot object it would be easier if the names of the dataframe are "f", "qm", and "lt", that way it gets mapped automatically, if not make sure to use "aes(x=f,y=qm,z=lt)".
#' For plotting on the plotly object the mapping of the new data should be as shown in the example: \code{a = ~qm, b = ~f, c = ~lt}, where \code{a} refers to the top ("qm"), \code{b} refers to the bottom left ("f"), and \code{c} refers to the bottom right ("lt").
#' The examples show basic usage and how to add data, which can be more customizable.
#'
#' @examples
#' library(ggplot2)
#' library(plotly)
#'
#' d = data.frame(qm=c(23,26.9,25.3),
#'                f=c(27,23.7,5.1),
#'                lt=c(50,49.4,59.6))
#'
#' # adding data to ggplot object
#' ternary_qmflt() + geom_point(data = d)
#'
#' # adding data to plotly object
#' ternary_qmflt('plotly') %>%
#'   add_trace(a = ~qm, b = ~f, c = ~lt,
#'             data = d,
#'             name = 'My data',
#'             type = "scatterternary",
#'             mode = "markers",
#'             marker = list(size=8,color='coral',
#'                           symbol=3,opacity=.9),
#'             hovertemplate = paste0('Qm: %{a}<br>',
#'                                    'F: %{b}<br>',
#'                                    'Lt: %{c}'))
#'
ternary_qmflt = function(output = c('ggplot','plotly'),
                         language = c('en','es'),
                         opacity = 0.5) {

  tb.QmFLt = tibble::tribble(
    ~qm,     ~f,    ~lt,                     ~Label,                ~Label.es,
    100,      0,      0,          "Craton interior",        "Crat\u00f3n interior",
    80,     20,      0,          "Craton interior",        "Crat\u00f3n interior",
    73.3,   13.8,   12.9,          "Craton interior",        "Crat\u00f3n interior",
    89,      0,     11,          "Craton interior",        "Crat\u00f3n interior",
    80,     20,      0, "Transitional Continental", "Transici\u00f3n continental",
    57,     43,      0, "Transitional Continental", "Transici\u00f3n continental",
    49.341, 34.377, 16.283, "Transitional Continental", "Transici\u00f3n continental",
    68.151, 18.133, 13.717, "Transitional Continental", "Transici\u00f3n continental",
    73.3,   13.8,   12.9, "Transitional Continental", "Transici\u00f3n continental",
    57,     43,      0,          "Basement Uplift",      "Basemento elevado",
    0,    100,      0,          "Basement Uplift",      "Basemento elevado",
    0,     77,     23,          "Basement Uplift",      "Basemento elevado",
    22.199,  57.56, 20.242,          "Basement Uplift",      "Basemento elevado",
    49.341, 34.377, 16.283,          "Basement Uplift",      "Basemento elevado",
    89,      0,     11,       "Quartzose Recycled",     "Reciclaje cuarzoso",
    73.3,   13.8,   12.9,       "Quartzose Recycled",     "Reciclaje cuarzoso",
    68.151, 18.133, 13.717,       "Quartzose Recycled",     "Reciclaje cuarzoso",
    50.273, 16.786, 32.941,       "Quartzose Recycled",     "Reciclaje cuarzoso",
    58,      0,     42,       "Quartzose Recycled",     "Reciclaje cuarzoso",
    58,      0,     42,    "Transitional Recycled",  "Reciclaje transiconal",
    50.273, 16.786, 32.941,    "Transitional Recycled",  "Reciclaje transiconal",
    29.66, 15.191, 55.148,    "Transitional Recycled",  "Reciclaje transiconal",
    21.639, 14.544, 63.817,    "Transitional Recycled",  "Reciclaje transiconal",
    29,      0,     71,    "Transitional Recycled",  "Reciclaje transiconal",
    29,      0,     71,          "Lithic Recycled",       "Reciclaje l\u00edtico",
    21.639, 14.544, 63.817,          "Lithic Recycled",       "Reciclaje l\u00edtico",
    12.685, 13.932, 73.383,          "Lithic Recycled",       "Reciclaje l\u00edtico",
    0,     13,     87,          "Lithic Recycled",       "Reciclaje l\u00edtico",
    0,      0,    100,          "Lithic Recycled",       "Reciclaje l\u00edtico",
    68.151, 18.133, 13.717,                    "Mixed",                 "Mezcla",
    49.341, 34.377, 16.283,                    "Mixed",                 "Mezcla",
    29.66, 15.191, 55.148,                    "Mixed",                 "Mezcla",
    50.273, 16.786, 32.941,                    "Mixed",                 "Mezcla",
    49.341, 34.377, 16.283,            "Dissected Arc",         "Arco disectado",
    22.199,  57.56, 20.242,            "Dissected Arc",         "Arco disectado",
    29.66, 15.191, 55.148,            "Dissected Arc",         "Arco disectado",
    22.199,  57.56, 20.242,         "Transitional Arc",      "Arco transicional",
    0,     77,     23,         "Transitional Arc",      "Arco transicional",
    0,     47,     53,         "Transitional Arc",      "Arco transicional",
    12.685, 13.932, 73.383,         "Transitional Arc",      "Arco transicional",
    21.639, 14.544, 63.817,         "Transitional Arc",      "Arco transicional",
    29.66, 15.191, 55.148,         "Transitional Arc",      "Arco transicional",
    0,     47,     53,          "Undissected Arc",      "Arco no disectado",
    0,     13,     87,          "Undissected Arc",      "Arco no disectado",
    12.685, 13.932, 73.383,          "Undissected Arc",      "Arco no disectado"
  ) %>%
    dplyr::mutate(dplyr::across(.data$Label:.data$Label.es,
                                forcats::as_factor))

  # reusable function for creating annotation object
  label <- function(txt) {
    list(
      text = txt,
      x = 0.1, y = 1,
      ax = 0, ay = 0,
      xref = "paper", yref = "paper",
      align = "center",
      font = list(family = "serif", size = 15, color = "white"),
      bgcolor = "#b3b3b3", bordercolor = "black", borderwidth = 2
    )
  }

  # reusable function for axis formatting
  axis <- function(txt) {
    list(
      title = txt, tickformat = ".0%", tickfont = list(size = 10)
    )
  }

  QmFLt.ternaryAxes <- list(
    aaxis = axis("Qm"),
    baxis = axis("F"),
    caxis = axis("Lt")
  )

  QmFLt.pal = c("#989FA7", "#5A9AE1", "#2F4996",
                "#9EA76E", "#6B6943", "#564E37",
                "#D4E3D0",
                "#BFAED2", "#DAB7A3", "#B21C3F")

  if (any(output == 'ggplot' & language == 'en')) {
    QmFLt <- ggtern::ggtern(data=tb.QmFLt,
                            ggtern::aes(.data$f,.data$qm,.data$lt)) +
      ggplot2::geom_polygon(aes(fill=.data$Label,
                                color=.data$Label,
                                group=.data$Label),
                            alpha=opacity) +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_manual(values = QmFLt.pal) +
      ggplot2::scale_color_manual(values = QmFLt.pal) +
      ggplot2::labs(title="QmFLt",
                    fill = "Provenance",
                    color = "Provenance",
                    T="Qm",
                    L="F",
                    R="Lt")
  } else if (any(output == 'ggplot' & language == 'es')) {
    QmFLt <- ggtern::ggtern(data=tb.QmFLt,
                            ggtern::aes(.data$f,.data$qm,.data$lt)) +
      ggplot2::geom_polygon(aes(fill=.data$Label.es,
                                color=.data$Label.es,
                                group=.data$Label.es),
                            alpha=opacity) +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_manual(values = QmFLt.pal) +
      ggplot2::scale_color_manual(values = QmFLt.pal) +
      ggplot2::labs(title="QmFLt",
                    fill = "Proveniencia",
                    color = "Proveniencia",
                    T="Qm",
                    L="F",
                    R="Lt")
  } else if (any(output == 'plotly' & language == 'en')) {
    QmFLt = plotly::plot_ly() %>%
      plotly::add_trace(
        data = tb.QmFLt,
        a = ~qm, b = ~f, c = ~lt,
        color = ~Label,
        colors = QmFLt.pal %>% purrr::set_names(levels(tb.QmFLt$Label)),
        opacity = opacity*2,
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        hoveron = 'fills'
      ) %>%
      plotly::layout(
        annotations = label("QmFLt"), ternary = QmFLt.ternaryAxes,
        legend = list(title=list(text='<b> Provenance </b>'))
      ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'QmFLt',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  } else if (any(output == 'plotly' & language == 'es')) {
    QmFLt = plotly::plot_ly() %>%
      plotly::add_trace(
        data = tb.QmFLt,
        a = ~qm, b = ~f, c = ~lt,
        color = ~Label.es,
        colors = QmFLt.pal %>% purrr::set_names(levels(tb.QmFLt$Label.es)),
        opacity = opacity*2,
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        hoveron = 'fills'
      ) %>%
      plotly::layout(
        annotations = label("QmFLt"), ternary = QmFLt.ternaryAxes,
        legend = list(title=list(text='<b> Proveniencia </b>'))
      ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'QmFLt',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  }

  return(QmFLt)

}
