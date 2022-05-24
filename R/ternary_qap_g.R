#' QAP Ternary Diagram for Gabbroic Rocks
#' @description \code{ternary_qap_g()} draws either a static or interactive ternary diagram, in english or spanish. It is a base diagram where data can be plotted.
#' @param output The output format: "ggplot" or "plotly" (default is "ggplot")
#' @param language The language to be displayed: "en" for english or "es" for spanish (deafult is "en")
#' @param opacity Transparency level (default is 0.5)
#'
#' @return QAP ternary diagram for gabbroic rocks in the desired format (object)
#' @export
#'
#' @details For plotting data on the ggplot object it would be easier if the names of the dataframe are "cpx", "p", and "opx", that way it gets mapped automatically, if not make sure to use "aes(x=cpx,y=p,z=opx)".
#' For plotting on the plotly object the mapping of the new data should be as shown in the example: \code{a = ~p, b = ~cpx, c = ~opx}, where \code{a} refers to the top ("p"), \code{b} refers to the bottom left ("cpx"), and \code{c} refers to the bottom right ("opx").
#' The examples show basic usage and how to add data, which can be more customizable.
#'
#' @examples
#' library(ggplot2)
#' library(plotly)
#'
#' d = data.frame(p=c(23,26.9,25.3),
#'                cpx=c(27,23.7,5.1),
#'                opx=c(50,49.4,59.6))
#'
#' # adding data to ggplot object
#' ternary_qap_g() + geom_point(data = d)
#'
#' # adding data to plotly object
#' ternary_qap_g('plotly') %>%
#'   add_trace(a = ~p, b = ~cpx, c = ~opx,
#'             data = d,
#'             name = 'My data',
#'             type = "scatterternary",
#'             mode = "markers",
#'             marker = list(size=8,color='coral',
#'                           symbol=3,opacity=.9),
#'             hovertemplate = paste0('P: %{a}<br>',
#'                                    'Cpx: %{b}<br>',
#'                                    'Opx: %{c}'))
#'
ternary_qap_g = function(output = c('ggplot','plotly'),
                         language = c('en','es'),
                         opacity = 0.5) {

  tb.QAP_G = tibble::tribble(
    ~p, ~cpx, ~opx,              ~Label.en,                ~Label.es,
    100,    0,    0,          "Anorthosite",             "Anortosita",
    90,   10,    0,          "Anorthosite",             "Anortosita",
    90,    0,   10,          "Anorthosite",             "Anortosita",
    90,   10,    0,               "Gabbro",                  "Gabro",
    10,   90,    0,               "Gabbro",                  "Gabro",
    10,   85,    5,               "Gabbro",                  "Gabro",
    90,    5,    5,               "Gabbro",                  "Gabro",
    90,    5,    5, "Orthopyroxene gabbro",   "Gabro ortopirox\u00e9nico",
    10,   85,    5, "Orthopyroxene gabbro",   "Gabro ortopirox\u00e9nico",
    10,   45,   45, "Orthopyroxene gabbro",   "Gabro ortopirox\u00e9nico",
    90,    5,    5, "Clinopyroxene norite", "Norita clinopirox\u00e9nica",
    10,   45,   45, "Clinopyroxene norite", "Norita clinopirox\u00e9nica",
    10,    5,   85, "Clinopyroxene norite", "Norita clinopirox\u00e9nica",
    90,    5,    5,               "Norite",                 "Norita",
    10,    5,   85,               "Norite",                 "Norita",
    10,    0,   90,               "Norite",                 "Norita",
    90,    0,   10,               "Norite",                 "Norita",
    10,   90,    0,      "Clinopyroxenite",        "Clinopiroxenita",
    0,  100,    0,      "Clinopyroxenite",        "Clinopiroxenita",
    0,   90,   10,      "Clinopyroxenite",        "Clinopiroxenita",
    10,   90,    0,           "Websterite",             "Websterita",
    0,   90,   10,           "Websterite",             "Websterita",
    0,   10,   90,           "Websterite",             "Websterita",
    10,    0,   90,           "Websterite",             "Websterita",
    10,    0,   90,      "Orthopyroxenite",         "Ortopiroxenita",
    0,   10,   90,      "Orthopyroxenite",         "Ortopiroxenita",
    0,    0,  100,      "Orthopyroxenite",         "Ortopiroxenita"
  ) %>%
    dplyr::mutate(dplyr::across(Label.en:Label.es,forcats::as_factor))


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

  QAP_G.ternaryAxes <- list(
    aaxis = axis("P"),
    baxis = axis("Cpx"),
    caxis = axis("Opx")
  )

  QAP_G.pal = viridisLite::viridis(8,direction = -1,option = 'G')

  if (any(output == 'ggplot' & language == 'en')) {
    QAP_G <- ggtern::ggtern(data=tb.QAP_G,ggtern::aes(cpx,p,opx)) +
      ggplot2::geom_polygon(aes(fill=Label.en,col=Label.en,
                                group=Label.en),
                            alpha=opacity) +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_manual('Gabbros',values = QAP_G.pal) +
      ggplot2::scale_color_manual('Gabbros',values = QAP_G.pal) +
      ggplot2::labs(T="P",
                    L="Cpx",
                    R="Opx")
  } else if (any(output == 'ggplot' & language == 'es')) {
    QAP_G <- ggtern::ggtern(data=tb.QAP_G,ggtern::aes(cpx,p,opx)) +
      ggplot2::geom_polygon(aes(fill=Label.es,col=Label.es,
                                group=Label.es),
                            alpha=opacity) +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_manual('Gabros',values = QAP_G.pal) +
      ggplot2::scale_color_manual('Gabros',values = QAP_G.pal) +
      ggplot2::labs(T="P",
                    L="Cpx",
                    R="Opx")
  } else if (any(output == 'plotly' & language == 'en')) {
    QAP_G = plotly::plot_ly() %>%
      plotly::add_trace(
        data = tb.QAP_G,
        a = ~p, b = ~cpx, c = ~opx,
        color = ~Label.en,
        colors = QAP_G.pal,
        opacity = opacity*2,
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        line = list(color = "black"),
        hoverinfo = 'text',
        hoveron = 'fills'
      ) %>%
      plotly::layout(
        ternary = QAP_G.ternaryAxes,
        legend = list(title=list(text='<b> Gabros </b>'))
      ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Gabbros',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  } else if (any(output == 'plotly' & language == 'es')) {
    QAP_G = plotly::plot_ly() %>%
      plotly::add_trace(
        data = tb.QAP_G,
        a = ~p, b = ~cpx, c = ~opx,
        color = ~Label.es,
        colors = QAP_G.pal,
        opacity = opacity*2,
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        line = list(color = "black"),
        hoverinfo = 'text',
        hoveron = 'fills'
      ) %>%
      plotly::layout(
        ternary = QAP_G.ternaryAxes,
        legend = list(title=list(text='<b> Gabros </b>'))
      ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Gabros',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  }

  return(QAP_G)

}
