#' QAP Ternary Diagram for Ultramafic Rocks
#' @description \code{ternary_qap_um()} draws either a static or interactive ternary diagram, in english or spanish. It is a base diagram where data can be plotted.
#' @param output The output format: "ggplot" or "plotly" (default is "ggplot")
#' @param language The language to be displayed: "en" for english or "es" for spanish (deafult is "en")
#' @param opacity Transparency level (default is 0.5)
#'
#' @return QAP ternary diagram for ultramafic rocks in the desired format (object)
#' @export
#' @importFrom ggplot2 .data
#'
#' @details For plotting data on the ggplot object it would be easier if the names of the dataframe are "opx", "ol", and "cpx", that way it gets mapped automatically, if not make sure to use "aes(x=opx,y=ol,z=cpx)".
#' For plotting on the plotly object the mapping of the new data should be as shown in the example: \code{a = ~ol, b = ~opx, c = ~cpx}, where \code{a} refers to the top ("ol"), \code{b} refers to the bottom left ("opx"), and \code{c} refers to the bottom right ("cpx").
#' The examples show basic usage and how to add data, which can be more customizable.
#'
#' @examples
#' library(ggplot2)
#' library(plotly)
#'
#' d = data.frame(ol=c(23,26.9,25.3),
#'                opx=c(27,23.7,5.1),
#'                cpx=c(50,49.4,59.6))
#'
#' # adding data to ggplot object
#' ternary_qap_um() + geom_point(data = d)
#'
#' # adding data to plotly object
#' ternary_qap_um('plotly') %>%
#'   add_trace(a = ~ol, b = ~opx, c = ~cpx,
#'             data = d,
#'             name = 'My data',
#'             type = "scatterternary",
#'             mode = "markers",
#'             marker = list(size=8,color='coral',
#'                           symbol=3,opacity=.9),
#'             hovertemplate = paste0('Ol: %{a}<br>',
#'                                    'Opx: %{b}<br>',
#'                                    'Cpx: %{c}'))
#'
ternary_qap_um = function(output = c('ggplot','plotly'),
                          language = c('en','es'),
                          opacity = 0.5) {


  tb.QAP_UM = tibble::tribble(
    ~ol, ~opx, ~cpx,                 ~Label.en,                   ~Label.es,
    100,    0,    0,                  "Dunite",                    "Dunita",
    90,   10,    0,                  "Dunite",                    "Dunita",
    90,    0,   10,                  "Dunite",                    "Dunita",
    90,   10,    0,             "Harzburgite",               "Harzburgita",
    40,   60,    0,             "Harzburgite",               "Harzburgita",
    40,   55,    5,             "Harzburgite",               "Harzburgita",
    90,    5,    5,             "Harzburgite",               "Harzburgita",
    90,    5,    5,              "Lherzolite",                "Lherzolita",
    40,   55,    5,              "Lherzolite",                "Lherzolita",
    40,    5,   55,              "Lherzolite",                "Lherzolita",
    90,    5,    5,                "Wehrlite",                  "Wehrlita",
    40,    5,   55,                "Wehrlite",                  "Wehrlita",
    40,    0,   60,                "Wehrlite",                  "Wehrlita",
    90,    0,   10,                "Wehrlite",                  "Wehrlita",
    40,   60,    0, "Olivine orthopyroxenite",  "Ortopiroxenita oliv\u00ednica",
    10,   90,    0, "Olivine orthopyroxenite",  "Ortopiroxenita oliv\u00ednica",
    5,   90,    5, "Olivine orthopyroxenite",  "Ortopiroxenita oliv\u00ednica",
    40,   55,    5, "Olivine orthopyroxenite",  "Ortopiroxenita oliv\u00ednica",
    40,   55,    5,      "Olivine websterite",      "Websterita oliv\u00ednica",
    5,   90,    5,      "Olivine websterite",      "Websterita oliv\u00ednica",
    5,    5,   90,      "Olivine websterite",      "Websterita oliv\u00ednica",
    40,    5,   55,      "Olivine websterite",      "Websterita oliv\u00ednica",
    40,    5,   55, "Olivine clinopyroxenite", "Clinopiroxenita oliv\u00ednica",
    5,    5,   90, "Olivine clinopyroxenite", "Clinopiroxenita oliv\u00ednica",
    10,    0,   90, "Olivine clinopyroxenite", "Clinopiroxenita oliv\u00ednica",
    40,    0,   60, "Olivine clinopyroxenite", "Clinopiroxenita oliv\u00ednica",
    10,   90,    0,         "Orthopyroxenite",            "Ortopiroxenita",
    0,  100,    0,         "Orthopyroxenite",            "Ortopiroxenita",
    0,   90,   10,         "Orthopyroxenite",            "Ortopiroxenita",
    5,   90,    5,              "Websterite",                "Websterita",
    0,   90,   10,              "Websterite",                "Websterita",
    0,   10,   90,              "Websterite",                "Websterita",
    5,    5,   90,              "Websterite",                "Websterita",
    10,    0,   90,         "Clinopyroxenite",           "Clinopiroxenita",
    0,   10,   90,         "Clinopyroxenite",           "Clinopiroxenita",
    0,    0,  100,         "Clinopyroxenite",           "Clinopiroxenita"
  ) %>%
    dplyr::mutate(dplyr::across(.data$Label.en:.data$Label.es,
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

  QAP_UM.ternaryAxes <- list(
    aaxis = axis("Ol"),
    baxis = axis("Opx"),
    caxis = axis("Cpx")
  )

  QAP_UM.pal = viridisLite::viridis(10,direction = -1,option = 'D')

  if (any(output == 'ggplot' & language == 'en')) {
    QAP_UM <- ggtern::ggtern(data=tb.QAP_UM,
                             ggtern::aes(.data$opx,.data$ol,.data$cpx)) +
      ggplot2::geom_polygon(aes(fill=.data$Label.en,
                                color=.data$Label.en,
                                group=.data$Label.en),
                            alpha=opacity) +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_manual('Ultramafic',values = QAP_UM.pal) +
      ggplot2::scale_color_manual('Ultramafic',values = QAP_UM.pal) +
      ggplot2::labs(T="Ol",
                    L="Opx",
                    R="Cpx")
  } else if (any(output == 'ggplot' & language == 'es')) {
    QAP_UM <- ggtern::ggtern(data=tb.QAP_UM,
                             ggtern::aes(.data$opx,.data$ol,.data$cpx)) +
      ggplot2::geom_polygon(aes(fill=.data$Label.es,
                                color=.data$Label.es,
                                group=.data$Label.es),
                            alpha=opacity) +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_manual("Ultram\u00e1ficas",values = QAP_UM.pal) +
      ggplot2::scale_color_manual("Ultram\u00e1ficas",values = QAP_UM.pal) +
      ggplot2::labs(T="Ol",
                    L="Opx",
                    R="Cpx")
  } else if (any(output == 'plotly' & language == 'en')) {
    QAP_UM = plotly::plot_ly() %>%
      plotly::add_trace(
        data = tb.QAP_UM,
        a = ~ol, b = ~opx, c = ~cpx,
        color = ~Label.en,
        colors = QAP_UM.pal,
        opacity = opacity*2,
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        line = list(color = "black"),
        hoverinfo = 'text',
        hoveron = 'fills'
      ) %>%
      plotly::layout(
        ternary = QAP_UM.ternaryAxes,
        legend = list(title=list(text='<b> Ultramafic </b>'))
      ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Ultramafic',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  } else if (any(output == 'plotly' & language == 'es')) {
    QAP_UM = plotly::plot_ly() %>%
      plotly::add_trace(
        data = tb.QAP_UM,
        a = ~ol, b = ~opx, c = ~cpx,
        color = ~Label.es,
        colors = QAP_UM.pal,
        opacity = opacity*2,
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        line = list(color = "black"),
        hoverinfo = 'text',
        hoveron = 'fills'
      ) %>%
      plotly::layout(
        ternary = QAP_UM.ternaryAxes,
        legend = list(title=list(text='<b> Ultram\u00e1ficas </b>'))
      ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Ultramaficas',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  }

  return(QAP_UM)

}
