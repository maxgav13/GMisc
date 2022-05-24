#' QAP Ternary Diagram for Mafic Rocks
#' @description \code{ternary_qap_m()} draws either a static or interactive ternary diagram, in english or spanish. It is a base diagram where data can be plotted.
#' @param output The output format: "ggplot" or "plotly" (default is "ggplot")
#' @param language The language to be displayed: "en" for english or "es" for spanish (deafult is "en")
#' @param opacity Transparency level (default is 0.5)
#'
#' @return QAP ternary diagram for mafic rocks in the desired format (object)
#' @export
#'
#' @details For plotting data on the ggplot object it would be easier if the names of the dataframe are "ol", "p", and "px", that way it gets mapped automatically, if not make sure to use "aes(x=ol,y=p,z=px)".
#' For plotting on the plotly object the mapping of the new data should be as shown in the example: \code{a = ~p, b = ~ol, c = ~px}, where \code{a} refers to the top ("p"), \code{b} refers to the bottom left ("ol"), and \code{c} refers to the bottom right ("px").
#' The examples show basic usage and how to add data, which can be more customizable.
#'
#' @examples
#' library(ggplot2)
#' library(plotly)
#'
#' d = data.frame(p=c(23,26.9,25.3),
#'                ol=c(27,23.7,5.1),
#'                px=c(50,49.4,59.6))
#'
#' # adding data to ggplot object
#' ternary_qap_m() + geom_point(data = d)
#'
#' # adding data to plotly object
#' ternary_qap_m('plotly') %>%
#'   add_trace(a = ~p, b = ~ol, c = ~px,
#'             data = d,
#'             name = 'My data',
#'             type = "scatterternary",
#'             mode = "markers",
#'             marker = list(size=8,color='cyan',
#'                           symbol=3,opacity=.9),
#'             hovertemplate = paste0('P: %{a}<br>',
#'                                    'Ol: %{b}<br>',
#'                                    'Cpx: %{c}'))
#'
ternary_qap_m = function(output = c('ggplot','plotly'),
                         language = c('en','es'),
                         opacity = 0.5) {


  tb.QAP_M = tibble::tribble(
    ~p, ~ol, ~px, ~Label.en, ~Label.es,
    100,0,0,"Anorthosite","Anortosita",
    90,10,0,"Anorthosite","Anortosita",
    90,0,10,"Anorthosite","Anortosita",
    90,10,0,"Troctolite","Troctolita",
    10,90,0,"Troctolite","Troctolita",
    5,90,5,"Troctolite","Troctolita",
    90,5,5,"Troctolite","Troctolita",
    90,5,5,"Olivine gabbro /
        Olivine norite","Gabro oliv\u00ednoco /
        Norita oliv\u00ednica",
    5,90,5,"Olivine gabbro /
        Olivine norite","Gabro oliv\u00ednoco /
        Norita oliv\u00ednica",
    5,5,90,"Olivine gabbro /
        Olivine norite","Gabro oliv\u00ednoco /
        Norita oliv\u00ednica",
    90,0,10,"Gabbro / Norite","Gabro / Norita",
    90,5,5,"Gabbro / Norite","Gabro / Norita",
    5,5,90,"Gabbro / Norite","Gabro / Norita",
    10,0,90,"Gabbro / Norite","Gabro / Norita",
    10,90,0,"Dunite","Dunita",
    0,100,0,"Dunite","Dunita",
    0,90,10,"Dunite","Dunita",
    5,90,5,"Wehrlite /
        Harzburgite","Wehrlita /
        Harzburgita",
    0,90,10,"Wehrlite /
        Harzburgite","Wehrlita /
        Harzburgita",
    0,42.5,57.5,"Wehrlite /
        Harzburgite","Wehrlita /
        Harzburgita",
    5,40,55,"Wehrlite /
        Harzburgite","Wehrlita /
        Harzburgita",
    5,40,55,"Olivine clino/orto-pyroxenite","Clino/Orto-piroxenita oliv\u00ednica",
    0,42.5,57.5,"Olivine clino/orto-pyroxenite","Clino/Orto-piroxenita oliv\u00ednica",
    0,10,90,"Olivine clino/orto-pyroxenite","Clino/Orto-piroxenita oliv\u00ednica",
    5,5,90,"Olivine clino/orto-pyroxenite","Clino/Orto-piroxenita oliv\u00ednica",
    10,0,90,"Clinopyroxenite /
        Orthopyroxenite","Clinopiroxenita /
        Ortopiroxenita",
    0,10,90,"Clinopyroxenite /
        Orthopyroxenite","Clinopiroxenita /
        Ortopiroxenita",
    0,0,100,"Clinopyroxenite /
        Orthopyroxenite","Clinopiroxenita /
        Ortopiroxenita") %>%
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

  QAP_M.ternaryAxes <- list(
    aaxis = axis("P"),
    baxis = axis("Ol"),
    caxis = axis("Px")
  )

  QAP_M.pal = viridisLite::viridis(8,direction = -1,option = 'F')

  if (any(output == 'ggplot' & language == 'en')) {
    QAP_M <- ggtern::ggtern(data=tb.QAP_M,ggtern::aes(ol,p,px)) +
      ggplot2::geom_polygon(aes(fill=Label.en,color=Label.en,
                                group=Label.en),
                            alpha=opacity) +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      # custom_percent("Percent") +
      ggplot2::scale_fill_manual('Mafic',values = QAP_M.pal) +
      ggplot2::scale_color_manual('Mafic',values = QAP_M.pal) +
      ggplot2::labs(T="P",
                    L="Ol",
                    R="Px")
  } else if (any(output == 'ggplot' & language == 'es')) {
    QAP_M <- ggtern::ggtern(data=tb.QAP_M,ggtern::aes(ol,p,px)) +
      ggplot2::geom_polygon(aes(fill=Label.es,color=Label.es,
                                group=Label.es),
                            alpha=opacity) +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      # custom_percent("Percent") +
      ggplot2::scale_fill_manual('M\u00e1ficas',values = QAP_M.pal) +
      ggplot2::scale_color_manual('M\u00e1ficas',values = QAP_M.pal) +
      ggplot2::labs(T="P",
                    L="Ol",
                    R="Px")
  } else if (any(output == 'plotly' & language == 'en')) {
    QAP_M = plotly::plot_ly() %>%
      plotly::add_trace(
        data = tb.QAP_M,
        a = ~p, b = ~ol, c = ~px,
        color = ~Label.en,
        colors = QAP_M.pal,
        opacity = opacity*2,
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        line = list(color = "black"),
        hoverinfo = 'text',
        hoveron = 'fills'
      ) %>%
      plotly::layout(
        ternary = QAP_M.ternaryAxes,
        legend = list(title=list(text='<b> Mafic </b>'))
      ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Mafic',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  } else if (any(output == 'plotly' & language == 'es')) {
    QAP_M = plotly::plot_ly() %>%
      plotly::add_trace(
        data = tb.QAP_M,
        a = ~p, b = ~ol, c = ~px,
        color = ~Label.es,
        colors = QAP_M.pal,
        opacity = opacity*2,
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        line = list(color = "black"),
        hoverinfo = 'text',
        hoveron = 'fills'
      ) %>%
      plotly::layout(
        ternary = QAP_M.ternaryAxes,
        legend = list(title=list(text='<b> M\u00e1ficas </b>'))
      ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Maficas',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  }

  return(QAP_M)

}
