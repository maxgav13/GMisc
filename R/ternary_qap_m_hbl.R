#' QAP Ternary Diagram for Mafic Rocks (horneblende)
#' @description \code{ternary_qap_m_hbl()} draws either a static or interactive ternary diagram, in english or spanish. It is a base diagram where data can be plotted.
#' @param output The output format: "ggplot" or "plotly" (default is "ggplot")
#' @param language The language to be displayed: "en" for english or "es" for spanish (deafult is "en")
#' @param opacity Transparency level (default is 0.5)
#'
#' @return QAP ternary diagram for mafic rocks in the desired format (object)
#' @export
#' @importFrom ggplot2 .data
#'
#' @details For plotting data on the ggplot object it would be easier if the names of the dataframe are "px", "p", and "hbl", that way it gets mapped automatically, if not make sure to use "aes(x=px,y=p,z=hbl)".
#' For plotting on the plotly object the mapping of the new data should be as shown in the example: \code{a = ~p, b = ~px, c = ~hbl}, where \code{a} refers to the top ("p"), \code{b} refers to the bottom left ("px"), and \code{c} refers to the bottom right ("hbl").
#' The examples show basic usage and how to add data, which can be more customizable.
#'
#' @family classification plots, petrology
#'
#' @examples
#' library(ggplot2)
#' library(plotly)
#'
#' d = data.frame(p=c(23,40,60),
#'                hbl=c(27,50,30),
#'                px=c(50,10,10))
#'
#' # adding data to ggplot object
#' ternary_qap_m_hbl() + geom_point(data = d)
#'
#' # adding data to plotly object
#' ternary_qap_m_hbl('plotly') %>%
#'   add_trace(a = ~p, b = ~px, c = ~hbl,
#'             data = d,
#'             name = 'My data',
#'             type = "scatterternary",
#'             mode = "markers",
#'             marker = list(size=8,color='cyan',
#'                           symbol=3,opacity=.9),
#'             hovertemplate = paste0('P: %{a}<br>',
#'                                    'Px: %{b}<br>',
#'                                    'Hbl: %{c}'))
#'
ternary_qap_m_hbl = function(
  output = c('ggplot', 'plotly'),
  language = c('en', 'es'),
  opacity = 0.5
) {
  tb.QAP_M = tibble::tribble(
    ~p                                               , ~px , ~hbl , ~Label.en                                   , ~Label.es                      ,
                                                 100 ,   0 ,    0 , "Anorthosite"                               , "Anortosita"                   ,
                                                  90 ,  10 ,    0 , "Anorthosite"                               , "Anortosita"                   ,
                                                  90 ,   0 ,   10 , "Anorthosite"                               , "Anortosita"                   ,
                                                 100 ,   0 ,    0 , "Anorthosite"                               , "Anortosita"                   ,
                                                  90 ,  10 ,    0 , "Gabbro/Norite"                             , "Gabro/Norita"                 ,
                                                  10 ,  90 ,    0 , "Gabbro/Norite"                             , "Gabro/Norita"                 ,
                                                  10 ,  85 ,    5 , "Gabbro/Norite"                             , "Gabro/Norita"                 ,
                                                  90 ,   5 ,    5 , "Gabbro/Norite"                             , "Gabro/Norita"                 ,
                                                  90 ,  10 ,    0 , "Gabbro/Norite"                             , "Gabro/Norita"                 ,
                                                  90 ,   5 ,    5 , "Pyroxene hornblende gabbro/norite"         ,
    "Gabro/Norita pyrox\u00e9nica hornbl\u00e9ndica" ,
                                                  10 ,  85 ,    5 , "Pyroxene hornblende gabbro/norite"         ,
    "Gabro/Norita pyrox\u00e9nica hornbl\u00e9ndica" ,
                                                  10 ,   5 ,   85 , "Pyroxene hornblende gabbro/norite"         ,
    "Gabro/Norita pyrox\u00e9nica hornbl\u00e9ndica" ,
                                                  90 ,   5 ,    5 , "Pyroxene hornblende gabbro/norite"         ,
    "Gabro/Norita pyrox\u00e9nica hornbl\u00e9ndica" ,
                                                  90 ,   0 ,   10 , "Hornblende gabbro"                         , "Gabro honrbl\u00e9ndico"      ,
                                                  90 ,   5 ,    5 , "Hornblende gabbro"                         , "Gabro honrbl\u00e9ndico"      ,
                                                  10 ,   5 ,   85 , "Hornblende gabbro"                         , "Gabro honrbl\u00e9ndico"      ,
                                                  10 ,   0 ,   90 , "Hornblende gabbro"                         , "Gabro honrbl\u00e9ndico"      ,
                                                  90 ,   0 ,   10 , "Hornblende gabbro"                         , "Gabro honrbl\u00e9ndico"      ,
                                                   0 , 100 ,    0 , "Plagioclase-bearing pyroxenite"            , "Piroxenita con plagioclasa"   ,
                                                  10 ,  90 ,    0 , "Plagioclase-bearing pyroxenite"            , "Piroxenita con plagioclasa"   ,
                                                   0 ,  90 ,   10 , "Plagioclase-bearing pyroxenite"            , "Piroxenita con plagioclasa"   ,
                                                   0 , 100 ,    0 , "Plagioclase-bearing pyroxenite"            , "Piroxenita con plagioclasa"   ,
                                                  10 ,  90 ,    0 , "Plagioclase-bearing hornblende pyroxenite" ,
    "Piroxenita hornbl\u00e9ndica con plagioclasa"   ,
                                                  10 ,  45 ,   45 , "Plagioclase-bearing hornblende pyroxenite" ,
    "Piroxenita hornbl\u00e9ndica con plagioclasa"   ,
                                                   0 ,  50 ,   50 , "Plagioclase-bearing hornblende pyroxenite" ,
    "Piroxenita hornbl\u00e9ndica con plagioclasa"   ,
                                                   0 ,  90 ,   10 , "Plagioclase-bearing hornblende pyroxenite" ,
    "Piroxenita hornbl\u00e9ndica con plagioclasa"   ,
                                                  10 ,  90 ,    0 , "Plagioclase-bearing hornblende pyroxenite" ,
    "Piroxenita hornbl\u00e9ndica con plagioclasa"   ,
                                                  10 ,  45 ,   45 , "Plagioclase-bearing pyroxene hornblendite" ,
    "Hornblendita pirox\u00e9nica con plagioclasa"   ,
                                                  10 ,   0 ,   90 , "Plagioclase-bearing pyroxene hornblendite" ,
    "Hornblendita pirox\u00e9nica con plagioclasa"   ,
                                                   0 ,  10 ,   90 , "Plagioclase-bearing pyroxene hornblendite" ,
    "Hornblendita pirox\u00e9nica con plagioclasa"   ,
                                                   0 ,  50 ,   50 , "Plagioclase-bearing pyroxene hornblendite" ,
    "Hornblendita pirox\u00e9nica con plagioclasa"   ,
                                                  10 ,  45 ,   45 , "Plagioclase-bearing pyroxene hornblendite" ,
    "Hornblendita pirox\u00e9nica con plagioclasa"   ,
                                                  10 ,   0 ,   90 , "Plagioclase-bearing hornblendite"          , "Hornblendita con plagioclasa" ,
                                                   0 ,   0 ,  100 , "Plagioclase-bearing hornblendite"          , "Hornblendita con plagioclasa" ,
                                                   0 ,  10 ,   90 , "Plagioclase-bearing hornblendite"          , "Hornblendita con plagioclasa" ,
                                                  10 ,   0 ,   90 , "Plagioclase-bearing hornblendite"          , "Hornblendita con plagioclasa"
  ) %>%
    dplyr::mutate(dplyr::across(
      .data$Label.en:.data$Label.es,
      forcats::as_factor
    ))

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

  QAP_M.ternaryAxes <- list(
    aaxis = axis("P"),
    baxis = axis("Px"),
    caxis = axis("Hbl")
  )

  QAP_M.pal = c(
    "#FAEBDDFF",
    "#F6BF9DFF",
    "#F59067FF",
    "#F05B42FF",
    "#D82848FF",
    "#AE1759FF",
    "#7B1F59FF",
    "#4C1D4BFF"
  )

  if (any(output == 'ggplot' & language == 'en')) {
    QAP_M <- ggtern::ggtern(
      data = tb.QAP_M,
      ggtern::aes(.data$px, .data$p, .data$hbl)
    ) +
      ggplot2::geom_polygon(
        ggplot2::aes(
          fill = .data$Label.en,
          color = .data$Label.en,
          group = .data$Label.en
        ),
        alpha = opacity
      ) +
      ggtern::theme_bw() +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      # custom_percent("Percent") +
      ggplot2::scale_fill_manual(values = QAP_M.pal) +
      ggplot2::scale_color_manual(values = QAP_M.pal) +
      ggplot2::labs(
        fill = "Mafic",
        color = "Mafic",
        T = "P",
        L = "Px",
        R = "Hbl"
      )
  } else if (any(output == 'ggplot' & language == 'es')) {
    QAP_M <- ggtern::ggtern(
      data = tb.QAP_M,
      ggtern::aes(.data$px, .data$p, .data$hbl)
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
      # custom_percent("Percent") +
      ggplot2::scale_fill_manual(values = QAP_M.pal) +
      ggplot2::scale_color_manual(values = QAP_M.pal) +
      ggplot2::labs(
        fill = "M\u00e1ficas",
        color = "M\u00e1ficas",
        T = "P",
        L = "Px",
        R = "Hbl"
      )
  } else if (any(output == 'plotly' & language == 'en')) {
    QAP_M = plotly::plot_ly() %>%
      plotly::add_trace(
        data = tb.QAP_M,
        a = ~p,
        b = ~px,
        c = ~hbl,
        color = ~Label.en,
        colors = QAP_M.pal,
        opacity = opacity * 2,
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        hoverinfo = 'text',
        hoveron = 'fills'
      ) %>%
      plotly::layout(
        ternary = QAP_M.ternaryAxes,
        legend = list(title = list(text = '<b> Mafic </b>')),
        margin = list(autoexpand = T, t = 35)
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
        a = ~p,
        b = ~px,
        c = ~hbl,
        color = ~Label.es,
        colors = QAP_M.pal,
        opacity = opacity * 2,
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        hoverinfo = 'text',
        hoveron = 'fills'
      ) %>%
      plotly::layout(
        ternary = QAP_M.ternaryAxes,
        legend = list(title = list(text = '<b> M\u00e1ficas </b>')),
        margin = list(autoexpand = T, t = 35)
      ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Mafic',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  }

  return(QAP_M)
}
