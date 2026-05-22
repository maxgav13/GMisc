#' QAP Ternary Diagram for Ultramafic Rocks with Hornblende
#' @description \code{ternary_qap_um_hbl()} draws either a static or interactive ternary diagram, in english or spanish. It is a base diagram where data can be plotted.
#' @param output The output format: "ggplot" or "plotly" (default is "ggplot")
#' @param language The language to be displayed: "en" for english or "es" for spanish (deafult is "en")
#' @param opacity Transparency level (default is 0.5)
#'
#' @return QAP ternary diagram for ultramafic rocks with hornblende in the desired format (object)
#' @export
#' @importFrom ggplot2 .data
#'
#' @details For plotting data on the ggplot object it would be easier if the names of the dataframe are "px", "ol", and "hbl", that way it gets mapped automatically, if not make sure to use "aes(x=px,y=ol,z=hbl)".
#' For plotting on the plotly object the mapping of the new data should be as shown in the example: \code{a = ~ol, b = ~px, c = ~hbl}, where \code{a} refers to the top ("ol"), \code{b} refers to the bottom left ("px"), and \code{c} refers to the bottom right ("hbl").
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
#' d = data.frame(ol=c(23,40,60),
#'                px=c(27,50,30),
#'                hbl=c(50,10,10),
#'                loc=c("A", "B", "C"))
#'
#' # adding data to ggplot object
#' ternary_qap_um_hbl() + geom_point(aes(col = loc), data = d)
#'
#' # adding data to plotly object
#' ternary_qap_um_hbl('plotly') %>%
#'   add_trace(a = ~ol, b = ~px, c = ~hbl,
#'             data = d,
#'             name = 'My data',
#'             type = "scatterternary",
#'             mode = "markers",
#'             marker = list(size=8,color='coral',
#'                           symbol=3,opacity=.9),
#'             hovertemplate = paste0('Ol: %{a}<br>',
#'                                    'Px: %{b}<br>',
#'                                    'Hbl: %{c}'))
#'
#' # customizing colors and symbols by location
#' my_pal    <- c("A" = "red",      "B" = "blue",   "C" = "green")
#' my_shapes <- c("A" = "circle",   "B" = "square",  "C" = "diamond")
#'
#' ternary_qap_um_hbl('plotly') %>%
#'   add_trace(a = ~ol, b = ~px, c = ~hbl,
#'             data = d,
#'             type = "scatterternary",
#'             mode = "markers",
#'             split = ~loc,
#'             marker = list(size=8,
#'                           color  = ~my_pal[loc],
#'                           symbol = ~my_shapes[loc],
#'                           opacity=.9),
#'             hovertemplate = paste0('Ol: %{a}<br>',
#'                                    'Px: %{b}<br>',
#'                                    'Hbl: %{c}'))
#'
ternary_qap_um_hbl = function(
  output = c('ggplot', 'plotly'),
  language = c('en', 'es'),
  opacity = 0.5
) {
  tb.QAP_UM = tibble::tribble(
    ~ol , ~px   , ~hbl  , ~Label.en                        , ~Label.es                                      ,
    100 ,   0   ,   0   , "Dunite"                         , "Dunita"                                       ,
     90 ,  10   ,   0   , "Dunite"                         , "Dunita"                                       ,
     90 ,   0   ,  10   , "Dunite"                         , "Dunita"                                       ,
    100 ,   0   ,   0   , "Dunite"                         , "Dunita"                                       ,
     90 ,  10   ,   0   , "Pyroxene peridotite"            , "Peridotita pirox\u00e9nica"                   ,
     40 ,  60   ,   0   , "Pyroxene peridotite"            , "Peridotita pirox\u00e9nica"                   ,
     40 ,  55   ,   5   , "Pyroxene peridotite"            , "Peridotita pirox\u00e9nica"                   ,
     90 ,   5   ,   5   , "Pyroxene peridotite"            , "Peridotita pirox\u00e9nica"                   ,
     90 ,  10   ,   0   , "Pyroxene peridotite"            , "Peridotita pirox\u00e9nica"                   ,
     90 ,   5   ,   5   , "Pyroxene hornblende peridotite" , "Peridotita pirox\u00e9nica hornbl\u00e9ndica" ,
     40 ,  55   ,   5   , "Pyroxene hornblende peridotite" , "Peridotita pirox\u00e9nica hornbl\u00e9ndica" ,
     40 ,   5   ,  55   , "Pyroxene hornblende peridotite" , "Peridotita pirox\u00e9nica hornbl\u00e9ndica" ,
     90 ,   5   ,   5   , "Pyroxene hornblende peridotite" , "Peridotita pirox\u00e9nica hornbl\u00e9ndica" ,
     90 ,   5   ,   5   , "Hornblende peridotite"          , "Peridotita hornbl\u00e9ndica"                 ,
     40 ,   5   ,  55   , "Hornblende peridotite"          , "Peridotita hornbl\u00e9ndica"                 ,
     40 ,   0   ,  60   , "Hornblende peridotite"          , "Peridotita hornbl\u00e9ndica"                 ,
     90 ,   0   ,  10   , "Hornblende peridotite"          , "Peridotita hornbl\u00e9ndica"                 ,
     90 ,   5   ,   5   , "Hornblende peridotite"          , "Peridotita hornbl\u00e9ndica"                 ,
     40 ,  60   ,   0   , "Olivine pyroxenite"             , "Piroxenita oliv\u00ednica"                    ,
     10 ,  90   ,   0   , "Olivine pyroxenite"             , "Piroxenita oliv\u00ednica"                    ,
      5 ,  90   ,   5   , "Olivine pyroxenite"             , "Piroxenita oliv\u00ednica"                    ,
     40 ,  55   ,   5   , "Olivine pyroxenite"             , "Piroxenita oliv\u00ednica"                    ,
     40 ,  60   ,   0   , "Olivine pyroxenite"             , "Piroxenita oliv\u00ednica"                    ,
     40 ,  55   ,   5   , "Olivine hornblende pyroxenite"  , "Piroxenita oliv\u00ednica hornbl\u00e9ndica"  ,
      5 ,  90   ,   5   , "Olivine hornblende pyroxenite"  , "Piroxenita oliv\u00ednica hornbl\u00e9ndica"  ,
      5 ,  47.5 ,  47.5 , "Olivine hornblende pyroxenite"  , "Piroxenita oliv\u00ednica hornbl\u00e9ndica"  ,
     40 ,  30   ,  30   , "Olivine hornblende pyroxenite"  , "Piroxenita oliv\u00ednica hornbl\u00e9ndica"  ,
     40 ,  55   ,   5   , "Olivine hornblende pyroxenite"  , "Piroxenita oliv\u00ednica hornbl\u00e9ndica"  ,
     40 ,  30   ,  30   , "Olivine pyroxene hornblendite"  , "Hornblendita oliv\u00ednica pirox\u00e9nica"  ,
     40 ,   5   ,  55   , "Olivine pyroxene hornblendite"  , "Hornblendita oliv\u00ednica pirox\u00e9nica"  ,
      5 ,   5   ,  90   , "Olivine pyroxene hornblendite"  , "Hornblendita oliv\u00ednica pirox\u00e9nica"  ,
      5 ,  47.5 ,  47.5 , "Olivine pyroxene hornblendite"  , "Hornblendita oliv\u00ednica pirox\u00e9nica"  ,
     40 ,  30   ,  30   , "Olivine pyroxene hornblendite"  , "Hornblendita oliv\u00ednica pirox\u00e9nica"  ,
     40 ,   5   ,  55   , "Olivine hornblendite"           , "Hornblendita oliv\u00ednica"                  ,
      5 ,   5   ,  90   , "Olivine hornblendite"           , "Hornblendita oliv\u00e1nica"                  ,
     10 ,   0   ,  90   , "Olivine hornblendite"           , "Hornblendita oliv\u00e1nica"                  ,
     40 ,   0   ,  60   , "Olivine hornblendite"           , "Hornblendita oliv\u00e1nica"                  ,
     40 ,   5   ,  55   , "Olivine hornblendite"           , "Hornblendita oliv\u00ednica"                  ,
     10 ,  90   ,   0   , "Pyroxenite"                     , "Piroxenita"                                   ,
      0 , 100   ,   0   , "Pyroxenite"                     , "Piroxenita"                                   ,
      0 ,  90   ,  10   , "Pyroxenite"                     , "Piroxenita"                                   ,
     10 ,  90   ,   0   , "Pyroxenite"                     , "Piroxenita"                                   ,
      5 ,  90   ,   5   , "Hornblende pyroxenite"          , "Piroxenita hornbl\u00e9ndica"                 ,
      5 ,  47.5 ,  47.5 , "Hornblende pyroxenite"          , "Piroxenita hornbl\u00e9ndica"                 ,
      0 ,  50   ,  50   , "Hornblende pyroxenite"          , "Piroxenita hornbl\u00e9ndica"                 ,
      0 ,  90   ,  10   , "Hornblende pyroxenite"          , "Piroxenita hornbl\u00e9ndica"                 ,
      5 ,  90   ,   5   , "Hornblende pyroxenite"          , "Piroxenita hornbl\u00e9ndica"                 ,
      5 ,  47.5 ,  47.5 , "Pyroxene hornblendite"          , "Hornblendita pirox\u00e9nica"                 ,
      5 ,   5   ,  90   , "Pyroxene hornblendite"          , "Hornblendita pirox\u00e9nica"                 ,
      0 ,  10   ,  90   , "Pyroxene hornblendite"          , "Hornblendita pirox\u00e9nica"                 ,
      0 ,  50   ,  50   , "Pyroxene hornblendite"          , "Hornblendita pirox\u00e9nica"                 ,
      5 ,  47.5 ,  47.5 , "Pyroxene hornblendite"          , "Hornblendita pirox\u00e9nica"                 ,
     10 ,   0   ,  90   , "Hornblendite"                   , "Hornblendita"                                 ,
      0 ,  10   ,  90   , "Hornblendite"                   , "Hornblendita"                                 ,
      0 ,   0   , 100   , "Hornblendite"                   , "Hornblendita"                                 ,
     10 ,   0   ,  90   , "Hornblendite"                   , "Hornblendita"
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

  QAP_UM.ternaryAxes <- list(
    aaxis = axis("Ol"),
    baxis = axis("Px"),
    caxis = axis("Hbl")
  )

  QAP_UM.pal = c(
    "#FDE725FF",
    "#C2DF23FF",
    "#85D54AFF",
    "#51C56AFF",
    "#2BB07FFF",
    "#1E9B8AFF",
    "#25858EFF",
    "#2D708EFF",
    "#38598CFF",
    "#433E85FF",
    "#482173FF",
    "#440154FF"
  )

  if (any(output == 'ggplot' & language == 'en')) {
    QAP_UM <- ggtern::ggtern(
      data = tb.QAP_UM,
      ggtern::aes(.data$px, .data$ol, .data$hbl)
    ) +
      ggplot2::geom_polygon(
        ggplot2::aes(
          fill = .data$Label.en,
          # color = .data$Label.en,
          group = .data$Label.en
        ),
        alpha = opacity
      ) +
      ggtern::theme_bw() +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_manual(values = QAP_UM.pal) +
      # ggplot2::scale_color_manual(values = QAP_UM.pal) +
      ggplot2::labs(
        fill = "Ultramafic",
        # color = "Ultramafic",
        T = "Ol",
        L = "Px",
        R = "Hbl"
      )
  } else if (any(output == 'ggplot' & language == 'es')) {
    QAP_UM <- ggtern::ggtern(
      data = tb.QAP_UM,
      ggtern::aes(.data$px, .data$ol, .data$hbl)
    ) +
      ggplot2::geom_polygon(
        ggplot2::aes(
          fill = .data$Label.es,
          # color = .data$Label.es,
          group = .data$Label.es
        ),
        alpha = opacity
      ) +
      ggtern::theme_bw() +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_manual(values = QAP_UM.pal) +
      # ggplot2::scale_color_manual(values = QAP_UM.pal) +
      ggplot2::labs(
        fill = "Ultram\u00e1ficas",
        # color = "Ultram\u00e1ficas",
        T = "Ol",
        L = "Px",
        R = "Hbl"
      )
  } else if (any(output == 'plotly' & language == 'en')) {
    QAP_UM = plotly::plot_ly() %>%
      plotly::add_trace(
        data = tb.QAP_UM,
        a = ~ol,
        b = ~px,
        c = ~hbl,
        color = ~Label.en,
        colors = QAP_UM.pal,
        opacity = opacity * 2,
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        hoverinfo = 'text',
        hoveron = 'fills'
      ) %>%
      plotly::layout(
        ternary = QAP_UM.ternaryAxes,
        legend = list(title = list(text = '<b> Ultramafic </b>')),
        margin = list(autoexpand = T, t = 35)
      ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Ultramafic_hbl',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  } else if (any(output == 'plotly' & language == 'es')) {
    QAP_UM = plotly::plot_ly() %>%
      plotly::add_trace(
        data = tb.QAP_UM,
        a = ~ol,
        b = ~px,
        c = ~hbl,
        color = ~Label.es,
        colors = QAP_UM.pal,
        opacity = opacity * 2,
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        # line = list(color = "black"),
        hoverinfo = 'text',
        hoveron = 'fills'
      ) %>%
      plotly::layout(
        ternary = QAP_UM.ternaryAxes,
        legend = list(title = list(text = '<b> Ultram\u00e1ficas </b>')),
        margin = list(autoexpand = T, t = 35)
      ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Ultramafic_hbl',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  }

  return(QAP_UM)
}
