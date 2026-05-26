#' Ternary Diagram for Pyroclastic Rocks by Size
#' @description \code{ternary_pyroclastic_size()} draws either a static or interactive ternary diagram, in english or spanish. It is a base diagram where data can be plotted.
#' @param output The output format: "ggplot" or "plotly" (default is "ggplot")
#' @param language The language to be displayed: "en" for english or "es" for spanish (deafult is "en")
#' @param opacity Transparency level (default is 0.5)
#'
#' @return Ternary diagram for pyroclastic rocks based on size in the desired format (object)
#' @export
#' @importFrom ggplot2 .data
#'
#' @details For plotting data on the ggplot object it would be easier if the names of the dataframe are "lapilli", "bb", and "ash", that way it gets mapped automatically, if not make sure to use "aes(x=lapilli,y=bb,z=ash)".
#' For plotting on the plotly object the mapping of the new data should be as shown in the example: \code{a = ~bb, b = ~lapilli, c = ~ash}, where \code{a} refers to the top ("bb"), \code{b} refers to the bottom left ("lapilli"), and \code{c} refers to the bottom right ("ash").
#' The examples show basic usage and how to add data, which can be more customizable.
#' For a more complete description on how to use this function and similar ones, see
#' \code{vignette("classification_diagrams_vig")}
#'
#' The ternary diagram for pyroclastic rocks based on size is used in volcanology and sedimentary petrology to classify pyroclastic deposits based on the relative proportions of different grain sizes, specifically blocks and bombs (> 64 mm), lapilli (2-64 mm), and ash (< 2 mm). The diagram helps to infer the eruptive style and depositional processes of volcanic eruptions, as different types of eruptions produce characteristic grain size distributions in the resulting pyroclastic deposits.
#'
#' @family classification plots, petrology
#'
#' @examples
#' library(ggplot2)
#' library(plotly)
#'
#' d = data.frame(lapilli=c(23,40,60),
#'                bb=c(27,50,30),
#'                ash=c(50,10,10),
#'                loc=c("A", "B", "C"))
#'
#' # adding data to ggplot object
#' ternary_pyroclastic_size() + geom_point(aes(col = loc), data = d)
#'
#' # adding data to plotly object
#' ternary_pyroclastic_size('plotly') %>%
#'   add_trace(a = ~bb, b = ~lapilli, c = ~ash,
#'             data = d,
#'             name = 'My data',
#'             type = "scatterternary",
#'             mode = "markers",
#'             marker = list(size=8,color='cyan',
#'                           symbol=3,opacity=.9),
#'             hovertemplate = paste0('BB: %{a}<br>',
#'                                    'Lapilli: %{b}<br>',
#'                                    'Ash: %{c}'))
#'
#' # adding data to plotly object with custom colors
#' my_pal    <- c("A" = "red",      "B" = "blue",   "C" = "green")
#' my_shapes <- c("A" = "circle",   "B" = "square",  "C" = "diamond")
#'
#' ternary_pyroclastic_size('plotly') %>%
#'   add_trace(a = ~bb, b = ~lapilli, c = ~ash,
#'             data = d,
#'             type = "scatterternary",
#'             mode = "markers",
#'             split = ~loc,
#'             marker = list(size=8,
#'                           color  = ~my_pal[loc],
#'                           symbol = ~my_shapes[loc],
#'                           opacity=.9),
#'             hovertemplate = paste0('BB: %{a}<br>',
#'                                    'Lapilli: %{b}<br>',
#'                                    'Ash: %{c}'))
#'
ternary_pyroclastic_size = function(
  output = c('ggplot', 'plotly'),
  language = c('en', 'es'),
  opacity = .5
) {
  output <- match.arg(output)
  language <- match.arg(language)

  tb.pyro = tibble::tribble(
    ~bb, ~lapilli, ~ash, ~Label, ~Label.es,
    100,0,0,"Pyroclastic breccia /
        Agglomerate","Brecha pirocl\u00e1stica /
        Aglomerado",
    75,25,0,"Pyroclastic breccia /
        Agglomerate","Brecha pirocl\u00e1stica /
        Aglomerado",
    75,0,25,"Pyroclastic breccia /
        Agglomerate","Brecha pirocl\u00e1stica /
        Aglomerado",
    100,0,0,"Pyroclastic breccia /
        Agglomerate","Brecha pirocl\u00e1stica /
        Aglomerado",
    75,25,0,"Lapilli-tuff breccia","Toba brechosa",
    25,75,0,"Lapilli-tuff breccia","Toba brechosa",
    25,0,75,"Lapilli-tuff breccia","Toba brechosa",
    75,0,25,"Lapilli-tuff breccia","Toba brechosa",
    75,25,0,"Lapilli-tuff breccia","Toba brechosa",
    25,75,0,"Lapilli-stone","Lapillita",
    0,100,0,"Lapilli-stone","Lapillita",
    0,75,25,"Lapilli-stone","Lapillita",
    25,75,0,"Lapilli-stone","Lapillita",
    25,75,0,"Lapilli tuff","Toba lapill\u00edtica",
    0,75,25,"Lapilli tuff","Toba lapill\u00edtica",
    0,25,75,"Lapilli tuff","Toba lapill\u00edtica",
    25,0,75,"Lapilli tuff","Toba lapill\u00edtica",
    25,75,0,"Lapilli tuff","Toba lapill\u00edtica",
    25,0,75,"Tuff","Toba",
    0,25,75,"Tuff","Toba",
    0,0,100,"Tuff","Toba",
    25,0,75,"Tuff","Toba") %>%
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

  pyro.ternaryAxes.en <- list(
    aaxis = axis("Blocks & Bombs (> 64 mm)"),
    baxis = axis("Lapilli (2-64 mm)"),
    caxis = axis("Ash (< 2 mm)")
  )

  pyro.ternaryAxes.es <- list(
    aaxis = axis("Bloques & Bombas (> 64 mm)"),
    baxis = axis("Lapilli (2-64 mm)"),
    caxis = axis("Ceniza (< 2 mm)")
  )

  if (output == 'ggplot' & language == 'en') {
    pyro <- ggtern::ggtern(
      data = tb.pyro,
      ggtern::aes(.data$lapilli, .data$bb, .data$ash)
    ) +
      ggplot2::geom_polygon(
        aes(
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
      ggplot2::scale_fill_brewer(palette = 'Set1') +
      # ggplot2::scale_color_brewer(palette = 'Set1') +
      ggplot2::labs(
        # color = "Pyroclastic rock",
        fill = "Pyroclastic rock",
        T = "Blocks &\nBombs (> 64 mm)",
        L = "Lapilli\n(2-64 mm)",
        R = "Ash\n(< 2 mm)"
      )
  } else if (output == 'ggplot' & language == 'es') {
    pyro <- ggtern::ggtern(
      data = tb.pyro,
      ggtern::aes(.data$lapilli, .data$bb, .data$ash)
    ) +
      ggplot2::geom_polygon(
        aes(
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
      ggplot2::scale_fill_brewer(palette = 'Set1') +
      # ggplot2::scale_color_brewer(palette = 'Set1') +
      ggplot2::labs(
        # color = "Roca Pirocl\u00e1stica",
        fill = "Roca Pirocl\u00e1stica",
        T = "Bloques &\nBombas (> 64 mm)",
        L = "Lapilli\n(2-64 mm)",
        R = "Ceniza\n(< 2 mm)"
      )
  } else if (output == 'plotly' & language == 'en') {
    pyro = plotly::plot_ly() %>%
      plotly::add_trace(
        data = tb.pyro,
        a = ~bb,
        b = ~lapilli,
        c = ~ash,
        color = ~Label,
        colors = 'Set1',
        opacity = opacity * 2,
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        line = list(color = "black", width = 0.5),
        hoverinfo = 'text',
        hoveron = 'fills'
      ) %>%
      plotly::layout(
        ternary = pyro.ternaryAxes.en,
        legend = list(title = list(text = '<b> Pyroclastic rock </b>'))
      ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Pyroclastic_size',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  } else if (output == 'plotly' & language == 'es') {
    pyro = plotly::plot_ly() %>%
      plotly::add_trace(
        data = tb.pyro,
        a = ~bb,
        b = ~lapilli,
        c = ~ash,
        color = ~Label.es,
        colors = 'Set1',
        opacity = opacity * 2,
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        line = list(color = "black", width = 0.5),
        hoverinfo = 'text',
        hoveron = 'fills'
      ) %>%
      plotly::layout(
        ternary = pyro.ternaryAxes.es,
        legend = list(title = list(text = '<b> Roca Pirocl\u00e1stica </b>'))
      ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Pyroclastic_size',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  }

  return(pyro)
}
