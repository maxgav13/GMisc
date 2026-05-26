#' Ternary Diagram for Pyroclastic Rocks by Type
#' @description \code{ternary_pyroclastic_type()} draws either a static or interactive ternary diagram, in english or spanish. It is a base diagram where data can be plotted.
#' @param output The output format: "ggplot" or "plotly" (default is "ggplot")
#' @param language The language to be displayed: "en" for english or "es" for spanish (deafult is "en")
#' @param opacity Transparency level (default is 0.5)
#'
#' @return Ternary diagram for pyroclastic rocks based on type in the desired format (object)
#' @export
#' @importFrom ggplot2 .data
#'
#' @details For plotting data on the ggplot object it would be easier if the names of the dataframe are "l", "g", and "c", that way it gets mapped automatically, if not make sure to use "aes(x=g,y=l,z=c)".
#' For plotting on the plotly object the mapping of the new data should be as shown in the example: \code{a = ~l, b = ~g, c = ~c}, where \code{a} refers to the top ("l"), \code{b} refers to the bottom left ("g"), and \code{c} refers to the bottom right ("c").
#' The examples show basic usage and how to add data, which can be more customizable.
#'
#' The ternary diagram for pyroclastic rocks based on type is used in volcanology and sedimentary petrology to classify pyroclastic deposits based on the relative proportions of different components, specifically lithic fragments, glass, and crystals. The diagram helps to infer the eruptive style and depositional processes of volcanic eruptions, as different types of eruptions produce characteristic component distributions in the resulting pyroclastic deposits.
#' For a more complete description on how to use this function and similar ones, see
#' \code{vignette("classification_diagrams_vig")}
#'
#' @family classification plots, petrology
#'
#' @examples
#' library(ggplot2)
#' library(plotly)
#'
#' d = data.frame(l=c(23,40,60),
#'                g=c(27,50,30),
#'                c=c(50,10,10),
#'                loc=c("A", "B", "C"))
#'
#' # adding data to ggplot object
#' ternary_pyroclastic_type() + geom_point(aes(col = loc), data = d)
#'
#' # adding data to plotly object
#' ternary_pyroclastic_type('plotly') %>%
#'   add_trace(a = ~l, b = ~g, c = ~c,
#'             data = d,
#'             name = 'My data',
#'             type = "scatterternary",
#'             mode = "markers",
#'             marker = list(size=8,color='cyan',
#'                           symbol=3,opacity=.9),
#'             hovertemplate = paste0('Rock fragments: %{a}<br>',
#'                                    'Glass: %{b}<br>',
#'                                    'Crystals: %{c}'))
#'
#' # adding data to plotly object with custom colors
#' my_pal    <- c("A" = "red",      "B" = "blue",   "C" = "green")
#' my_shapes <- c("A" = "circle",   "B" = "square",  "C" = "diamond")
#'
#' ternary_pyroclastic_type('plotly') %>%
#'   add_trace(a = ~l, b = ~g, c = ~c,
#'             data = d,
#'             type = "scatterternary",
#'             mode = "markers",
#'             split = ~loc,
#'             marker = list(size=8,
#'                           color  = ~my_pal[loc],
#'                           symbol = ~my_shapes[loc],
#'                           opacity=.9),
#'             hovertemplate = paste0('Rock fragments: %{a}<br>',
#'                                    'Glass: %{b}<br>',
#'                                    'Crystals: %{c}'))
#'
ternary_pyroclastic_type = function(
  output = c('ggplot', 'plotly'),
  language = c('en', 'es'),
  opacity = .5
) {
  output <- match.arg(output)
  language <- match.arg(language)

  tb.pyro = tibble::tribble(
    ~l  , ~g  , ~c  , ~Label         , ~Label.es           ,
    100 ,   0 ,   0 , "Lithic tuff"  , "Toba l\u00edtica"  ,
     50 ,  50 ,   0 , "Lithic tuff"  , "Toba l\u00edtica"  ,
     35 ,  35 ,  35 , "Lithic tuff"  , "Toba l\u00edtica"  ,
     50 ,   0 ,  50 , "Lithic tuff"  , "Toba l\u00edtica"  ,
    100 ,   0 ,   0 , "Lithic tuff"  , "Toba l\u00edtica"  ,
      0 , 100 ,   0 , "Vitric tuff"  , "Toba v\u00edtrica" ,
     50 ,  50 ,   0 , "Vitric tuff"  , "Toba v\u00edtrica" ,
     35 ,  35 ,  35 , "Vitric tuff"  , "Toba v\u00edtrica" ,
      0 ,  50 ,  50 , "Vitric tuff"  , "Toba v\u00edtrica" ,
      0 , 100 ,   0 , "Vitric tuff"  , "Toba v\u00edtrica" ,
      0 ,   0 , 100 , "Crystal tuff" , "Toba cristalina"   ,
     50 ,   0 ,  50 , "Crystal tuff" , "Toba cristalina"   ,
     35 ,  35 ,  35 , "Crystal tuff" , "Toba cristalina"   ,
      0 ,  50 ,  50 , "Crystal tuff" , "Toba cristalina"   ,
      0 ,   0 , 100 , "Crystal tuff" , "Toba cristalina"
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

  pyro.ternaryAxes.en <- list(
    aaxis = axis("Rock fragments"),
    baxis = axis("Glass"),
    caxis = axis("Crystals")
  )

  pyro.ternaryAxes.es <- list(
    aaxis = axis("Litoclastos"),
    baxis = axis("Vitroclastos"),
    caxis = axis("Cristaloclastos")
  )

  if (output == 'ggplot' & language == 'en') {
    pyro <- ggtern::ggtern(
      data = tb.pyro,
      ggtern::aes(.data$g, .data$l, .data$c)
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
      ggplot2::scale_fill_brewer(palette = 'Set1') +
      # ggplot2::scale_color_brewer(palette = 'Set1') +
      ggplot2::labs(
        fill = "Pyroclastic rock",
        # color = "Pyroclastic rock",
        T = "Rock fragments",
        L = "Glass",
        R = "Crystals"
      )
  } else if (output == 'ggplot' & language == 'es') {
    pyro <- ggtern::ggtern(
      data = tb.pyro,
      ggtern::aes(.data$g, .data$l, .data$c)
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
      ggplot2::scale_fill_brewer(palette = 'Set1') +
      # ggplot2::scale_color_brewer(palette = 'Set1') +
      ggplot2::labs(
        fill = "Roca pirocl\u00e1stica",
        # color = "Roca pirocl\u00e1stica",
        T = "Litoclastos",
        L = "Vitroclastos",
        R = "Cristaloclastos"
      )
  } else if (output == 'plotly' & language == 'en') {
    pyro = plotly::plot_ly() %>%
      plotly::add_trace(
        data = tb.pyro,
        a = ~l,
        b = ~g,
        c = ~c,
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
        legend = list(title = list(text = '<b> Pyroclastic rock </b>')),
        margin = list(autoexpand = T, t = 35)
      ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Pyroclastic_type',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  } else if (output == 'plotly' & language == 'es') {
    pyro = plotly::plot_ly() %>%
      plotly::add_trace(
        data = tb.pyro,
        a = ~l,
        b = ~g,
        c = ~c,
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
        legend = list(title = list(text = '<b> Roca pirocl\u00e1stica </b>')),
        margin = list(autoexpand = T, t = 35)
      ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Pyroclastic_type',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  }

  return(pyro)
}
