#' Shepard's Ternary Diagram for Soils
#' @description \code{ternary_shepard()} draws either a static or interactive ternary diagram, in english or spanish. It is a base diagram where data can be plotted.
#' @param output The output format: "ggplot" or "plotly" (default is "ggplot")
#' @param language The language to be displayed: "en" for english or "es" for spanish (deafult is "en")
#' @param opacity Transparency level (default is 0.5)
#'
#' @return Shepard's ternary diagram for soils in the desired format (object)
#' @export
#' @importFrom ggplot2 .data
#'
#' @details For plotting data on the ggplot object it would be easier if the names of the dataframe are "sand", "clay", and "silt", that way it gets mapped automatically, if not make sure to use "aes(x=sand,y=clay,z=silt)".
#' For plotting on the plotly object the mapping of the new data should be as shown in the example: \code{a = ~clay, b = ~sand, c = ~silt}, where \code{a} refers to the top ("clay"), \code{b} refers to the bottom left ("sand"), and \code{c} refers to the bottom right ("silt").
#' The examples show basic usage and how to add data, which can be more customizable.
#'
#' @examples
#' library(ggplot2)
#' library(plotly)
#'
#' d = data.frame(sand=c(23,26.9,25.3),
#'                clay=c(27,23.7,5.1),
#'                silt=c(50,49.4,59.6))
#'
#' # adding data to ggplot object
#' ternary_shepard() + geom_point(data = d)
#'
#' # adding data to plotly object
#' ternary_shepard('plotly') %>%
#'   add_trace(a = ~clay, b = ~sand, c = ~silt,
#'             data = d,
#'             name = 'My data',
#'             type = "scatterternary",
#'             mode = "markers",
#'             marker = list(size=8,color='blue',
#'                           symbol=3,opacity=.9),
#'             hovertemplate = paste0('Clay: %{a}<br>',
#'                                    'Sand: %{b}<br>',
#'                                    'Silt: %{c}'))
#'
ternary_shepard = function(output = c('ggplot','plotly'),
                           language = c('en','es'),
                           opacity = 0.5) {

  tb.Shepard = tibble::tribble(
    ~clay, ~sand, ~silt,               ~Label,                ~Label.es,
    1,     0,     0,               "Clay",                "Arcilla",
    0.75,  0.25,     0,               "Clay",                "Arcilla",
    0.75,     0,  0.25,               "Clay",                "Arcilla",
    0.5,   0.5,     0,         "Sandy Clay",        "Arcilla arenosa",
    0.75,  0.25,     0,         "Sandy Clay",        "Arcilla arenosa",
    0.75, 0.125, 0.125,         "Sandy Clay",        "Arcilla arenosa",
    0.6,   0.2,   0.2,         "Sandy Clay",        "Arcilla arenosa",
    0.4,   0.4,   0.2,         "Sandy Clay",        "Arcilla arenosa",
    0.75, 0.125, 0.125,         "Silty Clay",         "Arcilla limosa",
    0.75,     0,  0.25,         "Silty Clay",         "Arcilla limosa",
    0.5,     0,   0.5,         "Silty Clay",         "Arcilla limosa",
    0.4,   0.2,   0.4,         "Silty Clay",         "Arcilla limosa",
    0.6,   0.2,   0.2,         "Silty Clay",         "Arcilla limosa",
    0.6,   0.2,   0.2, "Sand + Silt + Clay", "Arena + Limo + Arcilla",
    0.2,   0.2,   0.6, "Sand + Silt + Clay", "Arena + Limo + Arcilla",
    0.2,   0.6,   0.2, "Sand + Silt + Clay", "Arena + Limo + Arcilla",
    0.5,   0.5,     0,        "Clayey Sand",        "Arena arcillosa",
    0.4,   0.4,   0.2,        "Clayey Sand",        "Arena arcillosa",
    0.2,   0.6,   0.2,        "Clayey Sand",        "Arena arcillosa",
    0.125,  0.75, 0.125,        "Clayey Sand",        "Arena arcillosa",
    0.25,  0.75,     0,        "Clayey Sand",        "Arena arcillosa",
    0.5,     0,   0.5,        "Clayey Silt",         "Limo arcilloso",
    0.25,     0,  0.75,        "Clayey Silt",         "Limo arcilloso",
    0.125, 0.125,  0.75,        "Clayey Silt",         "Limo arcilloso",
    0.2,   0.2,   0.6,        "Clayey Silt",         "Limo arcilloso",
    0.4,   0.2,   0.4,        "Clayey Silt",         "Limo arcilloso",
    0,     1,     0,               "Sand",                  "Arena",
    0.25,  0.75,     0,               "Sand",                  "Arena",
    0,  0.75,  0.25,               "Sand",                  "Arena",
    0.125,  0.75, 0.125,         "Silty Sand",           "Arena limosa",
    0.2,   0.6,   0.2,         "Silty Sand",           "Arena limosa",
    0.2,   0.4,   0.4,         "Silty Sand",           "Arena limosa",
    0,   0.5,   0.5,         "Silty Sand",           "Arena limosa",
    0,  0.75,  0.25,         "Silty Sand",           "Arena limosa",
    0.2,   0.4,   0.4,         "Sandy Silt",           "Limo arenoso",
    0.2,   0.2,   0.6,         "Sandy Silt",           "Limo arenoso",
    0.125, 0.125,  0.75,         "Sandy Silt",           "Limo arenoso",
    0,  0.25,  0.75,         "Sandy Silt",           "Limo arenoso",
    0,   0.5,   0.5,         "Sandy Silt",           "Limo arenoso",
    0.25,     0,  0.75,               "Silt",                   "Limo",
    0,     0,     1,               "Silt",                   "Limo",
    0,  0.25,  0.75,               "Silt",                   "Limo"
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

  Shepard.ternaryAxes.en <- list(
    aaxis = axis("Clay"),
    baxis = axis("Sand"),
    caxis = axis("Silt")
  )

  Shepard.ternaryAxes.es <- list(
    aaxis = axis("Arcilla"),
    baxis = axis("Arena"),
    caxis = axis("Limo")
  )


  if (any(output == 'ggplot' & language == 'en')) {
    Shepard <- ggtern::ggtern(data=tb.Shepard,
                              ggtern::aes(.data$sand,.data$clay,.data$silt)) +
      ggplot2::geom_polygon(aes(fill=.data$Label,
                                color=.data$Label,
                                group=.data$Label),
                            alpha=opacity) +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_brewer(palette = 'Set3') +
      ggplot2::scale_color_brewer(palette = 'Set3') +
      ggplot2::labs(title="Shepard",
                    fill = "Soil",
                    color = "Soil",
                    T="Clay",
                    L="Sand",
                    R="Silt")
  } else if (any(output == 'ggplot' & language == 'es')) {
    Shepard <- ggtern::ggtern(data=tb.Shepard,
                              ggtern::aes(.data$sand,.data$clay,.data$silt)) +
      ggplot2::geom_polygon(aes(fill=.data$Label.es,
                                color=.data$Label.es,
                                group=.data$Label.es),
                            alpha=opacity) +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_brewer(palette = 'Set3') +
      ggplot2::scale_color_brewer(palette = 'Set3') +
      ggplot2::labs(title="Shepard",
                    fill = "Suelo",
                    color = "Suelo",
                    T="Arcilla",
                    L="Arena",
                    R="Limo")
  } else if (any(output == 'plotly' & language == 'en')) {
    Shepard = plotly::plot_ly() %>%
      plotly::add_trace(
        data = tb.Shepard,
        a = ~clay, b = ~sand, c = ~silt,
        color = ~Label,
        colors = 'Set3',
        opacity = opacity*2,
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        hoveron = 'fills'
      ) %>%
      plotly::layout(
        annotations = label("Shepard"), ternary = Shepard.ternaryAxes.en,
        legend = list(title=list(text='<b> Soil </b>'))
      ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Shepard',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  } else if (any(output == 'plotly' & language == 'es')) {
    Shepard = plotly::plot_ly() %>%
      plotly::add_trace(
        data = tb.Shepard,
        a = ~clay, b = ~sand, c = ~silt,
        color = ~Label.es,
        colors = 'Set3',
        opacity = opacity*2,
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        hoveron = 'fills'
      ) %>%
      plotly::layout(
        annotations = label("Shepard"), ternary = Shepard.ternaryAxes.es,
        legend = list(title=list(text='<b> Suelo </b>'))
      ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Shepard',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  }

  return(Shepard)

}
