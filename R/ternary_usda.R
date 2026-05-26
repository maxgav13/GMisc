#' USDA's Ternary Diagram for Soils
#' @description \code{ternary_usda()} draws either a static or interactive ternary diagram, in english or spanish. It is a base diagram where data can be plotted.
#' @param output The output format: "ggplot" or "plotly" (default is "ggplot")
#' @param language The language to be displayed: "en" for english or "es" for spanish (deafult is "en")
#' @param opacity Transparency level (default is 0.5)
#'
#' @return USDA's ternary diagram for soils in the desired format (object)
#' @export
#' @importFrom ggplot2 .data
#'
#' @details For plotting data on the ggplot object it would be easier if the names of the dataframe are "sand", "clay", and "silt", that way it gets mapped automatically, if not make sure to use "aes(x=sand,y=clay,z=silt)".
#' For plotting on the plotly object the mapping of the new data should be as shown in the example: \code{a = ~clay, b = ~sand, c = ~silt}, where \code{a} refers to the top ("clay"), \code{b} refers to the bottom left ("sand"), and \code{c} refers to the bottom right ("silt").
#' The examples show basic usage and how to add data, which can be more customizable.
#' For a more complete description on how to use this function and similar ones, see
#' \code{vignette("classification_diagrams_vig")}
#'
#' @family classification plots, soil mechanics
#'
#' @examples
#' library(ggplot2)
#' library(plotly)
#'
#' d = data.frame(sand=c(23,40,60),
#'                clay=c(27,50,30),
#'                silt=c(50,10,10),
#'                loc=c("A", "B", "C"))
#'
#' # adding data to ggplot object
#' ternary_usda() + geom_point(aes(col = loc), data = d)
#'
#' # adding data to plotly object
#' ternary_usda('plotly') %>%
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
#' # customizing colors and symbols by location
#' my_pal    <- c("A" = "red",      "B" = "blue",   "C" = "green")
#' my_shapes <- c("A" = "circle",   "B" = "square",  "C" = "diamond")
#'
#' ternary_usda('plotly') %>%
#'   add_trace(a = ~clay, b = ~sand, c = ~silt,
#'             data = d,
#'             type = "scatterternary",
#'             mode = "markers",
#'             split = ~loc,
#'             marker = list(size=8,
#'                           color  = ~my_pal[loc],
#'                           symbol = ~my_shapes[loc],
#'                           opacity=.9),
#'             hovertemplate = paste0('Clay: %{a}<br>',
#'                                    'Sand: %{b}<br>',
#'                                    'Silt: %{c}'))
#'
ternary_usda = function(
  output = c('ggplot', 'plotly'),
  language = c('en', 'es'),
  opacity = 0.5
) {
  output <- match.arg(output)
  language <- match.arg(language)

  tb.usda = tibble::tribble(
    ~clay , ~sand , ~silt , ~Label            ,
    1     , 0     , 0     , "Clay"            ,
    0.55  , 0.45  , 0     , "Clay"            ,
    0.4   , 0.45  , 0.15  , "Clay"            ,
    0.4   , 0.2   , 0.4   , "Clay"            ,
    0.6   , 0     , 0.4   , "Clay"            ,
    1     , 0     , 0     , "Clay"            ,
    0.55  , 0.45  , 0     , "Sandy Clay"      ,
    0.35  , 0.65  , 0     , "Sandy Clay"      ,
    0.35  , 0.45  , 0.2   , "Sandy Clay"      ,
    0.55  , 0.45  , 0     , "Sandy Clay"      ,
    0.35  , 0.65  , 0     , "Sandy Clay Loam" ,
    0.2   , 0.8   , 0     , "Sandy Clay Loam" ,
    0.2   , 0.525 , 0.275 , "Sandy Clay Loam" ,
    0.275 , 0.45  , 0.275 , "Sandy Clay Loam" ,
    0.35  , 0.45  , 0.2   , "Sandy Clay Loam" ,
    0.35  , 0.65  , 0     , "Sandy Clay Loam" ,
    0.2   , 0.8   , 0     , "Sandy Loam"      ,
    0.15  , 0.85  , 0     , "Sandy Loam"      ,
    0     , 0.7   , 0.3   , "Sandy Loam"      ,
    0     , 0.5   , 0.5   , "Sandy Loam"      ,
    0.05  , 0.45  , 0.5   , "Sandy Loam"      ,
    0.05  , 0.525 , 0.425 , "Sandy Loam"      ,
    0.2   , 0.525 , 0.275 , "Sandy Loam"      ,
    0.2   , 0.8   , 0     , "Sandy Loam"      ,
    0.15  , 0.85  , 0     , "Loamy Sand"      ,
    0.1   , 0.9   , 0     , "Loamy Sand"      ,
    0     , 0.85  , 0.15  , "Loamy Sand"      ,
    0     , 0.7   , 0.3   , "Loamy Sand"      ,
    0.15  , 0.85  , 0     , "Loamy Sand"      ,
    0.1   , 0.9   , 0     , "Sand"            ,
    0     , 1     , 0     , "Sand"            ,
    0     , 0.85  , 0.15  , "Sand"            ,
    0.1   , 0.9   , 0     , "Sand"            ,
    0.4   , 0.45  , 0.15  , "Clay Loam"       ,
    0.275 , 0.45  , 0.275 , "Clay Loam"       ,
    0.275 , 0.2   , 0.525 , "Clay Loam"       ,
    0.4   , 0.2   , 0.4   , "Clay Loam"       ,
    0.4   , 0.45  , 0.15  , "Clay Loam"       ,
    0.275 , 0.45  , 0.275 , "Loam"            ,
    0.2   , 0.525 , 0.275 , "Loam"            ,
    0.05  , 0.525 , 0.425 , "Loam"            ,
    0.05  , 0.45  , 0.5   , "Loam"            ,
    0.275 , 0.225 , 0.5   , "Loam"            ,
    0.275 , 0.45  , 0.275 , "Loam"            ,
    0.275 , 0.225 , 0.5   , "Silt Loam"       ,
    0     , 0.5   , 0.5   , "Silt Loam"       ,
    0     , 0.2   , 0.8   , "Silt Loam"       ,
    0.125 , 0.075 , 0.8   , "Silt Loam"       ,
    0.125 , 0     , 0.875 , "Silt Loam"       ,
    0.275 , 0     , 0.725 , "Silt Loam"       ,
    0.275 , 0.225 , 0.5   , "Silt Loam"       ,
    0.6   , 0     , 0.4   , "Silty Clay"      ,
    0.4   , 0.2   , 0.4   , "Silty Clay"      ,
    0.4   , 0     , 0.6   , "Silty Clay"      ,
    0.6   , 0     , 0.4   , "Silty Clay"      ,
    0.4   , 0.2   , 0.4   , "Silty Clay Loam" ,
    0.275 , 0.2   , 0.525 , "Silty Clay Loam" ,
    0.275 , 0     , 0.725 , "Silty Clay Loam" ,
    0.4   , 0     , 0.6   , "Silty Clay Loam" ,
    0.4   , 0.2   , 0.4   , "Silty Clay Loam" ,
    0.125 , 0.075 , 0.8   , "Silt"            ,
    0     , 0.2   , 0.8   , "Silt"            ,
    0     , 0     , 1     , "Silt"            ,
    0.125 , 0     , 0.875 , "Silt"            ,
    0.125 , 0.075 , 0.8   , "Silt"
  ) %>%
    dplyr::mutate(
      Label.es = dplyr::case_when(
        Label == 'Clay' ~ "Arcilla",
        Label == 'Sandy Clay' ~ "Arcilla arenosa",
        Label == 'Sandy Clay Loam' ~ "Franco arcillo arenoso",
        Label == 'Sandy Loam' ~ "Franco arenoso",
        Label == 'Loamy Sand' ~ "Arena franca",
        Label == 'Sand' ~ "Arena",
        Label == 'Clay Loam' ~ "Franco arcilloso",
        Label == 'Loam' ~ "Franco",
        Label == 'Silt Loam' ~ "Franco limoso",
        Label == 'Silty Clay' ~ "Arcilla limosa",
        Label == 'Silty Clay Loam' ~ "Franco arcillo limoso",
        Label == 'Silt' ~ "Limo"
      ),
      dplyr::across(.data$Label:.data$Label.es, forcats::as_factor)
    )

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

  usda.ternaryAxes.en <- list(
    aaxis = axis("Clay"),
    baxis = axis("Sand"),
    caxis = axis("Silt")
  )

  usda.ternaryAxes.es <- list(
    aaxis = axis("Arcilla"),
    baxis = axis("Arena"),
    caxis = axis("Limo")
  )

  if (output == 'ggplot' & language == 'en') {
    usda <- ggtern::ggtern(
      data = tb.usda,
      ggtern::aes(.data$sand, .data$clay, .data$silt)
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
      ggplot2::scale_fill_brewer(palette = 'Set3') +
      # ggplot2::scale_color_brewer(palette = 'Set3') +
      ggplot2::labs(
        fill = "Soil",
        # color = "Soil",
        T = "Clay",
        L = "Sand",
        R = "Silt"
      )
  } else if (output == 'ggplot' & language == 'es') {
    usda <- ggtern::ggtern(
      data = tb.usda,
      ggtern::aes(.data$sand, .data$clay, .data$silt)
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
      ggplot2::scale_fill_brewer(palette = 'Set3') +
      # ggplot2::scale_color_brewer(palette = 'Set3') +
      ggplot2::labs(
        fill = "Suelo",
        # color = "Suelo",
        T = "Arcilla",
        L = "Arena",
        R = "Limo"
      )
  } else if (output == 'plotly' & language == 'en') {
    usda = plotly::plot_ly() %>%
      plotly::add_trace(
        data = tb.usda,
        a = ~clay,
        b = ~sand,
        c = ~silt,
        color = ~Label,
        colors = 'Set3',
        opacity = opacity * 2,
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        line = list(color = "black", width = 0.5),
        hoverinfo = 'text',
        hoveron = 'fills'
      ) %>%
      plotly::layout(
        ternary = usda.ternaryAxes.en,
        legend = list(title = list(text = '<b> Soil </b>')),
        margin = list(autoexpand = T, t = 35)
      ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'usda',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  } else if (output == 'plotly' & language == 'es') {
    usda = plotly::plot_ly() %>%
      plotly::add_trace(
        data = tb.usda,
        a = ~clay,
        b = ~sand,
        c = ~silt,
        color = ~Label.es,
        colors = 'Set3',
        opacity = opacity * 2,
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        line = list(color = "black", width = 0.5),
        hoverinfo = 'text',
        hoveron = 'fills'
      ) %>%
      plotly::layout(
        ternary = usda.ternaryAxes.es,
        legend = list(title = list(text = '<b> Suelo </b>')),
        margin = list(autoexpand = T, t = 35)
      ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'usda',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  }

  return(usda)
}
