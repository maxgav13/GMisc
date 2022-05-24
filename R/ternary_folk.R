#' Folk's Ternary Diagram for Sandstones
#' @description \code{ternary_folk()} draws either a static or interactive ternary diagram, in english or spanish. It is a base diagram where data can be plotted.
#' @param output The output format: "ggplot" or "plotly" (default is "ggplot")
#' @param language The language to be displayed: "en" for english or "es" for spanish (deafult is "en")
#' @param opacity Transparency level (default is 0.5)
#'
#' @return Folk's ternary diagram for sandstones in the desired format (object)
#' @export
#'
#' @details For plotting data on the ggplot object it would be easier if the names of the dataframe are "f", "q", and "r", that way it gets mapped automatically, if not make sure to use "aes(x=f,y=q,z=r)".
#' For plotting on the plotly object the mapping of the new data should be as shown in the example: \code{a = ~q, b = ~f, c = ~r}, where \code{a} refers to the top ("q"), \code{b} refers to the bottom left ("f"), and \code{c} refers to the bottom right ("r").
#' The examples show basic usage and how to add data, which can be more customizable.
#'
#' @examples
#' library(ggplot2)
#' library(plotly)
#'
#' d = data.frame(q=c(23,26.9,25.3),
#'                f=c(27,23.7,5.1),
#'                r=c(50,49.4,59.6))
#'
#' # adding data to ggplot object
#' ternary_folk() + geom_point(data = d)
#'
#' # adding data to plotly object
#' ternary_folk('plotly') %>%
#'   add_trace(a = ~q, b = ~f, c = ~r,
#'             data = d,
#'             name = 'My data',
#'             type = "scatterternary",
#'             mode = "markers",
#'             marker = list(size=8,color='coral',
#'                           symbol=3,opacity=.9),
#'             hovertemplate = paste0('Q: %{a}<br>',
#'                                    'F: %{b}<br>',
#'                                    'R: %{c}'))
#'
ternary_folk = function(output = c('ggplot','plotly'),
                        language = c('en','es'),
                        opacity = .5) {

  tb.Folk = tibble::tribble(
    ~q,    ~f,    ~r,                    ~Label,                 ~Label.es,
    100,     0,     0,           "Quartzarenite",          "Quarzo arenita",
    95,     5,     0,           "Quartzarenite",          "Quarzo arenita",
    95,   2.5,   2.5,           "Quartzarenite",          "Quarzo arenita",
    95,     0,     5,           "Quartzarenite",          "Quarzo arenita",
    95,     5,     0,               "Subarkose",               "Subarcosa",
    75,    25,     0,               "Subarkose",               "Subarcosa",
    75,  12.5,  12.5,               "Subarkose",               "Subarcosa",
    95,   2.5,   2.5,               "Subarkose",               "Subarcosa",
    95,   2.5,   2.5,          "Sublitharenite",           "Sublitarenita",
    75,  12.5,  12.5,          "Sublitharenite",           "Sublitarenita",
    75,     0,    25,          "Sublitharenite",           "Sublitarenita",
    95,     0,     5,          "Sublitharenite",           "Sublitarenita",
    75,    25,     0,                  "Arkose",                  "Arcosa",
    0,   100,     0,                  "Arkose",                  "Arcosa",
    0,    75,    25,                  "Arkose",                  "Arcosa",
    75, 18.75,  6.25,                  "Arkose",                  "Arcosa",
    75, 18.75,  6.25,           "Lithik Arkose",           "Arcosa l\u00edtica",
    0,    75,    25,           "Lithik Arkose",           "Arcosa l\u00edtica",
    0,    50,    50,           "Lithik Arkose",           "Arcosa l\u00edtica",
    75,  12.5,  12.5,           "Lithik Arkose",           "Arcosa l\u00edtica",
    75,  12.5,  12.5, "Feldspathic Litharenite", "Litarenita feldesp\u00e1tica",
    0,    50,    50, "Feldspathic Litharenite", "Litarenita feldesp\u00e1tica",
    0,    27,    75, "Feldspathic Litharenite", "Litarenita feldesp\u00e1tica",
    75,  6.25, 18.75, "Feldspathic Litharenite", "Litarenita feldesp\u00e1tica",
    75,  6.25, 18.75,             "Litharenite",              "Litarenita",
    0,    27,    75,             "Litharenite",              "Litarenita",
    0,     0,   100,             "Litharenite",              "Litarenita",
    75,     0,    25,             "Litharenite",              "Litarenita"
  ) %>%
    dplyr::mutate(dplyr::across(Label:Label.es,forcats::as_factor))


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

  Folk.ternaryAxes <- list(
    aaxis = axis("Q"),
    baxis = axis("F"),
    caxis = axis("R")
  )

  Folk.pal = c("#777777", "#AACDC9", "#C0CF98",
               "#2F4996", "#D3C6E2", "#EBD1C0", "#564E37")

  if (any(output == 'ggplot' & language == 'en')) {
    Folk <- ggtern::ggtern(data=tb.Folk,ggtern::aes(f,q,r)) +
      ggplot2::geom_polygon(ggplot2::aes(fill=Label,color=Label,
                                         group=Label),
                            alpha=opacity) +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_manual(values = Folk.pal) +
      ggplot2::scale_color_manual(values = Folk.pal) +
      ggplot2::labs(title="Folk",
                    fill = "Sandstone",
                    color = "Sandstone",
                    T="Q",
                    L="F",
                    R="R")
  } else if (any(output == 'ggplot' & language == 'es')) {
    Folk <- ggtern::ggtern(data=tb.Folk,ggtern::aes(f,q,r)) +
      ggplot2::geom_polygon(ggplot2::aes(fill=Label.es,color=Label.es,
                                         group=Label.es),
                            alpha=opacity) +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_manual(values = Folk.pal) +
      ggplot2::scale_color_manual(values = Folk.pal) +
      ggplot2::labs(title="Folk",
                    fill = "Arenisca",
                    color = "Arenisca",
                    T="Q",
                    L="F",
                    R="R")
  } else if (any(output == 'plotly' & language == 'en')) {
    Folk = plotly::plot_ly() %>%
      plotly::add_trace(
        data = tb.Folk,
        a = ~q, b = ~f, c = ~r,
        color = ~Label,
        colors = Folk.pal %>% purrr::set_names(levels(tb.Folk$Label)),
        opacity = opacity*2,
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        hoveron = 'fills'
      ) %>%
      plotly::layout(
        annotations = label("Folk"), ternary = Folk.ternaryAxes,
        legend = list(title=list(text='<b> Sandstone </b>'))
      ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Folk',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  } else if (any(output == 'plotly' & language == 'es')) {
    Folk = plotly::plot_ly() %>%
      plotly::add_trace(
        data = tb.Folk,
        a = ~q, b = ~f, c = ~r,
        color = ~Label.es,
        colors = Folk.pal %>% purrr::set_names(levels(tb.Folk$Label.es)),
        opacity = opacity*2,
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        hoveron = 'fills'
      ) %>%
      plotly::layout(
        annotations = label("Folk"), ternary = Folk.ternaryAxes,
        legend = list(title=list(text='<b> Arenisca </b>'))
      ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Folk',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  }

  return(Folk)

}

