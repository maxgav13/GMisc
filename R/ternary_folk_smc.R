#' Folk's Ternary Diagram for Clastic Fine-Grained Rocks
#' @description \code{ternary_folk_smc()} draws either a static or interactive ternary diagram, in english or spanish. It is a base diagram where data can be plotted.
#' @param output The output format: "ggplot" or "plotly" (default is "ggplot")
#' @param language The language to be displayed: "en" for english or "es" for spanish (deafult is "en")
#' @param opacity Transparency level (default is 0.5)
#'
#' @return Folk's ternary diagram for sandstones in the desired format (object)
#' @export
#' @importFrom ggplot2 .data
#'
#' @details For plotting data on the ggplot object it would be easier if the names of the dataframe are "m", "s", and "c", that way it gets mapped automatically, if not make sure to use "aes(x=m,y=s,z=c)".
#' For plotting on the plotly object the mapping of the new data should be as shown in the example: \code{a = ~s, b = ~m, c = ~c}, where \code{a} refers to the top ("s"), \code{b} refers to the bottom left ("m"), and \code{c} refers to the bottom right ("c").
#' The examples show basic usage and how to add data, which can be more customizable.
#'
#' @family classification plots, petrology
#'
#' @examples
#' library(ggplot2)
#' library(plotly)
#'
#' d = data.frame(m=c(23,40,60),
#'                s=c(27,50,30),
#'                c=c(50,10,10))
#'
#' # adding data to ggplot object
#' ternary_folk_smc() + geom_point(data = d)
#'
#' # adding data to plotly object
#' ternary_folk_smc('plotly') %>%
#'   add_trace(a = ~s, b = ~m, c = ~c,
#'             data = d,
#'             name = 'My data',
#'             type = "scatterternary",
#'             mode = "markers",
#'             marker = list(size=8,color='coral',
#'                           symbol=3,opacity=.9),
#'             hovertemplate = paste0('S: %{a}<br>',
#'                                    'M: %{b}<br>',
#'                                    'C: %{c}'))
#'
ternary_folk_smc = function(
  output = c('ggplot', 'plotly'),
  language = c('en', 'es'),
  opacity = .5
) {
  tb.Folk = tibble::tribble(
    ~s  , ~m    , ~c    , ~Label                             , ~Label.es                                 ,
    100 ,   0   ,   0   , "Sandstone \n(Sand)"               , "Arenisca \n(Arena)"                      ,
     90 ,  10   ,   0   , "Sandstone \n(Sand)"               , "Arenisca \n(Arena)"                      ,
     90 ,   0   ,  10   , "Sandstone \n(Sand)"               , "Arenisca \n(Arena)"                      ,
    100 ,   0   ,   0   , "Sandstone \n(Sand)"               , "Arenisca \n(Arena)"                      ,
     90 ,  10   ,   0   , "Silty sandstone \n(Silty sand)"   , "Arenisca limosa \n(Arena limosa)"        ,
     90 ,   6.5 ,   3.5 , "Silty sandstone \n(Silty sand)"   , "Arenisca limosa \n(Arena limosa)"        ,
     50 ,  33.4 ,  16.6 , "Silty sandstone \n(Silty sand)"   , "Arenisca limosa \n(Arena limosa)"        ,
     50 ,  50   ,   0   , "Silty sandstone \n(Silty sand)"   , "Arenisca limosa \n(Arena limosa)"        ,
     90 ,  10   ,   0   , "Silty sandstone \n(Silty sand)"   , "Arenisca limosa \n(Arena limosa)"        ,
     90 ,   6.5 ,   3.5 , "Muddy sandstone \n(Muddy sand)"   , "Arenisca lodosa \n(Arena lodosa)"        ,
     90 ,   3.5 ,   6.5 , "Muddy sandstone \n(Muddy sand)"   , "Arenisca lodosa \n(Arena lodosa)"        ,
     50 ,  16.6 ,  33.4 , "Muddy sandstone \n(Muddy sand)"   , "Arenisca lodosa \n(Arena lodosa)"        ,
     50 ,  33.4 ,  16.6 , "Muddy sandstone \n(Muddy sand)"   , "Arenisca lodosa \n(Arena lodosa)"        ,
     90 ,   6.5 ,   3.5 , "Muddy sandstone \n(Muddy sand)"   , "Arenisca lodosa \n(Arena lodosa)"        ,
     90 ,   3.5 ,   6.5 , "Clayey sandstone \n(Clayey sand)" , "Arenisca arcillosa \n(Arena arcillosa)"  ,
     90 ,   0   ,  10   , "Clayey sandstone \n(Clayey sand)" , "Arenisca arcillosa \n(Arena arcillosa)"  ,
     50 ,   0   ,  50   , "Clayey sandstone \n(Clayey sand)" , "Arenisca arcillosa \n(Arena arcillosa)"  ,
     50 ,  16.6 ,  33.4 , "Clayey sandstone \n(Clayey sand)" , "Arenisca arcillosa \n(Arena arcillosa)"  ,
     90 ,   3.5 ,   6.5 , "Clayey sandstone \n(Clayey sand)" , "Arenisca arcillosa \n(Arena arcillosa)"  ,
     50 ,  50   ,   0   , "Sandy siltstone \n(Sandy silt)"   , "Limolita arenosa \n(Limo arenoso)"       ,
     50 ,  33.4 ,  16.6 , "Sandy siltstone \n(Sandy silt)"   , "Limolita arenosa \n(Limo arenoso)"       ,
     10 ,  60   ,  30   , "Sandy siltstone \n(Sandy silt)"   , "Limolita arenosa \n(Limo arenoso)"       ,
     10 ,  90   ,   0   , "Sandy siltstone \n(Sandy silt)"   , "Limolita arenosa \n(Limo arenoso)"       ,
     50 ,  50   ,   0   , "Sandy siltstone \n(Sandy silt)"   , "Limolita arenosa \n(Limo arenoso)"       ,
     50 ,  33.4 ,  16.6 , "Sandy mudstone \n(Sandy mud)"     , "Lodolita arenosa \n(Lodo arenoso)"       ,
     50 ,  16.6 ,  33.4 , "Sandy mudstone \n(Sandy mud)"     , "Lodolita arenosa \n(Lodo arenoso)"       ,
     10 ,  30   ,  60   , "Sandy mudstone \n(Sandy mud)"     , "Lodolita arenosa \n(Lodo arenoso)"       ,
     10 ,  60   ,  30   , "Sandy mudstone \n(Sandy mud)"     , "Lodolita arenosa \n(Lodo arenoso)"       ,
     50 ,  33.4 ,  16.6 , "Sandy mudstone \n(Sandy mud)"     , "Lodolita arenosa \n(Lodo arenoso)"       ,
     50 ,  16.6 ,  33.4 , "Sandy claystone \n(Sandy clay)"   , "Arcillolita arenosa \n(Arcilla arenosa)" ,
     50 ,   0   ,  50   , "Sandy claystone \n(Sandy clay)"   , "Arcillolita arenosa \n(Arcilla arenosa)" ,
     10 ,   0   ,  90   , "Sandy claystone \n(Sandy clay)"   , "Arcillolita arenosa \n(Arcilla arenosa)" ,
     10 ,  30   ,  60   , "Sandy claystone \n(Sandy clay)"   , "Arcillolita arenosa \n(Arcilla arenosa)" ,
     50 ,  16.6 ,  33.4 , "Sandy claystone \n(Sandy clay)"   , "Arcillolita arenosa \n(Arcilla arenosa)" ,
     10 ,  90   ,   0   , "Siltstone \n(Silt)"               , "Limolita \n(Limo)"                       ,
     10 ,  60   ,  30   , "Siltstone \n(Silt)"               , "Limolita \n(Limo)"                       ,
      0 ,  66.7 ,  33.3 , "Siltstone \n(Silt)"               , "Limolita \n(Limo)"                       ,
      0 , 100   ,   0   , "Siltstone \n(Silt)"               , "Limolita \n(Limo)"                       ,
     10 ,  90   ,   0   , "Siltstone \n(Silt)"               , "Limolita \n(Limo)"                       ,
     10 ,  60   ,  30   , "Mudstone \n(Mud)"                 , "Lodolita \n(Lodo)"                       ,
     10 ,  30   ,  60   , "Mudstone \n(Mud)"                 , "Lodolita \n(Lodo)"                       ,
      0 ,  33.3 ,  66.7 , "Mudstone \n(Mud)"                 , "Lodolita \n(Lodo)"                       ,
      0 ,  66.7 ,  33.3 , "Mudstone \n(Mud)"                 , "Lodolita \n(Lodo)"                       ,
     10 ,  60   ,  30   , "Mudstone \n(Mud)"                 , "Lodolita \n(Lodo)"                       ,
     10 ,  30   ,  60   , "Claystone \n(Clay)"               , "Arcillolita \n(Arcilla)"                 ,
     10 ,   0   ,  90   , "Claystone \n(Clay)"               , "Arcillolita \n(Arcilla)"                 ,
      0 ,   0   , 100   , "Claystone \n(Clay)"               , "Arcillolita \n(Arcilla)"                 ,
      0 ,  33.3 ,  66.7 , "Claystone \n(Clay)"               , "Arcillolita \n(Arcilla)"                 ,
     10 ,  30   ,  60   , "Claystone \n(Clay)"               , "Arcillolita \n(Arcilla)"
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

  Folk.ternaryAxes.en <- list(
    aaxis = axis("Sand"),
    baxis = axis("Silt"),
    caxis = axis("Clay")
  )

  Folk.ternaryAxes.es <- list(
    aaxis = axis("Arena"),
    baxis = axis("Limo"),
    caxis = axis("Arcilla")
  )

  Folk.pal = c(
    "#FFF7EC",
    "#FEE9CC",
    "#FDD8A7",
    "#FDC38C",
    "#FCA16C",
    "#F67B51",
    "#E7533A",
    "#CF2518",
    "#AD0000",
    "#7F0000"
  )

  if (any(output == 'ggplot' & language == 'en')) {
    Folk <- ggtern::ggtern(
      data = tb.Folk,
      ggtern::aes(.data$m, .data$s, .data$c)
    ) +
      ggplot2::geom_polygon(
        ggplot2::aes(
          fill = .data$Label,
          color = .data$Label,
          group = .data$Label
        ),
        alpha = opacity
      ) +
      ggtern::theme_bw() +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_manual(values = Folk.pal) +
      ggplot2::scale_color_manual(values = Folk.pal) +
      ggplot2::labs(
        fill = "Rock (sediment)",
        color = "Rock (sediment)",
        T = "Sand",
        L = "Silt",
        R = "Clay"
      )
  } else if (any(output == 'ggplot' & language == 'es')) {
    Folk <- ggtern::ggtern(
      data = tb.Folk,
      ggtern::aes(.data$m, .data$s, .data$c)
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
      ggplot2::scale_fill_manual(values = Folk.pal) +
      ggplot2::scale_color_manual(values = Folk.pal) +
      ggplot2::labs(
        fill = "Roca (sedimento)",
        color = "Roca (sedimento)",
        T = "Arena",
        L = "Limo",
        R = "Arcilla"
      )
  } else if (any(output == 'plotly' & language == 'en')) {
    Folk = plotly::plot_ly() %>%
      plotly::add_trace(
        data = tb.Folk,
        a = ~s,
        b = ~m,
        c = ~c,
        color = ~Label,
        colors = Folk.pal %>% purrr::set_names(levels(tb.Folk$Label)),
        opacity = opacity * 2,
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        hoveron = 'fills'
      ) %>%
      plotly::layout(
        ternary = Folk.ternaryAxes.en,
        legend = list(title = list(text = '<b> Rock (sediment) </b>')),
        margin = list(autoexpand = T, t = 35)
      ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Folk_smc',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  } else if (any(output == 'plotly' & language == 'es')) {
    Folk = plotly::plot_ly() %>%
      plotly::add_trace(
        data = tb.Folk,
        a = ~s,
        b = ~m,
        c = ~c,
        color = ~Label.es,
        colors = Folk.pal %>% purrr::set_names(levels(tb.Folk$Label.es)),
        opacity = opacity * 2,
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        hoveron = 'fills'
      ) %>%
      plotly::layout(
        ternary = Folk.ternaryAxes.es,
        legend = list(title = list(text = '<b> Roca (sedimento) </b>')),
        margin = list(autoexpand = T, t = 35)
      ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Folk_smc',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  }

  return(Folk)
}
