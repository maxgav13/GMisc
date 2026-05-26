#' FAP Ternary Diagram for Plutonic and Volcanic Rocks
#' @description \code{ternary_fap()} draws either a static or interactive ternary diagram, in english or spanish. It is a base diagram where data can be plotted.
#' @param output The output format: "ggplot" or "plotly" (default is "ggplot")
#' @param language The language to be displayed: "en" for english or "es" for spanish (deafult is "en")
#' @param type The type of igneous rock: "volcanic" or "plutonic" (default is "volcanic")
#'
#' @return FAP ternary diagram for plutonic and volcanic rocks in the desired format (object)
#' @export
#' @importFrom ggplot2 .data
#'
#' @details For plotting data on the ggplot object it would be easier if the names of the dataframe are "p", "f", and "a", that way it gets mapped automatically, if not make sure to use "aes(x=p,y=f,z=a)".
#' For plotting on the plotly object the mapping of the new data should be as shown in the example: \code{a = ~f, b = ~p, c = ~a}, where \code{a} refers to the top ("f"), \code{b} refers to the bottom left ("p"), and \code{c} refers to the bottom right ("a").
#' The examples show basic usage and how to add data, which can be more customizable.
#'
#' @family classification plots, petrology
#'
#' @examples
#' library(ggplot2)
#' library(plotly)
#'
#' d = data.frame(f=c(23,40,60),
#'                p=c(27,50,30),
#'                a=c(50,10,10))
#'
#' # adding data to ggplot object
#' ternary_fap() + geom_point(data = d)
#'
#' # adding data to plotly object
#' ternary_fap('plotly') %>%
#'   add_trace(a = ~f, b = ~p, c = ~a,
#'             data = d,
#'             name = 'My data',
#'             type = "scatterternary",
#'             mode = "markers",
#'             marker = list(size=8,color='coral',
#'                           symbol=3,opacity=.9),
#'             hovertemplate = paste0('F: %{a}<br>',
#'                                    'P: %{b}<br>',
#'                                    'A: %{c}'))
#'
ternary_fap = function(
  output = c('ggplot', 'plotly'),
  language = c('en', 'es'),
  type = c('volcanic', 'plutonic')
) {
  output <- match.arg(output)
  language <- match.arg(language)
  type <- match.arg(type)

  tb.FAP = tibble::tribble(
    ~f , ~p    , ~a    , ~Label , ~Plut.en                                  , ~Plut.es                                     , ~Volc.en                                 , ~Volc.es                                      ,
     0 ,   0   , 100   , "6'"   , "Foid-bearing alakali feldspar syenite"   , "Sienita alcalifeldesp\u00e1tica con foides" , "Foid-bearing alakali feldspar trachyte" , "Traquita alcalifeldesp\u00e1tica con foides" ,
    10 ,   0   ,  90   , "6'"   , "Foid-bearing alakali feldspar syenite"   , "Sienita alcalifeldesp\u00e1tica con foides" , "Foid-bearing alakali feldspar trachyte" , "Traquita alcalifeldesp\u00e1tica con foides" ,
    10 ,   9   ,  81   , "6'"   , "Foid-bearing alakali feldspar syenite"   , "Sienita alcalifeldesp\u00e1tica con foides" , "Foid-bearing alakali feldspar trachyte" , "Traquita alcalifeldesp\u00e1tica con foides" ,
     0 ,  10   ,  90   , "6'"   , "Foid-bearing alakali feldspar syenite"   , "Sienita alcalifeldesp\u00e1tica con foides" , "Foid-bearing alakali feldspar trachyte" , "Traquita alcalifeldesp\u00e1tica con foides" ,
     0 ,   0   , 100   , "6'"   , "Foid-bearing alakali feldspar syenite"   , "Sienita alcalifeldesp\u00e1tica con foides" , "Foid-bearing alakali feldspar trachyte" , "Traquita alcalifeldesp\u00e1tica con foides" ,
     0 ,  10   ,  90   , "7'"   , "Foid-bearing syenite"                    , "Sienita con foides"                         , "Foid-bearing trachyte"                  , "Traquita con foides"                         ,
    10 ,   9   ,  81   , "7'"   , "Foid-bearing syenite"                    , "Sienita con foides"                         , "Foid-bearing trachyte"                  , "Traquita con foides"                         ,
    10 ,  31.4 ,  58.6 , "7'"   , "Foid-bearing syenite"                    , "Sienita con foides"                         , "Foid-bearing trachyte"                  , "Traquita con foides"                         ,
     0 ,  35   ,  65   , "7'"   , "Foid-bearing syenite"                    , "Sienita con foides"                         , "Foid-bearing trachyte"                  , "Traquita con foides"                         ,
     0 ,  10   ,  90   , "7'"   , "Foid-bearing syenite"                    , "Sienita con foides"                         , "Foid-bearing trachyte"                  , "Traquita con foides"                         ,
    10 ,  31.4 ,  58.6 , "8'"   , "Foid-bearing monzonite"                  , "Monzonita con foides"                       , "Foid-bearing latite"                    , "Latita con foides"                           ,
     0 ,  35   ,  65   , "8'"   , "Foid-bearing monzonite"                  , "Monzonita con foides"                       , "Foid-bearing latite"                    , "Latita con foides"                           ,
     0 ,  65   ,  35   , "8'"   , "Foid-bearing monzonite"                  , "Monzonita con foides"                       , "Foid-bearing latite"                    , "Latita con foides"                           ,
    10 ,  58.5 ,  31.5 , "8'"   , "Foid-bearing monzonite"                  , "Monzonita con foides"                       , "Foid-bearing latite"                    , "Latita con foides"                           ,
    10 ,  31.4 ,  58.6 , "8'"   , "Foid-bearing monzonite"                  , "Monzonita con foides"                       , "Foid-bearing latite"                    , "Latita con foides"                           ,
    10 ,  58.5 ,  31.5 , "9'"   , "Foid-bearing monzodiorite/monzogabbro"   , "Monzogabro/monzodiorita con foides"         , "Basalt/Andesite"                        , "Basalto/Andesita"                            ,
     0 ,  65   ,  35   , "9'"   , "Foid-bearing monzodiorite/monzogabbro"   , "Monzogabro/monzodiorita con foides"         , "Basalt/Andesite"                        , "Basalto/Andesita"                            ,
     0 ,  90   ,  10   , "9'"   , "Foid-bearing monzodiorite/monzogabbro"   , "Monzogabro/monzodiorita con foides"         , "Basalt/Andesite"                        , "Basalto/Andesita"                            ,
    10 ,  81   ,   9   , "9'"   , "Foid-bearing monzodiorite/monzogabbro"   , "Monzogabro/monzodiorita con foides"         , "Basalt/Andesite"                        , "Basalto/Andesita"                            ,
    10 ,  58.5 ,  31.5 , "9'"   , "Foid-bearing monzodiorite/monzogabbro"   , "Monzogabro/monzodiorita con foides"         , "Basalt/Andesite"                        , "Basalto/Andesita"                            ,
     0 , 100   ,   0   , "10'"  , "Foid-bearing gabbro/diorite/anorthosite" , "Gabro/diorita/anortosita con foides"        , "Basalt/Andesite"                        , "Basalto/Andesita"                            ,
    10 ,  90   ,   0   , "10'"  , "Foid-bearing gabbro/diorite/anorthosite" , "Gabro/diorita/anortosita con foides"        , "Basalt/Andesite"                        , "Basalto/Andesita"                            ,
    10 ,  81   ,   9   , "10'"  , "Foid-bearing gabbro/diorite/anorthosite" , "Gabro/diorita/anortosita con foides"        , "Basalt/Andesite"                        , "Basalto/Andesita"                            ,
     0 ,  90   ,  10   , "10'"  , "Foid-bearing gabbro/diorite/anorthosite" , "Gabro/diorita/anortosita con foides"        , "Basalt/Andesite"                        , "Basalto/Andesita"                            ,
     0 , 100   ,   0   , "10'"  , "Foid-bearing gabbro/diorite/anorthosite" , "Gabro/diorita/anortosita con foides"        , "Basalt/Andesite"                        , "Basalto/Andesita"                            ,
    10 ,   0   ,  90   , "11"   , "Foid syenite"                            , "Sienita foid\u00edtica"                     , "Phonolite"                              , "Fonolita"                                    ,
    10 ,   9   ,  81   , "11"   , "Foid syenite"                            , "Sienita foid\u00edtica"                     , "Phonolite"                              , "Fonolita"                                    ,
    60 ,   4   ,  36   , "11"   , "Foid syenite"                            , "Sienita foid\u00edtica"                     , "Phonolite"                              , "Fonolita"                                    ,
    60 ,   0   ,  40   , "11"   , "Foid syenite"                            , "Sienita foid\u00edtica"                     , "Phonolite"                              , "Fonolita"                                    ,
    10 ,   0   ,  90   , "11"   , "Foid syenite"                            , "Sienita foid\u00edtica"                     , "Phonolite"                              , "Fonolita"                                    ,
    10 ,   9   ,  81   , "12"   , "Foid monzosyenite"                       , "Monzosienita foid\u00edtica"                , "Tephritic phonolite"                    , "Fonolita tefr\u00edtica"                     ,
    10 ,  45   ,  45   , "12"   , "Foid monzosyenite"                       , "Monzosienita foid\u00edtica"                , "Tephritic phonolite"                    , "Fonolita tefr\u00edtica"                     ,
    60 ,  20   ,  20   , "12"   , "Foid monzosyenite"                       , "Monzosienita foid\u00edtica"                , "Tephritic phonolite"                    , "Fonolita tefr\u00edtica"                     ,
    60 ,   4   ,  36   , "12"   , "Foid monzosyenite"                       , "Monzosienita foid\u00edtica"                , "Tephritic phonolite"                    , "Fonolita tefr\u00edtica"                     ,
    10 ,   9   ,  81   , "12"   , "Foid monzosyenite"                       , "Monzosienita foid\u00edtica"                , "Tephritic phonolite"                    , "Fonolita tefr\u00edtica"                     ,
    10 ,  45   ,  45   , "13"   , "Foid monzodiorite/monzogabbro"           , "Monzodiorita/monzogabro foid\u00edtica"     , "Phonolitic basanite/tephrite"           , "Basanita/tefrita fonol\u00edtica"            ,
    10 ,  81   ,   9   , "13"   , "Foid monzodiorite/monzogabbro"           , "Monzodiorita/monzogabro foid\u00edtica"     , "Phonolitic basanite/tephrite"           , "Basanita/tefrita fonol\u00edtica"            ,
    60 ,  36   ,   4   , "13"   , "Foid monzodiorite/monzogabbro"           , "Monzodiorita/monzogabro foid\u00edtica"     , "Phonolitic basanite/tephrite"           , "Basanita/tefrita fonol\u00edtica"            ,
    60 ,  20   ,  20   , "13"   , "Foid monzodiorite/monzogabbro"           , "Monzodiorita/monzogabro foid\u00edtica"     , "Phonolitic basanite/tephrite"           , "Basanita/tefrita fonol\u00edtica"            ,
    10 ,  45   ,  45   , "13"   , "Foid monzodiorite/monzogabbro"           , "Monzodiorita/monzogabro foid\u00edtica"     , "Phonolitic basanite/tephrite"           , "Basanita/tefrita fonol\u00edtica"            ,
    10 ,  81   ,   9   , "14"   , "Foid diorite/gabbro"                     , "Diorita/gabro foid\u00edtica"               , "Basanite/tephrite"                      , "Basanita/tefrita"                            ,
    10 ,  90   ,   0   , "14"   , "Foid diorite/gabbro"                     , "Diorita/gabro foid\u00edtica"               , "Basanite/tephrite"                      , "Basanita/tefrita"                            ,
    60 ,  40   ,   0   , "14"   , "Foid diorite/gabbro"                     , "Diorita/gabro foid\u00edtica"               , "Basanite/tephrite"                      , "Basanita/tefrita"                            ,
    60 ,  36   ,   4   , "14"   , "Foid diorite/gabbro"                     , "Diorita/gabro foid\u00edtica"               , "Basanite/tephrite"                      , "Basanita/tefrita"                            ,
    10 ,  81   ,   9   , "14"   , "Foid diorite/gabbro"                     , "Diorita/gabro foid\u00edtica"               , "Basanite/tephrite"                      , "Basanita/tefrita"
  ) %>%
    dplyr::mutate(dplyr::across(
      .data$Label:dplyr::last_col(),
      forcats::as_factor
    ))

  Labs.FAP = data.frame(
    f = c(5, 5, 5, 5, 5, 28, 28, 28, 28),
    p = c(5, 21, 47.5, 72, 90, 3, 22, 50, 68),
    a = c(90, 74, 47.5, 23, 5, 69, 50, 22, 4),
    Label = as.factor(c("6'", "7'", "8'", "9'", "10'", "11", "12", "13", "14"))
  )

  tb.fap.plut = tibble::tribble(
    ~f  , ~p , ~a , ~Label , ~Plut.en    , ~Plut.es    ,
    100 ,  0 ,  0 , "15"   , "Foidolite" , "Foidolita" ,
     60 , 40 ,  0 , "15"   , "Foidolite" , "Foidolita" ,
     60 ,  0 , 40 , "15"   , "Foidolite" , "Foidolita" ,
    100 ,  0 ,  0 , "15"   , "Foidolite" , "Foidolita"
  )

  tb.fap.plut = dplyr::bind_rows(
    tb.FAP %>% dplyr::select(1:4, .data$Plut.en, .data$Plut.es),
    tb.fap.plut
  )

  Labs.fap.plut = data.frame(
    f = 75,
    p = 12.5,
    a = 12.5,
    Label = as.factor("15")
  )

  Labs.fap.plut = dplyr::bind_rows(
    Labs.FAP %>% dplyr::select(1:4),
    Labs.fap.plut
  )

  tb.fap.volc = tibble::tribble(
    ~f  , ~p , ~a , ~Label , ~Volc.en             , ~Volc.es                  ,
     60 ,  0 , 40 , "15a"  , "Phonolitic foidite" , "Foidita fonol\u00edtica" ,
     60 , 20 , 20 , "15a"  , "Phonolitic foidite" , "Foidita fonol\u00edtica" ,
     90 ,  5 ,  5 , "15a"  , "Phonolitic foidite" , "Foidita fonol\u00edtica" ,
     90 ,  0 , 10 , "15a"  , "Phonolitic foidite" , "Foidita fonol\u00edtica" ,
     60 ,  0 , 40 , "15a"  , "Phonolitic foidite" , "Foidita fonol\u00edtica" ,
     90 ,  5 ,  5 , "15b"  , "Tephritic foidite"  , "Foidita tefr\u00edtica"  ,
     60 , 20 , 20 , "15b"  , "Tephritic foidite"  , "Foidita tefr\u00edtica"  ,
     60 , 40 ,  0 , "15b"  , "Tephritic foidite"  , "Foidita tefr\u00edtica"  ,
     90 , 10 ,  0 , "15b"  , "Tephritic foidite"  , "Foidita tefr\u00edtica"  ,
     90 ,  5 ,  5 , "15b"  , "Tephritic foidite"  , "Foidita tefr\u00edtica"  ,
     90 , 10 ,  0 , "15c"  , "Foidite"            , "Foidita"                 ,
    100 ,  0 ,  0 , "15c"  , "Foidite"            , "Foidita"                 ,
     90 ,  0 , 10 , "15c"  , "Foidite"            , "Foidita"                 ,
     90 , 10 ,  0 , "15c"  , "Foidite"            , "Foidita"
  )

  tb.fap.volc = dplyr::bind_rows(
    tb.FAP %>% dplyr::select(1:4, .data$Volc.en, .data$Volc.es),
    tb.fap.volc
  )

  Labs.fap.volc = data.frame(
    f = c(75, 75, 94),
    p = c(7, 18, 3),
    a = c(18, 7, 3),
    Label = as.factor(c("15a", "15b", "15c"))
  )

  Labs.fap.volc = dplyr::bind_rows(
    Labs.FAP %>% dplyr::select(1:4),
    Labs.fap.volc
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

  FAP.ternaryAxes <- list(
    aaxis = axis("F"),
    baxis = axis("P"),
    caxis = axis("A")
  )

  FAP.pal = c(
    '#BC97B7',
    '#AD99AA',
    '#998D9D',
    '#897F8A',
    '#746C74',
    '#91ABD5',
    '#98A8C5',
    '#9197A3',
    '#6E6F73',
    '#5489C5',
    '#5489C5',
    '#3364A2'
  )

  if (type == 'plutonic' & output == 'ggplot') {
    FAP <- ggtern::ggtern(
      data = tb.fap.plut,
      ggtern::aes(.data$p, .data$f, .data$a)
    ) +
      ggplot2::geom_polygon(
        ggplot2::aes(group = .data$Label),
        fill = 'white',
        color = "black",
        alpha = .05
      ) +
      ggplot2::geom_text(
        data = Labs.fap.plut,
        ggtern::aes(label = .data$Label),
        size = 2.5,
        color = "black",
        show.legend = T,
        position = "identity"
      ) +
      ggplot2::theme_bw() +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggtern::theme_rotate(180) +
      # custom_percent("Percent") +
      # ggplot2::scale_fill_manual(values = FAP.pal) +
      # ggplot2::scale_color_manual(values = FAP.pal) +
      ggplot2::labs(T = "F", L = "P", R = "A")
  } else if (type == 'volcanic' & output == 'ggplot') {
    FAP <- ggtern::ggtern(
      data = tb.fap.volc,
      ggtern::aes(.data$p, .data$f, .data$a)
    ) +
      ggplot2::geom_polygon(
        ggplot2::aes(group = .data$Label),
        fill = 'white',
        color = "black",
        alpha = .05
      ) +
      ggplot2::geom_text(
        data = Labs.fap.volc,
        ggtern::aes(label = .data$Label),
        size = 2.5,
        color = "black",
        show.legend = T,
        position = "identity"
      ) +
      ggplot2::theme_bw() +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggtern::theme_rotate(180) +
      ggplot2::labs(T = "F", L = "P", R = "A")
  } else if (type == 'plutonic' & output == 'plotly' & language == 'en') {
    FAP = plotly::plot_ly() %>%
      plotly::add_trace(
        data = tb.fap.plut,
        a = ~f,
        b = ~p,
        c = ~a,
        color = ~Label,
        colors = 'transparent',
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        line = list(color = "black", width = 0.5),
        hoverinfo = 'text',
        text = ~ paste('</br>', Label, '</br>', Plut.en),
        hoveron = 'fills'
      ) %>%
      plotly::add_trace(
        data = Labs.fap.plut,
        a = ~f,
        b = ~p,
        c = ~a,
        text = ~Label,
        type = "scatterternary",
        mode = "text",
        name = 'ID',
        hoverinfo = 'none',
        textfont = list(family = "Arial", size = 12, color = "black"),
        showlegend = T
      ) %>%
      plotly::layout(
        ternary = FAP.ternaryAxes
      ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'FAP',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  } else if (type == 'plutonic' & output == 'plotly' & language == 'es') {
    FAP = plotly::plot_ly() %>%
      plotly::add_trace(
        data = tb.fap.plut,
        a = ~f,
        b = ~p,
        c = ~a,
        color = ~Label,
        colors = 'transparent',
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        line = list(color = "black", width = 0.5),
        hoverinfo = 'text',
        text = ~ paste('</br>', Label, '</br>', Plut.es),
        hoveron = 'fills'
      ) %>%
      plotly::add_trace(
        data = Labs.fap.plut,
        a = ~f,
        b = ~p,
        c = ~a,
        text = ~Label,
        type = "scatterternary",
        mode = "text",
        name = 'ID',
        hoverinfo = 'none',
        textfont = list(family = "Arial", size = 12, color = "black"),
        showlegend = T
      ) %>%
      plotly::layout(
        ternary = FAP.ternaryAxes
      ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'FAP',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  } else if (type == 'volcanic' & output == 'plotly' & language == 'en') {
    FAP = plotly::plot_ly() %>%
      plotly::add_trace(
        data = tb.fap.volc,
        a = ~f,
        b = ~p,
        c = ~a,
        color = ~Label,
        colors = 'transparent',
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        line = list(color = "black", width = 0.5),
        hoverinfo = 'text',
        text = ~ paste('</br>', Label, '</br>', Volc.en),
        hoveron = 'fills'
      ) %>%
      plotly::add_trace(
        data = Labs.fap.volc,
        a = ~f,
        b = ~p,
        c = ~a,
        text = ~Label,
        type = "scatterternary",
        mode = "text",
        name = 'ID',
        hoverinfo = 'none',
        textfont = list(family = "Arial", size = 12, color = "black"),
        showlegend = T
      ) %>%
      plotly::layout(
        ternary = FAP.ternaryAxes,
        margin = list(autoexpand = T, t = 35)
      ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'FAP',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  } else if (type == 'volcanic' & output == 'plotly' & language == 'es') {
    FAP = plotly::plot_ly() %>%
      plotly::add_trace(
        data = tb.fap.volc,
        a = ~f,
        b = ~p,
        c = ~a,
        color = ~Label,
        colors = 'transparent',
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        line = list(color = "black", width = 0.5),
        hoverinfo = 'text',
        text = ~ paste('</br>', Label, '</br>', Volc.es),
        hoveron = 'fills'
      ) %>%
      plotly::add_trace(
        data = Labs.fap.volc,
        a = ~f,
        b = ~p,
        c = ~a,
        text = ~Label,
        type = "scatterternary",
        mode = "text",
        name = 'ID',
        hoverinfo = 'none',
        textfont = list(family = "Arial", size = 12, color = "black"),
        showlegend = T
      ) %>%
      plotly::layout(
        ternary = FAP.ternaryAxes,
        margin = list(autoexpand = T, t = 35)
      ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'FAP',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  }

  return(FAP)
}
