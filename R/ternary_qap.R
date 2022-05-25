#' QAP Ternary Diagram for Plutonic and Volcanic Rocks
#' @description \code{ternary_qap()} draws either a static or interactive ternary diagram, in english or spanish. It is a base diagram where data can be plotted.
#' @param output The output format: "ggplot" or "plotly" (default is "ggplot")
#' @param language The language to be displayed: "en" for english or "es" for spanish (deafult is "en")
#' @param type The type of volcanic rock: "plutonic" or "volcanic" (default is "plutonic")
#'
#' @return QAP ternary diagram for plutonic and volcanic rocks in the desired format (object)
#' @export
#' @importFrom ggplot2 .data
#'
#' @details For plotting data on the ggplot object it would be easier if the names of the dataframe are "a", "q", and "p", that way it gets mapped automatically, if not make sure to use "aes(x=a,y=q,z=p)".
#' For plotting on the plotly object the mapping of the new data should be as shown in the example: \code{a = ~q, b = ~a, c = ~p}, where \code{a} refers to the top ("q"), \code{b} refers to the bottom left ("a"), and \code{c} refers to the bottom right ("p").
#' The examples show basic usage and how to add data, which can be more customizable.
#'
#' @examples
#' library(ggplot2)
#' library(plotly)
#'
#' d = data.frame(q=c(23,26.9,25.3),
#'                a=c(27,23.7,5.1),
#'                p=c(50,49.4,59.6))
#'
#' # adding data to ggplot object
#' ternary_qap() + geom_point(data = d)
#'
#' # adding data to plotly object
#' ternary_qap('plotly') %>%
#'   add_trace(a = ~q, b = ~a, c = ~p,
#'             data = d,
#'             name = 'My data',
#'             type = "scatterternary",
#'             mode = "markers",
#'             marker = list(size=8,color='coral',
#'                           symbol=3,opacity=.9),
#'             hovertemplate = paste0('Q: %{a}<br>',
#'                                    'A: %{b}<br>',
#'                                    'P: %{c}'))
#'
ternary_qap = function(output = c('ggplot','plotly'),
                       language = c('en','es'),
                       type = c('plutonic','volcanic')) {

  tb.QAP = tibble::tribble(
    ~q,   ~a,   ~p, ~Label,                                              ~Plut.en,                                                  ~Plut.es,                                        ~Volc.en,                                       ~Volc.es,
    100,    0,    0,   "1a",                                         "Quartzolite",                                              "Cuarzolita",                                              NA,                                             NA,
    90,   10,    0,   "1a",                                         "Quartzolite",                                              "Cuarzolita",                                              NA,                                             NA,
    90,    0,   10,   "1a",                                         "Quartzolite",                                              "Cuarzolita",                                              NA,                                             NA,
    100,    0,    0,   "1a",                                         "Quartzolite",                                              "Cuarzolita",                                              NA,                                             NA,
    90,   10,    0,   "1b",                               "Quartz-rich granitoid",                               "Granitoide rico en cuarzo",                                              NA,                                             NA,
    60,   40,    0,   "1b",                               "Quartz-rich granitoid",                               "Granitoide rico en cuarzo",                                              NA,                                             NA,
    60,    0,   40,   "1b",                               "Quartz-rich granitoid",                               "Granitoide rico en cuarzo",                                              NA,                                             NA,
    90,    0,   10,   "1b",                               "Quartz-rich granitoid",                               "Granitoide rico en cuarzo",                                              NA,                                             NA,
    90,   10,    0,   "1b",                               "Quartz-rich granitoid",                               "Granitoide rico en cuarzo",                                              NA,                                             NA,
    60,   40,    0,    "2",                             "Alkali feldspar granite",                              "Granito alcalifeldesp\u00e1tico",                      "Alkali feldspar rhyolite",                   "Riolita alcalifeldesp\u00e1tica",
    20,   80,    0,    "2",                             "Alkali feldspar granite",                              "Granito alcalifeldesp\u00e1tico",                      "Alkali feldspar rhyolite",                   "Riolita alcalifeldesp\u00e1tica",
    20,   72,    8,    "2",                             "Alkali feldspar granite",                              "Granito alcalifeldesp\u00e1tico",                      "Alkali feldspar rhyolite",                   "Riolita alcalifeldesp\u00e1tica",
    60,   36,    4,    "2",                             "Alkali feldspar granite",                              "Granito alcalifeldesp\u00e1tico",                      "Alkali feldspar rhyolite",                   "Riolita alcalifeldesp\u00e1tica",
    60,   40,    0,    "2",                             "Alkali feldspar granite",                              "Granito alcalifeldesp\u00e1tico",                      "Alkali feldspar rhyolite",                   "Riolita alcalifeldesp\u00e1tica",
    60,   36,    4,   "3a",                                       "Syeno-granite",                                           "Sieno-granito",                                      "Rhyolite",                           "Riolita (Liparita)",
    20,   72,    8,   "3a",                                       "Syeno-granite",                                           "Sieno-granito",                                      "Rhyolite",                           "Riolita (Liparita)",
    20,   52,   28,   "3a",                                       "Syeno-granite",                                           "Sieno-granito",                                      "Rhyolite",                           "Riolita (Liparita)",
    60,   26,   14,   "3a",                                       "Syeno-granite",                                           "Sieno-granito",                                      "Rhyolite",                           "Riolita (Liparita)",
    60,   36,    4,   "3a",                                       "Syeno-granite",                                           "Sieno-granito",                                      "Rhyolite",                           "Riolita (Liparita)",
    60,   26,   14,   "3b",                                       "Monzo-granite",                                           "Monzo-granito",                                    "Rhyodacite",                                    "Riodacita",
    20,   52,   28,   "3b",                                       "Monzo-granite",                                           "Monzo-granito",                                    "Rhyodacite",                                    "Riodacita",
    20,   28,   52,   "3b",                                       "Monzo-granite",                                           "Monzo-granito",                                    "Rhyodacite",                                    "Riodacita",
    60,   14,   26,   "3b",                                       "Monzo-granite",                                           "Monzo-granito",                                    "Rhyodacite",                                    "Riodacita",
    60,   26,   14,   "3b",                                       "Monzo-granite",                                           "Monzo-granito",                                    "Rhyodacite",                                    "Riodacita",
    60,   14,   26,    "4",                                        "Granodiorite",                                            "Granodiorita",                                        "Dacite",                                       "Dacita",
    20,   28,   52,    "4",                                        "Granodiorite",                                            "Granodiorita",                                        "Dacite",                                       "Dacita",
    20,    8,   72,    "4",                                        "Granodiorite",                                            "Granodiorita",                                        "Dacite",                                       "Dacita",
    60,    4,   36,    "4",                                        "Granodiorite",                                            "Granodiorita",                                        "Dacite",                                       "Dacita",
    60,   14,   26,    "4",                                        "Granodiorite",                                            "Granodiorita",                                        "Dacite",                                       "Dacita",
    60,    4,   36,    "5",                                            "Tonalite",                                                "Tonalita",                                 "Plagio-dacite",                                  "Plagidacita",
    20,    8,   72,    "5",                                            "Tonalite",                                                "Tonalita",                                 "Plagio-dacite",                                  "Plagidacita",
    20,    0,   80,    "5",                                            "Tonalite",                                                "Tonalita",                                 "Plagio-dacite",                                  "Plagidacita",
    60,    0,   40,    "5",                                            "Tonalite",                                                "Tonalita",                                 "Plagio-dacite",                                  "Plagidacita",
    60,    4,   36,    "5",                                            "Tonalite",                                                "Tonalita",                                 "Plagio-dacite",                                  "Plagidacita",
    20,   80,    0,   "6*",                      "Quartz alkali feldspar syenite",                     "Sienita alcalifeldesp\u00e1tica cuarzosa",               "Quartz alkali feldspar trachyte",           "Traquita cuarzo-alcalifeldesp\u00e1tica",
    5,   95,    0,   "6*",                      "Quartz alkali feldspar syenite",                     "Sienita alcalifeldesp\u00e1tica cuarzosa",               "Quartz alkali feldspar trachyte",           "Traquita cuarzo-alcalifeldesp\u00e1tica",
    5, 85.5,  9.5,   "6*",                      "Quartz alkali feldspar syenite",                     "Sienita alcalifeldesp\u00e1tica cuarzosa",               "Quartz alkali feldspar trachyte",           "Traquita cuarzo-alcalifeldesp\u00e1tica",
    20,   72,    8,   "6*",                      "Quartz alkali feldspar syenite",                     "Sienita alcalifeldesp\u00e1tica cuarzosa",               "Quartz alkali feldspar trachyte",           "Traquita cuarzo-alcalifeldesp\u00e1tica",
    20,   80,    0,   "6*",                      "Quartz alkali feldspar syenite",                     "Sienita alcalifeldesp\u00e1tica cuarzosa",               "Quartz alkali feldspar trachyte",           "Traquita cuarzo-alcalifeldesp\u00e1tica",
    20,   72,    8,   "7*",                                      "Quartz syenite",                                        "Sienita cuarzosa",                               "Quartz trachyte",                            "Traquita cuarzosa",
    5, 85.5,  9.5,   "7*",                                      "Quartz syenite",                                        "Sienita cuarzosa",                               "Quartz trachyte",                            "Traquita cuarzosa",
    5,   62,   33,   "7*",                                      "Quartz syenite",                                        "Sienita cuarzosa",                               "Quartz trachyte",                            "Traquita cuarzosa",
    20,   52,   28,   "7*",                                      "Quartz syenite",                                        "Sienita cuarzosa",                               "Quartz trachyte",                            "Traquita cuarzosa",
    20,   72,    8,   "7*",                                      "Quartz syenite",                                        "Sienita cuarzosa",                               "Quartz trachyte",                            "Traquita cuarzosa",
    20,   52,   28,   "8*",                                    "Quartz monzonite",                                      "Monzonita cuarzosa",                                 "Quartz latite",                              "Latita cuarzosa",
    5,   62,   33,   "8*",                                    "Quartz monzonite",                                      "Monzonita cuarzosa",                                 "Quartz latite",                              "Latita cuarzosa",
    5,   33,   62,   "8*",                                    "Quartz monzonite",                                      "Monzonita cuarzosa",                                 "Quartz latite",                              "Latita cuarzosa",
    20,   28,   52,   "8*",                                    "Quartz monzonite",                                      "Monzonita cuarzosa",                                 "Quartz latite",                              "Latita cuarzosa",
    20,   52,   28,   "8*",                                    "Quartz monzonite",                                      "Monzonita cuarzosa",                                 "Quartz latite",                              "Latita cuarzosa",
    20,   28,   52,   "9*",            "Quartz monzodiorite / Quartz monzogabbro",             "Monzodiorita cuarzosa / Monzogabro cuarzoso", "Quartz latite andesite / Quartz latite basalt", "Latiandesita cuarzosa / Latibasalto cuarzoso",
    5,   33,   62,   "9*",            "Quartz monzodiorite / Quartz monzogabbro",             "Monzodiorita cuarzosa / Monzogabro cuarzoso", "Quartz latite andesite / Quartz latite basalt", "Latiandesita cuarzosa / Latibasalto cuarzoso",
    5,  9.5, 85.5,   "9*",            "Quartz monzodiorite / Quartz monzogabbro",             "Monzodiorita cuarzosa / Monzogabro cuarzoso", "Quartz latite andesite / Quartz latite basalt", "Latiandesita cuarzosa / Latibasalto cuarzoso",
    20,    8,   72,   "9*",            "Quartz monzodiorite / Quartz monzogabbro",             "Monzodiorita cuarzosa / Monzogabro cuarzoso", "Quartz latite andesite / Quartz latite basalt", "Latiandesita cuarzosa / Latibasalto cuarzoso",
    20,   28,   52,   "9*",            "Quartz monzodiorite / Quartz monzogabbro",             "Monzodiorita cuarzosa / Monzogabro cuarzoso", "Quartz latite andesite / Quartz latite basalt", "Latiandesita cuarzosa / Latibasalto cuarzoso",
    20,    8,   72,  "10*", "Quartz diorite / Quartz gabbro / Quartz anorthosite", "Diorita cuarzosa / Gabro cuarzoso / Anortosita cuarzosa",           "Quartz andesite / Tholeiitic basalt",        "Andesita cuarzosa / Basalto tole\u00edtico",
    5,  9.5, 85.5,  "10*", "Quartz diorite / Quartz gabbro / Quartz anorthosite", "Diorita cuarzosa / Gabro cuarzoso / Anortosita cuarzosa",           "Quartz andesite / Tholeiitic basalt",        "Andesita cuarzosa / Basalto tole\u00edtico",
    5,    0,   95,  "10*", "Quartz diorite / Quartz gabbro / Quartz anorthosite", "Diorita cuarzosa / Gabro cuarzoso / Anortosita cuarzosa",           "Quartz andesite / Tholeiitic basalt",        "Andesita cuarzosa / Basalto tole\u00edtico",
    20,    0,   80,  "10*", "Quartz diorite / Quartz gabbro / Quartz anorthosite", "Diorita cuarzosa / Gabro cuarzoso / Anortosita cuarzosa",           "Quartz andesite / Tholeiitic basalt",        "Andesita cuarzosa / Basalto tole\u00edtico",
    20,    8,   72,  "10*", "Quartz diorite / Quartz gabbro / Quartz anorthosite", "Diorita cuarzosa / Gabro cuarzoso / Anortosita cuarzosa",           "Quartz andesite / Tholeiitic basalt",        "Andesita cuarzosa / Basalto tole\u00edtico",
    5,   95,    0,    "6",                             "Alkali feldspar syenite",                              "Sienita alcalifeldesp\u00e1tica",                      "Alkali feldspar trachyte",                  "Traquita alcalifeldesp\u00e1tica",
    0,  100,    0,    "6",                             "Alkali feldspar syenite",                              "Sienita alcalifeldesp\u00e1tica",                      "Alkali feldspar trachyte",                  "Traquita alcalifeldesp\u00e1tica",
    0,   90,   10,    "6",                             "Alkali feldspar syenite",                              "Sienita alcalifeldesp\u00e1tica",                      "Alkali feldspar trachyte",                  "Traquita alcalifeldesp\u00e1tica",
    5, 85.5,  9.5,    "6",                             "Alkali feldspar syenite",                              "Sienita alcalifeldesp\u00e1tica",                      "Alkali feldspar trachyte",                  "Traquita alcalifeldesp\u00e1tica",
    5,   95,    0,    "6",                             "Alkali feldspar syenite",                              "Sienita alcalifeldesp\u00e1tica",                      "Alkali feldspar trachyte",                  "Traquita alcalifeldesp\u00e1tica",
    5, 85.5,  9.5,    "7",                                             "Syenite",                                                 "Sienita",                                      "Trachyte",                                     "Traquita",
    0,   90,   10,    "7",                                             "Syenite",                                                 "Sienita",                                      "Trachyte",                                     "Traquita",
    0,   65,   35,    "7",                                             "Syenite",                                                 "Sienita",                                      "Trachyte",                                     "Traquita",
    5,   62,   33,    "7",                                             "Syenite",                                                 "Sienita",                                      "Trachyte",                                     "Traquita",
    5, 85.5,  9.5,    "7",                                             "Syenite",                                                 "Sienita",                                      "Trachyte",                                     "Traquita",
    5,   62,   33,    "8",                                           "Monzonite",                                               "Monzonita",                                        "Latite",                                       "Latita",
    0,   65,   35,    "8",                                           "Monzonite",                                               "Monzonita",                                        "Latite",                                       "Latita",
    0,   35,   65,    "8",                                           "Monzonite",                                               "Monzonita",                                        "Latite",                                       "Latita",
    5,   33,   62,    "8",                                           "Monzonite",                                               "Monzonita",                                        "Latite",                                       "Latita",
    5,   62,   33,    "8",                                           "Monzonite",                                               "Monzonita",                                        "Latite",                                       "Latita",
    5,   33,   62,    "9",                         "Monzodiorite / Monzograbbro",                              "Monzodiorita / Monzograbro",               "Latite andesite / Latite basalt",                   "Latiandesita / Latibasalto",
    0,   35,   65,    "9",                         "Monzodiorite / Monzograbbro",                              "Monzodiorita / Monzograbro",               "Latite andesite / Latite basalt",                   "Latiandesita / Latibasalto",
    0,   10,   90,    "9",                         "Monzodiorite / Monzograbbro",                              "Monzodiorita / Monzograbro",               "Latite andesite / Latite basalt",                   "Latiandesita / Latibasalto",
    5,  9.5, 85.5,    "9",                         "Monzodiorite / Monzograbbro",                              "Monzodiorita / Monzograbro",               "Latite andesite / Latite basalt",                   "Latiandesita / Latibasalto",
    5,   33,   62,    "9",                         "Monzodiorite / Monzograbbro",                              "Monzodiorita / Monzograbro",               "Latite andesite / Latite basalt",                   "Latiandesita / Latibasalto",
    5,  9.5, 85.5,   "10",                      "Diorite / Gabbro / Anorthosite",                            "Diorita / Gabro / Anortosita",                             "Andesite / Basalt",                           "Andesita / Basalto",
    0,   10,   90,   "10",                      "Diorite / Gabbro / Anorthosite",                            "Diorita / Gabro / Anortosita",                             "Andesite / Basalt",                           "Andesita / Basalto",
    0,    0,  100,   "10",                      "Diorite / Gabbro / Anorthosite",                            "Diorita / Gabro / Anortosita",                             "Andesite / Basalt",                           "Andesita / Basalto",
    5,    0,   95,   "10",                      "Diorite / Gabbro / Anorthosite",                            "Diorita / Gabro / Anortosita",                             "Andesite / Basalt",                           "Andesita / Basalto",
    5,  9.5, 85.5,   "10",                      "Diorite / Gabbro / Anorthosite",                            "Diorita / Gabro / Anortosita",                             "Andesite / Basalt",                           "Andesita / Basalto"
  ) %>%
    dplyr::mutate(forcats::as_factor(.data$Label))

  Labs.QAP = data.frame(
    q = c(93.3333333333333,75,40,40,40,40,40,
          12.5,12.5,12.5,12.5,12.5,2.5,2.5,2.5,2.5,2.5),
    a = c(3.33333333333333,12.5,57,46.5,30,13.5,
          3,83.125,67.875,43.75,19.625,4.375,92.625,75.625,48.75,
          21.875,4.875),
    p = c(3.33333333333333,12.5,3,13.5,30,46.5,
          57,4.375,19.625,43.75,67.875,83.125,4.875,21.875,48.75,
          75.625,92.625),
    Label = as.factor(c("1a","1b","2","3a",
                        "3b","4","5","6*","7*","8*","9*","10*","6",
                        "7","8","9","10"))
  )

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

  QAP.ternaryAxes <- list(
    aaxis = axis("Q"),
    baxis = axis("A"),
    caxis = axis("P")
  )

  if (any(output == 'ggplot')) {
    QAP <- ggtern::ggtern(data=tb.QAP,
                          ggtern::aes(.data$a,.data$q,.data$p)) +
      ggplot2::geom_polygon(fill='white',aes(group=.data$Label),
                            color="black",alpha=0.05) +
      ggplot2::geom_text(data=Labs.QAP,aes(label=.data$Label),size=2.5,color="black") +
      ggplot2::theme_bw() +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      # custom_percent("Percent") +
      ggplot2::labs(title="QAP",
                    T="Q",
                    L="A",
                    R="P")
  } else if (any(type == 'plutonic' & output == 'plotly' & language == 'en')) {
    QAP = plotly::plot_ly() %>%
      plotly::add_trace(
        data = tb.QAP,
        a = ~q, b = ~a, c = ~p,
        color = ~Label,
        colors = 'transparent',
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        line = list(color = "black"),
        hoverinfo = 'text',
        text = ~paste('</br>', Label,
                      '</br>', Plut.en),
        hoveron = 'fills'
      ) %>%
      plotly::add_trace(
        data = Labs.QAP,
        a = ~q, b = ~a, c = ~p,
        text = ~Label,
        type = "scatterternary",
        mode = "text",
        name = 'ID',
        hoverinfo = 'none',
        textfont = list(family= "Arial", size= 12, color= "Black"),
        showlegend = T
      ) %>%
      plotly::layout(
        annotations = label("QAP"), ternary = QAP.ternaryAxes
      ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'QAP',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  } else if (any(type == 'plutonic' & output == 'plotly' & language == 'es')) {
    QAP = plotly::plot_ly() %>%
      plotly::add_trace(
        data = tb.QAP,
        a = ~q, b = ~a, c = ~p,
        color = ~Label,
        colors = 'transparent',
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        line = list(color = "black"),
        hoverinfo = 'text',
        text = ~paste('</br>', Label,
                      '</br>', Plut.es),
        hoveron = 'fills'
      ) %>%
      plotly::add_trace(
        data = Labs.QAP,
        a = ~q, b = ~a, c = ~p,
        text = ~Label,
        type = "scatterternary",
        mode = "text",
        name = 'ID',
        hoverinfo = 'none',
        textfont = list(family= "Arial", size= 12, color= "Black"),
        showlegend = T
      ) %>%
      plotly::layout(
        annotations = label("QAP"), ternary = QAP.ternaryAxes
      ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'QAP',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  } else if (any(type == 'volcanic' & output == 'plotly' & language == 'en')) {
    QAP = plotly::plot_ly() %>%
      plotly::add_trace(
        data = tb.QAP,
        a = ~q, b = ~a, c = ~p,
        color = ~Label,
        colors = 'transparent',
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        line = list(color = "black"),
        hoverinfo = 'text',
        text = ~paste('</br>', Label,
                      '</br>', Volc.en),
        hoveron = 'fills'
      ) %>%
      plotly::add_trace(
        data = Labs.QAP,
        a = ~q, b = ~a, c = ~p,
        text = ~Label,
        type = "scatterternary",
        mode = "text",
        name = 'ID',
        hoverinfo = 'none',
        textfont = list(family= "Arial", size= 12, color= "Black"),
        showlegend = T
      ) %>%
      plotly::layout(
        annotations = label("QAP"), ternary = QAP.ternaryAxes
      ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'QAP',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  } else if (any(type == 'volcanic' & output == 'plotly' & language == 'es')) {
    QAP = plotly::plot_ly() %>%
      plotly::add_trace(
        data = tb.QAP,
        a = ~q, b = ~a, c = ~p,
        color = ~Label,
        colors = 'transparent',
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        line = list(color = "black"),
        hoverinfo = 'text',
        text = ~paste('</br>', Label,
                      '</br>', Volc.es),
        hoveron = 'fills'
      ) %>%
      plotly::add_trace(
        data = Labs.QAP,
        a = ~q, b = ~a, c = ~p,
        text = ~Label,
        type = "scatterternary",
        mode = "text",
        name = 'ID',
        hoverinfo = 'none',
        textfont = list(family= "Arial", size= 12, color= "Black"),
        showlegend = T
      ) %>%
      plotly::layout(
        annotations = label("QAP"), ternary = QAP.ternaryAxes
      ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'QAP',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  }

  return(QAP)

}
