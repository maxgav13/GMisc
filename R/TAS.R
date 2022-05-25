#' TAS Diagram
#' @description \code{TAS()} draws either a static or interactive diagram, in english or spanish. It is a base diagram where data can be plotted.
#' @param output The output format: "ggplot" or "plotly" (default is "ggplot")
#' @param language The language to be displayed: "en" for english or "es" for spanish (deafult is "en")
#'
#' @return TAS diagram in the desired format (object)
#' @export
#'
#' @importFrom ggplot2 .data
#' @details The examples show basic usage and how to add data, which can be more customizable. In general, just map the silica content to the x-axis and alkali content to the y-axis.
#'
#' @examples
#' library(ggplot2)
#' library(plotly)
#'
#' d = data.frame(silica=c(58,50,70), alkali=c(8,4,5))
#'
#' TAS()
#' TAS('plotly')
#'
#' # adding data to ggplot object
#' TAS(language = 'es') +
#'   geom_point(aes(x=silica, y=alkali), data=d)
#'
#' # adding data to plotly object
#' TAS('plotly') %>%
#'   add_markers(x=~silica, y=~alkali,
#'               data = d,
#'               name = "My data",
#'               marker = list(size=8,color='orange',
#'                             symbol=3,opacity=.9)) %>%
#'   layout(showlegend = TRUE)
#'
TAS = function(output = c('ggplot','plotly'),
               language = c('en','es')) {

  clssf=list(zona = as.character(1:15),
             name=c("Picrobasalt","Basalt",
                    "Basaltic\u000aandesite","Andesite","Dacite",
                    "Rhyolite","Trachy-\u000abasalt",
                    "Basaltic\u000atrachy-\u000aandesite","Trachy-\u000aandesite",
                    "Trachyte\u000aTrachydacite","Tephrite\u000aBasanite",
                    "Phono-\u000atephrite","Tephri-\u000aphonolite",
                    "Phonolite","Foidite"))
  lines1=list(x=c(37,41,41,45,48.4,52.5,50.35,37),y=c(0,0,7,9.4,11.5,14,15,15))
  lines2=list(x=c(41,45,45,41),y=c(0,0,3,3))
  lines3=list(x=c(45,52,52,45),y=c(0,0,5,5))
  lines4=list(x=c(52,57,57,52),y=c(0,0,5.9,5))
  lines5=list(x=c(57,63,63,57),y=c(0,0,7,5.9))
  lines6=list(x=c(63,77,69,63),y=c(0,0,8,7))
  lines7=list(x=c(77,80,80,69,69),y=c(0,0,15,15,8))
  lines8=list(x=c(45,52,49.4),y=c(5,5,7.3))
  lines9=list(x=c(52,57,53,49.4),y=c(5,5.9,9.3,7.3))
  lines10=list(x=c(57,63,57.6,53),y=c(5.9,7,11.7,9.3))
  lines11=list(x=c(63,69,69,63.75,57.6),y=c(7,8,15,15,11.7))
  lines12=list(x=c(41,45,45,49.4,45,41),y=c(3,3,5,7.3,9.4,7))
  lines13=list(x=c(49.4,53,48.4,45),y=c(7.3,9.3,11.5,9.4))
  lines14=list(x=c(53,57.6,52.5,48.4),y=c(9.3,11.7,14,11.5))
  lines15=list(x=c(57.6,63.75,50.35),y=c(11.7,15,15))

  text1=list(x=43,y=1.55,text="Picrobasalt")
  text2=list(x=48.5,y=2.8,text="Basalt")
  text3=list(x=54.8,y=3,text="Basaltic\u000aandesite")
  text4=list(x=59.9,y=3,text="Andesite")
  text5=list(x=67,y=3,text="Dacite")
  text6=list(x=75,y=8.3,text="Rhyolite")
  text7=list(x=63.5,y=11.2,text="Trachyte\u000aTrachydacite")
  text8=list(x=57.8,y=8.5,text="Trachy-\u000aandesite")
  text9=list(x=52.95,y=7,text="Basaltic\u000atrachy-\u000aandesite")
  text10=list(x=49.2,y=5.65,text="Trachy-\u000abasalt")
  text11=list(x=45,y=7,text="Tephrite\u000aBasanite")
  text12=list(x=49.2,y=9.3,text="Phono-\u000atephrite")
  text13=list(x=53,y=11.5,text="Tephri-\u000aphonolite")
  text14=list(x=57,y=14,text="Phonolite")
  text15=list(x=43,y=12,text="Foidite")

  TAS.labels = dplyr::bind_rows(text1,text2,text3,text4,text5,
                                text6,text7,text8,text9,text10,
                                text11,text12,text13,text14,text15,
                                .id = 'zona') %>%
    dplyr::rename(name = .data$text) %>%
    dplyr::mutate(name.es = c('Basalto\u000apicr\u00edtico','Basalto','Andesita\u000abas\u00e1ltica',
                              'Andesita','Dacita','Riolita',
                              'Traquita\u000aTraquidacita','Traqui-\u000aandesita',
                              'Traqui-\u000aandesita\u000abas\u00e1ltica','Traqui-\u000abasalto',
                              'Tefrita\u000aBasanita','Tefrita\u000afonol\u00edtica',
                              'Fonolita\u000atefr\u00edtica','Fonolita','Foidita'))

  tb.TAS = dplyr::bind_rows(lines2,lines3,lines4,lines5,
                            lines6,lines7,lines8,lines9,lines10,
                            lines11,lines12,lines13,lines14,
                            lines15,lines1,
                            .id = 'zona') %>%
    dplyr::left_join(clssf %>% dplyr::as_tibble(), by = 'zona') %>%
    dplyr::left_join(TAS.labels %>% dplyr::select(-c(.data$x,.data$y,.data$zona)),
                     by = c('name')) %>%
    dplyr::mutate(name = forcats::as_factor(.data$name),
                  name.es = forcats::as_factor(.data$name.es))

  TAS.line = tibble::tibble(x=c(39.2,40,43.2,45,48,50,53.7,55,60,65,77.4),
                            y=c(0,0.4,2,2.8,4,4.75,6,6.4,8,8.8,10))

  TAS_base.gg = ggplot(tb.TAS,aes(.data$x,.data$y)) +
    scale_x_continuous(breaks = seq(37,79,4),expand = c(0,0)) +
    scale_y_continuous(breaks = 1:15,expand = c(0,0)) +
    theme_classic() +
    coord_fixed(ratio = 1.5)

  if (any(output == 'ggplot' & language == 'en')) {
    TAS = TAS_base.gg +
      geom_polygon(aes(group=.data$name),fill='white',col='black') +
      geom_line(data = TAS.line, col='darkred') +
      geom_text(aes(label = .data$name),data = TAS.labels,size=2) +
      annotate('text', label = 'Alkaline',
               x=73, y=13, col='darkred',size=3) +
      annotate('text', label = 'Subalkaline\u000a/Tholeiitic',
               x=76, y=5, col='darkred',size=3) +
      labs(x = expression(paste(SiO[2], ' [wt %]')),
           y = expression(paste(Na[2],'O', ' + ', K[2],'O', ' [wt %]')))

  } else if (any(output == 'ggplot' & language == 'es')) {
    TAS = TAS_base.gg +
      geom_polygon(aes(group=.data$name),fill='white',col='black') +
      geom_line(data = TAS.line, col='darkred') +
      geom_text(aes(label = .data$name.es),data = TAS.labels,size=2) +
      annotate('text', label = 'Alcalina',
               x=73, y=13, col='darkred',size=3) +
      annotate('text', label = 'Subalcalina\u000a/Tole\u00edtica',
               x=76, y=5, col='darkred',size=3) +
      labs(x = expression(paste(SiO[2], ' [% peso]')),
           y = expression(paste(Na[2],'O', ' + ', K[2],'O', ' [% peso]')))

  } else if (any(output == 'plotly' & language == 'en')) {
    TAS = plotly::plot_ly() %>%
      plotly::add_polygons(
        data = tb.TAS,
        x=~x,y=~y,
        color=~name,
        colors='transparent',
        mode='lines',
        line = list(color = "black"),
        hoverinfo='text',
        showlegend=F) %>%
      plotly::add_lines(
        data = TAS.line,
        x=~x,y=~y,
        name='Series',
        hoverinfo = 'none',
        line = list(color = "darkred"),
        showlegend=F) %>%
      plotly::add_text(
        data = TAS.labels,
        x=~x,y=~y,
        text=~name,
        name = 'Type',
        hoverinfo = 'none',
        textfont = list(family= "Arial", size= 7, color= "Black"),
        showlegend=F) %>%
      plotly::layout(xaxis = list(title="SiO\u2082 [wt %]"),
                     yaxis = list(title="Na\u2082O + K\u2082O [wt %]",
                                  scaleanchor='x',scaleratio=1.5),
                     font=list(size = 12),
                     annotations=list(text=c('Alkaline','Subalkaline\u000a/Tholeiitic'),
                                      x=c(73,76),y=c(13,5),
                                      "showarrow"=F,
                                      font=list(size = 8, color = "darkred"))
                     ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'TAS',
          width = 9 * 96,
          height = 6 * 96
        )
      )

  } else if (any(output == 'plotly' & language == 'es')) {
    TAS = plotly::plot_ly() %>%
      plotly::add_polygons(
        data = tb.TAS,
        x=~x,y=~y,
        color=~name.es,
        colors='transparent',
        mode='lines',
        line = list(color = "black"),
        hoverinfo='text',
        showlegend=F) %>%
      plotly::add_lines(
        data = TAS.line,
        x=~x,y=~y,
        name='Series',
        hoverinfo = 'none',
        line = list(color = "darkred"),
        showlegend=F) %>%
      plotly::add_text(
        data = TAS.labels,
        x=~x,y=~y,
        text=~name.es,
        name = 'Tipo',
        hoverinfo = 'none',
        textfont = list(family= "Arial", size= 7, color= "Black"),
        showlegend=F) %>%
      plotly::layout(xaxis = list(title="SiO\u2082 [% peso]"),
                     yaxis = list(title="Na\u2082O + K\u2082O [% peso]",
                                  scaleanchor='x',scaleratio=1.5),
                     font=list(size = 12),
                     annotations=list(text=c('Alcalina','Subalcalina\u000a/Tole\u00edtica'),
                                      x=c(73,76),y=c(13,5),
                                      "showarrow"=F,
                                      font=list(size = 8, color = "darkred"))
                     ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'TAS',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  }

  return(TAS)
}
