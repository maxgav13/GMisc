#' @title Piper diagram
#' @description \code{piper_diagram()} draws a static Piper diagram with the water facies. It is a base diagram where data can be plotted.
#' @param opacity Transparency level (default is 0.5)
#'
#' @export
#' @return Piper diagram for hydrochemical data
#'
#' @import ggplot2
#'
#' @details For ploting data the \code{piper_data_prep()} function should be used to transform the data in the proper format. After that the data has \code{x} and \code{y} coordinates, and these are the ones to use when plotting the data on \code{piper_diagram()}.
#'
#' @examples
#' library(ggplot2)
#'
#' d = data.frame(Group = c('A','A','B','B'),
#'                Ca = c(120,150,110,52.6),
#'                Mg = c(78,160,110,28),
#'                Na = c(210,590,340,51.6),
#'                K = c(4.2,2,3.6,2.3),
#'                HCO3 = c(181,181,189,151),
#'                CO3 = 0,
#'                Cl = c(220,744,476,72.2),
#'                SO4 = c(560,1020,584,126))
#'
#' piper_data = piper_data_prep(d)
#'
#' # adding data
#' piper_diagram() +
#'   geom_point(aes(x,y,col=Group,shape=Group),
#'              size=3,
#'              data = piper_data) +
#'   scale_color_brewer('Group',palette = 'Dark2') +
#'   scale_shape_manual('Group',values = c(3,21))
#'
piper_diagram <- function(opacity = 0.5) {

  output = 'ggplot'

  grid1p1 <<- data.frame(x1 = c(20,40,60,80), x2= c(10,20,30,40),y1 = c(0,0,0,0), y2 = c(17.3206,34.6412,51.9618, 69.2824))
  grid1p2 <<- data.frame(x1 = c(20,40,60,80), x2= c(60,70,80,90),y1 = c(0,0,0,0), y2 = c(69.2824, 51.9618,34.6412,17.3206))
  grid1p3 <<- data.frame(x1 = c(10,20,30,40), x2= c(90,80,70,60),y1 = c(17.3206,34.6412,51.9618, 69.2824), y2 = c(17.3206,34.6412,51.9618, 69.2824))
  grid2p1 <<- grid1p1
  grid2p1$x1 <- grid2p1$x1+120
  grid2p1$x2 <- grid2p1$x2+120
  grid2p2 <<- grid1p2
  grid2p2$x1 <- grid2p2$x1+120
  grid2p2$x2 <- grid2p2$x2+120
  grid2p3 <<- grid1p3
  grid2p3$x1 <- grid2p3$x1+120
  grid2p3$x2 <- grid2p3$x2+120
  grid3p1 <<- data.frame(x1=c(100,90, 80, 70),y1=c(34.6412, 51.9618, 69.2824, 86.603), x2=c(150, 140, 130, 120), y2=c(121.2442,138.5648,155.8854,173.2060))
  grid3p2 <<- data.frame(x1=c(70, 80, 90, 100),y1=c(121.2442,138.5648,155.8854,173.2060), x2=c(120, 130, 140, 150), y2=c(34.6412, 51.9618, 69.2824, 86.603))

  ##Upper Diamond##
  ids <- factor(c("Sodium Bicarbonate", "Sodium Chloride",
                  "Calcium Bicarbonate", "Calcium Sulfate"))
  values <- data.frame(
    id = ids,
    value = c(1,2,3,4))
  positions <- data.frame(
    id=rep(ids, each = 4),
    x=c(110,85,110,135,
        135,110,135,160,
        85,60,85,110,
        110,85,110,135),
    y=c(17.3206,60.6221, 103.9236,60.6221,
        60.6221, 103.9236, 147.2251, 103.9236,
        60.6221,103.9236,147.2251,103.9236,
        103.9236,147.2251,190.5266,147.2251))
  polygons <- merge(values, positions)

  ##Left Ternary Plot##
  # ids2 <- factor(c("05", "06", "07", "08"))
  ids2 <- factor(c("Calcium", "No dominant type",
                   "Sodium and potassium", "Magnesium"))
  values2 <- data.frame(
    id = ids2,
    value = c(5,6,7,8))
  positions2 <- data.frame(
    id=rep(ids2, each = 3),
    x=c(50,0,25,
        50,25,75,
        100,50,75,
        75,25,50),
    y=c(0,0,43.3015,
        0,43.3015,43.3015,
        0,0,43.3015,
        43.3015,43.3015,86.603))
  polygons2 <- merge(values2, positions2)

  ##Right Ternary Plot##
  # ids3 <- factor(c("09", "10", "11", "12"))
  ids3 <- factor(c("Bicarbonate", "No dominant type",
                   "Chloride", "Sulphate"))
  values3 <- data.frame(
    id = ids3,
    value = c(9,10,11,12))
  positions3 <- data.frame(
    id=rep(ids3, each = 3),
    x=c(170,120,145,
        170,145,195,
        220,170,195,
        195,145,170),
    y=c(0,0,43.3015,
        0,43.3015,43.3015,
        0,0,43.3015,
        43.3015,43.3015,86.603))
  polygons3 <- merge(values3, positions3)

  polygons_all = dplyr::bind_rows(polygons,polygons2,polygons3)

  p <- ggplot() +
    geom_polygon(data=polygons_all,
                 aes(x=x,y=y,fill=id,group=value),
                 alpha = opacity,color='black',size=.5) +
    ## left hand ternary plot
    # geom_polygon(data=polygons2, aes(x=x,y=y,fill=id,group=id)) +
    geom_segment(aes(x=0,y=0, xend=100, yend=0)) +
    geom_segment(aes(x=0,y=0, xend=50, yend=86.603)) +
    geom_segment(aes(x=50,y=86.603, xend=100, yend=0)) +
    ## right hand ternary plot
    # geom_polygon(data=polygons3, aes(x=x,y=y,fill=id,group=id)) +
    geom_segment(aes(x=120,y=0, xend=220, yend=0)) +
    geom_segment(aes(x=120,y=0, xend=170, yend=86.603)) +
    geom_segment(aes(x=170,y=86.603, xend=220, yend=0)) +
    ## Upper diamond
    # geom_polygon(data=polygons, aes(x=x,y=y,fill=id,group=id)) +
    geom_segment(aes(x=110,y=190.5266, xend=60, yend=103.9236)) +
    geom_segment(aes(x=110,y=190.5266, xend=160, yend=103.9236)) +
    geom_segment(aes(x=110,y=17.3206, xend=160, yend=103.9236)) +
    geom_segment(aes(x=110,y=17.3206, xend=60, yend=103.9236)) +
    ## Add grid lines to the plots
    geom_segment(aes(x=x1, y=y1, yend=y2, xend=x2), data=grid1p1, linetype = "dashed", size = 0.25, colour = "grey50") +
    geom_segment(aes(x=x1, y=y1, yend=y2, xend=x2), data=grid1p2, linetype = "dashed", size = 0.25, colour = "grey50") +
    geom_segment(aes(x=x1, y=y1, yend=y2, xend=x2), data=grid1p3, linetype = "dashed", size = 0.25, colour = "grey50") +
    geom_segment(aes(x=x1, y=y1, yend=y2, xend=x2), data=grid2p1, linetype = "dashed", size = 0.25, colour = "grey50") +
    geom_segment(aes(x=x1, y=y1, yend=y2, xend=x2), data=grid2p2, linetype = "dashed", size = 0.25, colour = "grey50") +
    geom_segment(aes(x=x1, y=y1, yend=y2, xend=x2), data=grid2p3, linetype = "dashed", size = 0.25, colour = "grey50") +
    geom_segment(aes(x=x1, y=y1, yend=y2, xend=x2), data=grid3p1, linetype = "dashed", size = 0.25, colour = "grey50") +
    geom_segment(aes(x=x1, y=y1, yend=y2, xend=x2), data=grid3p2, linetype = "dashed", size = 0.25, colour = "grey50") +

    ### Labels and grid values

    geom_text(aes(c(20,40,60,80),c(-5,-5,-5,-5), label=c(80, 60, 40, 20)), size=3) +
    geom_text(aes(c(35,25,15,5),grid1p2$y2, label=c(80, 60, 40, 20)), size=3) +
    # geom_text(aes(c(95,85,75,65),grid1p3$y2, label=c(80, 60, 40, 20)), size=3) +
    coord_equal(ratio=1) +


    # geom_text(aes(c(155,145,135,125),grid2p2$y2, label=c(20, 40, 60, 80)), size=3) +
    geom_text(aes(c(215,205,195,185),grid2p3$y2, label=c(20, 40, 60, 80)), size=3) +
    geom_text(aes(c(140,160,180,200),c(-5,-5,-5,-5), label=c(20, 40, 60, 80)), size=3) +
    geom_text(aes(grid3p1$x1-5,grid3p1$y1, label=c(80, 60, 40, 20)), size=3) +
    geom_text(aes(grid3p1$x2+5,grid3p1$y2, label=c(20, 40, 60, 80)), size=3) +
    geom_text(aes(grid3p2$x1-5,grid3p2$y1, label=c(20, 40, 60, 80)), size=3) +
    geom_text(aes(grid3p2$x2+5,grid3p2$y2, label=c(80, 60, 40, 20)), size=3) +
    theme_bw() +

    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(), axis.ticks = element_blank(),
          axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.title.x = element_blank(), axis.title.y = element_blank()) +

    scale_fill_brewer('Type',palette = 'Set3')

  if (any(output == 'ggplot')) {
    p = p +
      geom_text(aes(17,50, label="Mg"), angle=60, size=4, parse=TRUE) +
      geom_text(aes(82.5,50, label="Na + K"), angle=-60, size=4) +
      geom_text(aes(50,-10, label="Ca"), size=4, parse=TRUE) +


      geom_text(aes(170,-10, label="Cl"), size=4, parse=TRUE) +
      geom_text(aes(205,50, label="SO[4]"), angle=-60, size=4, parse=TRUE) +
      geom_text(aes(137.5,50, label="CO[3]~+~HCO[3]"), angle=60, size=4, parse=TRUE) +
      geom_text(aes(72.5,150, label="SO[4]~+~Cl"), angle=60, size=4, parse=TRUE) +
      geom_text(aes(147.5,150, label="Ca~+~Mg"), angle=-60, size=4, parse=TRUE)


  } else if (any(output == 'plotly')) {

    #this fixes an issue that plotly can't render geom_text() with the  angle option set properly
    p <- plotly::ggplotly(p
                          ,tooltip = c("ID")
    )
    p <- p %>%
      plotly::layout(
        annotations=list(text=c("Mg<sup>2+</sup>",
                                "Na<sup>+</sup> + K<sup>+</sup>",
                                "Ca<sup>2+</sup>",
                                "Cl<sup>-</sup>",
                                "SO<sub>4</sub><sup>-</sup>",
                                "CO<sub>3</sub><sup>-2</sup> + HCO<sub>3</sub><sup>-</sup>",
                                "SO<sub>4</sub><sup>-2</sup> + Cl<sup>-</sup>",
                                "Ca<sup>2+</sup> + Mg<sup>2+</sup>"),
                         x = c(17,77.5,50,170,205,142.5,72.5,147.5),
                         y = c(50,50,-10,-10,50,50,150,150),
                         textangle = c(-60,60,0,0,60,-60,-60,60),
                         "showarrow"=F, font=list(size = 12, color = "black")
        ),
        showlegend = ifelse(is.null(data),F,T),
        legend = list(x = 0.1, y = 0.9))

  }

  return(p)

}
