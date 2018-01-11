#' @title Q system (1993)
#' @description Draws data points on top of the Q calssification system of 1993.
#' @param Q A numeric vector with the Q values
#' @param De A numeric vector with the eqivalent dimensions
#' @param dot.col Color for the points
#' @export
#' @return A ggplot object
#' @references Grimstad, E. & Barton, N. (1993). Updating the Q-System for NMT. Proc. int. symp. on sprayed concrete - modern use of wet mixed sprayed concrete for underground supports, 46-66. Oslo, Norway.
#' @import stats
#' @import png
#' @import grid
#' @import ggplot2
#' @examples
#' De = 9
#' Q = c(.026, 1.4, .078, .56, .35)
#' Q_93(Q, De)
Q_93 = function(Q, De, dot.col = 'blue') {

  img = readPNG(system.file("Q_94.png", package = "GMisc"))
  g = rasterGrob(img, width = unit(1,"npc"), height = unit(1,"npc"), interpolate = FALSE)

  df = data.frame(x = Q, y = De)
  p = ggplot(df) +
    annotation_custom(g, -3, 3, -.001, 2.29) +
    geom_point(aes(x, y), color = dot.col, size = 3) +
    scale_x_log10("Rock Mass Quality, Q",
                  breaks = c(.001,.004,.01,.04,.1,.4,1,4,10,40,100,400),
                  labels = c(".001",".004",".01",".04",".1",".4","1","4","10","40","100","400"),
                  limits=c(.001,1000), expand = c(0,0)) +
    scale_y_log10("Equivalent dimension, De",
                  breaks = c(1,2,3,5,10,20,50,100),
                  labels = c("1","2","3","5","10","20","50","100"),
                  limits=c(1,190), expand = c(0,0))
  return(p)
}
