#' @title Rose diagram (3D)
#' @description Plots a rose diagram for angular (direction and dip) data.
#' @param dir A vector of dip directions in degrees
#' @param dip A vector of dip angles in degrees
#' @param width Petal width
#' @param conf.level Confidence level to use for the cone of confidence (Default is 0.95)
#' @export
#' @return A ggplot obejct
#' @import stats
#' @import ggplot2
#' @details The mean direction and cone of confidence are displayed as red and cyan lines respectively.
#' @examples
#' dir = c(12,18,22,15,10,20)
#' dip = c(42,40,48,30,42,30)
#' rose_diag_3D(dir, dip)
#'
rose_diag_3D = function(dir, dip, width = 30, conf.level = 0.95) {

  r = dir_stats_3D(dir, dip, conf.level = conf.level)
  labelmeandir = bquote("Mean Direction = "~.(format(r$Mean.Dir, 1))*degree*''%+-%''*.(format(round(r$Cone, 1)))*degree)

  # q = hist(x, breaks = seq(0, 360, width))
  # outer = (max(q$counts) %/% 2 + 1) * 2

  n = 360/width
  nt = length(dir)

  plt.dirrose <- ggplot(data.frame(dir), aes(dir)) +
    stat_bin(aes(y=sqrt((..count..)/max(..count..)*100^2)),
             breaks = (0:n)/n*360, colour = "black", fill = "blue") +
    scale_x_continuous(breaks = 0:12/12*360, limits = c(0, 360)) +
    # scale_y_continuous(limits = c(0, outer)) +
    geom_vline(xintercept = c(r$Cone.lower, r$Mean.Dir, r$Cone.upper),
               col=c('cyan', 'red', 'cyan'), size = c(0.75, 1, 0.75)) +
    scale_y_continuous('', labels = NULL) +
    # labs(y = paste('Frequency of measurements (n = ', nt, ')')) +
    labs(caption = labelmeandir, title = paste('N = ', nt)) +
    coord_polar() +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_text(size=8, face = "plain", hjust = 0.9, vjust = 1.3),
          panel.grid.major = element_line(size=.3,colour = 'grey60'),
          panel.grid.minor = element_blank())

  return(plt.dirrose)
}
