#' @title Rose diagram (2D)
#' @description Plots a rose diagram for angular (dip direction or strike) data.
#' @param x A vector of angular measurements in degrees
#' @param width Petal width
#' @param dir A logical indicating if the data are directional (1) or non-directional (0)
#' @param conf.level Confidence level to use for the cone of confidence (Default is 0.95)
#' @export
#' @return A ggplot obejct
#' @import ggplot2
#' @details Directional data refers to dipping planes or lines, data expressed as dip direction (trend). Non-directional data refers to strike or lineations expressed as azimuths that can take two angles (i.e. 45 or 225).
#' The mean direction and cone of confidence are displayed as red and cyan lines respectively.
#' @examples
#' x = c(255, 239, 222, 231, 199, 271, 222, 274, 228, 246,
#'       177, 199, 257, 201, 237, 209, 216, 180, 182, 250,
#'       219, 196, 197, 246, 218, 235, 232, 243, 232, 180,
#'       231, 254, 242, 149, 212, 210, 230, 205, 220, 268)
#' rose_diag_2D(x, width = 30, dir = 1)
#' # non-directional data example
#' rose_diag_2D(carolina, width = 10, dir = 0)
#'
rose_diag_2D = function(x, width = 30, dir = 1, conf.level = 0.95) {

  if (dir == 0) {
    y = x + 180
    y = ifelse(y > 360, y - 360, y)
    z = c(x, y)
    } else {
      z = x
    }

  n = 360/width
  nt = length(x)

  r = dir_stats_2D(x, dir = dir, conf.level = conf.level)
  labelmeandir = bquote("Mean Direction = "~.(format(r$Mean.Dir, 1))*degree*''%+-%''*.(format(round(r$Cone, 1)))*degree~', N ='~.(nt))
  labelmeandir2 = paste0('Mean direction = ',format(r$Mean.Dir, 1)," (",format(round(r$Cone, 1)),")",', N = ', nt)

  cone = c(r$Cone.lower, r$Mean.Dir, r$Cone.upper)

  if (dir == 0) {
    if (r$Cone.lower > 180 & r$Cone.upper < 360 & r$Cone.upper > 180) {
      cone0 = c(r$Cone.lower, r$Mean.Dir, r$Cone.upper) - 180
    } else if (r$Cone.lower > 0 & r$Cone.upper < 180 & r$Cone.lower < 180) {
      cone0 = c(r$Cone.lower, r$Mean.Dir, r$Cone.upper) + 180
    } else if (r$Cone.lower < 180 & r$Cone.upper > 180 & r$Mean.Dir < 180) {
      cone0 = c(r$Cone.lower + 180 , r$Mean.Dir + 180, r$Cone.upper - 180)
    } else if (r$Cone.lower < 180 & r$Cone.upper > 180 & r$Mean.Dir > 180) {
      cone0 = c(r$Cone.lower + 180 , r$Mean.Dir - 180, r$Cone.upper - 180)
    } else if (r$Cone.lower < 360 & r$Cone.upper < 90 & r$Mean.Dir < 360 & r$Mean.Dir > 180) {
      cone0 = c(r$Cone.lower - 180 , r$Mean.Dir - 180, r$Cone.upper + 180)
    } else if (r$Cone.lower < 360 & r$Cone.upper < 90 & r$Mean.Dir < 90) {
      cone0 = c(r$Cone.lower - 180 , r$Mean.Dir + 180, r$Cone.upper + 180)
    }
  }

  # q = hist(x, breaks = seq(0, 360, width))
  # outer = (max(q$counts) %/% 2 + 1) * 2

  if (dir == 1) {
    plt.dirrose <- ggplot(data.frame(z), aes(z)) +
      stat_bin(aes(y=sqrt(stat(.data$count)/max(stat(.data$count))*100^2)),
               breaks = (0:n)/n*360, colour = "black", fill = "blue", closed = 'left') +
      scale_x_continuous(breaks = 0:12/12*360, limits = c(0, 360)) +
      # scale_y_continuous(limits = c(0, outer)) +
      geom_vline(xintercept = cone,
                 col = c('cyan', 'red', 'cyan'), size = c(0.75, 1, 0.75)) +
      scale_y_continuous('', labels = NULL) +
      # labs(y = paste('Frequency of measurements (n = ', nt, ')')) +
      labs(title = labelmeandir) +
      coord_polar() +
      theme_bw() +
      theme(title = element_text(size = 8),
            axis.title.x = element_blank(),
            panel.border = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_text(size=8, face = "plain", hjust = 0.9, vjust = 1.3),
            panel.grid.major = element_line(size=.3,colour = 'grey60'),
            panel.grid.minor = element_blank())
  } else {
    plt.dirrose <- ggplot(data.frame(z), aes(z)) +
      stat_bin(aes(y=sqrt(stat(.data$count)/max(stat(.data$count))*100^2)),
               breaks = (0:n)/n*360, colour = "black", fill = "blue", closed = 'left') +
      scale_x_continuous(breaks = 0:12/12*360, limits = c(0, 360)) +
      # scale_y_continuous(limits = c(0, outer)) +
      geom_vline(xintercept = c(cone,cone0),
                 col = c('cyan', 'red', 'cyan', 'cyan', 'red', 'cyan'),
                 size = c(0.75, 1, 0.75, 0.75, 1, 0.75)) +
      scale_y_continuous('', labels = NULL) +
      # labs(y = paste('Frequency of measurements (n = ', nt, ')')) +
      labs(title = labelmeandir) +
      coord_polar() +
      theme_bw() +
      theme(title = element_text(size = 8),
            axis.title.x = element_blank(),
            panel.border = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_text(size=8, face = "plain", hjust = 0.9, vjust = 1.3),
            panel.grid.major = element_line(size=.3,colour = 'grey60'),
            panel.grid.minor = element_blank())
  }


  return(plt.dirrose)
}
