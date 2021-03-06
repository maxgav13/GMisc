#' @title Rose diagram (3D)
#' @description Plots a rose diagram for angular (direction and dip) data.
#' @param az A vector of directions in degrees
#' @param dip A vector of dip angles in degrees
#' @param width Petal width
#' @param dir A logical indicating if the data are directional (1) or non-directional (0)
#' @param conf.level Confidence level to use for the cone of confidence (Default is 0.95)
#' @export
#' @return A ggplot obejct
#' @import ggplot2
#' @details The mean direction and cone of confidence are displayed as red and cyan lines respectively. If non-directional data is provided it must follow the right-hand rule.
#' @examples
#' dip.dir = c(12,18,22,15,10,20)
#' strike = d2s(dip.dir)
#' dip = c(42,40,48,30,42,30)
#' rose_diag_3D(dip.dir, dip)
#'
rose_diag_3D = function(az, dip, width = 30, dir = 1, conf.level = 0.95) {

  if (dir == 1) {
    x = az
  } else {
    x = s2d(az)
  }

  if (dir == 0) {
    y = x + 180
    y = ifelse(y > 360, y - 360, y)
    z = c(x, y)
  } else {
    z = x
  }

  p = dir_stats_3D(d2s(x), dip, conf.level = conf.level)
  r = dir_stats_3D(x, dip, conf.level = conf.level)
  labelmeandir = bquote("Mean Dip Direction = "~.(format(r$Mean.Dir, 1))*degree*''%+-%''*.(format(round(r$Cone, 1)))*degree~'& Mean Dip Angle ='~.(format(r$MeanDip, 1))*degree)
  labelmeanstrike = bquote("Mean Strike Direction = "~.(format(p$Mean.Dir, 1))*degree*''%+-%''*.(format(round(r$Cone, 1)))*degree~'& Mean Dip Angle ='~.(format(r$MeanDip, 1))*degree)

  if (dir == 0) {
    if (p$Cone.lower > 180 & p$Cone.upper < 360 & p$Cone.upper > 180) {
      cone0 = c(p$Cone.lower, p$Mean.Dir, p$Cone.upper) - 180
    } else if (p$Cone.lower > 0 & p$Cone.upper < 180 & p$Cone.lower < 180) {
      cone0 = c(p$Cone.lower, p$Mean.Dir, p$Cone.upper) + 180
    } else if (p$Cone.lower < 180 & p$Cone.upper > 180 & p$Mean.Dir < 180) {
      cone0 = c(p$Cone.lower + 180 , p$Mean.Dir + 180, p$Cone.upper - 180)
    } else if (p$Cone.lower < 180 & p$Cone.upper > 180 & p$Mean.Dir > 180) {
      cone0 = c(p$Cone.lower + 180 , p$Mean.Dir - 180, p$Cone.upper - 180)
    } else if (p$Cone.lower < 360 & p$Cone.upper < 90 & p$Mean.Dir < 360 & p$Mean.Dir > 180) {
      cone0 = c(p$Cone.lower - 180 , p$Mean.Dir - 180, p$Cone.upper + 180)
    } else if (p$Cone.lower < 360 & p$Cone.upper < 90 & p$Mean.Dir < 90) {
      cone0 = c(p$Cone.lower - 180 , p$Mean.Dir + 180, p$Cone.upper + 180)
    }
  }

  if (dir == 1) {
    cone = c(r$Cone.lower, r$Mean.Dir, r$Cone.upper)
  } else {
    cone = c(p$Cone.lower, p$Mean.Dir, p$Cone.upper)
  }

  # q = hist(x, breaks = seq(0, 360, width))
  # outer = (max(q$counts) %/% 2 + 1) * 2

  n = 360/width
  nt = length(x)

  if (dir == 1) {
    plt.dirrose <- ggplot(data.frame(z), aes(z)) +
      stat_bin(aes(y=sqrt((..count..)/max(..count..)*100^2)),
               breaks = (0:n)/n*360, colour = "black", fill = "blue", closed = 'left') +
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
  } else {
    plt.dirrose <- ggplot(data.frame(d2s(z)), aes(d2s(z))) +
      stat_bin(aes(y=sqrt((..count..)/max(..count..)*100^2)),
               breaks = (0:n)/n*360, colour = "black", fill = "blue", closed = 'left') +
      scale_x_continuous(breaks = 0:12/12*360, limits = c(0, 360)) +
      # scale_y_continuous(limits = c(0, outer)) +
      geom_vline(xintercept = c(cone,cone0),
                 col= c('cyan', 'red', 'cyan', 'cyan', 'red', 'cyan'),
                 size = c(0.75, 1, 0.75, 0.75, 1, 0.75)) +
      scale_y_continuous('', labels = NULL) +
      # labs(y = paste('Frequency of measurements (n = ', nt, ')')) +
      labs(caption = labelmeanstrike, title = paste('N = ', nt)) +
      coord_polar() +
      theme_bw() +
      theme(axis.title.x = element_blank(),
            panel.border = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_text(size=8, face = "plain", hjust = 0.9, vjust = 1.3),
            panel.grid.major = element_line(size=.3,colour = 'grey60'),
            panel.grid.minor = element_blank())
  }


  return(plt.dirrose)
}
