#' @title New Statistics contrasts
#' @description It plots all the samples with their confidence intervals (CI), and in addition it plots the contrasts between the selected samples.
#' @param dat Data frame of the samples with mean, standard deviation, sample size, and sample id
#' @param g1 Samples for group 1
#' @param g2 Samples for group 2
#' @param conf.level Confidence level for the interval (Default is 0.95)
#' @param ylab Label for the y-axis
#' @param ylab.diff Label for the secondary y-axis for the difference
#' @param col.g1 Color to use for group 1
#' @param col.g2 Color to use for group 2
#' @param col.diff Color to use for the difference
#' @param B.labels Labels to use for plot B of the difference between groups
#' @export
#' @return A ggplot object
#' @references Cumming, G. & Calin-Jageman, R.J. (2017). Introduction to the New Statistics: Estimation, Open Science, and Beyond. New York: Routledge.
#' @import ggplot2
#' @examples
#' dat = data.frame(mean = c(37.5,31.9,41.2,33.4,29.9,38.2),
#'                  sd = c(10,13.5,14.8,10,8.7,10),
#'                  n = c(19,19,19,19,19,19),
#'                  grp = c('NF10','AF10','AD10','NF17','AF17','AD17'))
#' NewStats_contrasts(dat, g1 = c('AD10', 'AD17'), g2 = c('AF10', 'AF17'))
#'
NewStats_contrasts = function(dat, g1, g2, conf.level = 0.95, col.g1 = 'blue', col.g2 = 'green3', col.diff = 'red', ylab = 'Values', ylab.diff = 'Difference', B.labels = c('G1', 'G2', 'Difference')) {

  my_theme = theme(axis.text = element_text(size=12),
                   axis.title = element_text(size=14),
                   legend.position = 'none')

  names(dat) = c('mean', 'sd', 'n', 'grp')

  for (i in 1:nrow(dat)) {
    dat$upper[i]=ci_t(dat[i,1],dat[i,2],dat[i,3],conf.level = conf.level)$upper
  }

  for (i in 1:nrow(dat)) {
    dat$lower[i]=ci_t(dat[i,1],dat[i,2],dat[i,3],conf.level = conf.level)$lower
  }

  G1 = dplyr::filter(dat,.data$grp %in% g1)
  G2 = dplyr::filter(dat,.data$grp %in% g2)

  for (i in 1:nrow(dat)) {
    if (isTRUE(any(dat$grp[i]==g1))){
      dat$contrast[i] = 1
    } else if (isTRUE(any(dat$grp[i]==g2))) {
      dat$contrast[i] = 2
    } else {
      dat$contrast[i] = 0
    }
  }

  dat$grp = factor(dat$grp, levels = dat$grp)
  dat$contrast = as.factor(dat$contrast)
  n.contrast = length(levels(dat$contrast))

  p1 = ggplot(dat, aes(.data$grp, .data$mean)) +
      # geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
      geom_pointrange(size=1, aes(ymin = lower, ymax = upper,
                                  col=.data$contrast)) +
      theme_bw() +
      my_theme +
      labs(x='', y=ylab)+
      {if (n.contrast==3) scale_color_manual(values = c('gray',col.g1,col.g2))
        else scale_color_manual(values = c(col.g1,col.g2))}

  pointEst = c(mean(G1$mean),
               mean(G2$mean),
               mean(G1$mean) - mean(G2$mean))

  lower = c(ci_t(mean(G1$mean),mean(G1$sd),sum(G1$n),conf.level = conf.level)$lower,
            ci_t(mean(G2$mean),mean(G2$sd),sum(G2$n),conf.level = conf.level)$lower,
            ci_t2(mean(G1$mean),mean(G1$sd),sum(G1$n),mean(G2$mean),mean(G2$sd),sum(G2$n),conf.level = conf.level)$lower)

  upper = c(ci_t(mean(G1$mean),mean(G1$sd),sum(G1$n),conf.level = conf.level)$upper,
            ci_t(mean(G2$mean),mean(G2$sd),sum(G2$n),conf.level = conf.level)$upper,
            ci_t2(mean(G1$mean),mean(G1$sd),sum(G1$n),mean(G2$mean),mean(G2$sd),sum(G2$n),conf.level = conf.level)$upper)

  estimates = data.frame(group = factor(c('1','2','3'),labels = B.labels),
                         point = pointEst,
                         lower = unlist(lower),
                         upper = unlist(upper))

  tweak = estimates$point[1] - estimates$point[3]
  estimates[3,2:4] = estimates[3,2:4] + tweak

  p2 = ggplot(estimates,aes(.data$group, .data$point))+
      # geom_errorbar(aes(ymin=lower,ymax=upper),width=.1)+
      geom_pointrange(aes(ymin=lower,ymax=upper,shape=(.data$group=='Difference')),col=c(col.g1,col.g2,col.diff),size=1)+
      scale_y_continuous(sec.axis = sec_axis(~.-tweak,name=ylab.diff), limits = c(layer_scales(p1)$y$range$range))+
      geom_segment(aes(x=1,xend=4,y=.data$point[1],yend=.data$point[1]),linetype=3, size=.6)+
      geom_segment(aes(x=2,xend=4,y=.data$point[2],yend=.data$point[2]),linetype=3, size=.6)+
      theme_bw()+
      my_theme+
      labs(x='', y='')

  q = cowplot::plot_grid(p1, p2, labels = c('A', 'B'))

  return(q)
}
