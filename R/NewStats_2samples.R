#' @title New Statistics two samples (idependent & dependent)
#' @description It plots the two samples and the difference between them with their confidence intervals (CI), based on the two samples being independent or dependent.
#' @param x Sample 1
#' @param y Sample 2
#' @param dep Logical indicating if the samples are dependent (Default is F)
#' @param conf.level Confidence level for the interval (Default is 0.95)
#' @param ylab.diff Label for the secondary y-axis for the difference
#' @param col.x Color to use for sample 1
#' @param col.y Color to use for sample 2
#' @param col.diff Color to use for the difference
#' @export
#' @return A ggplot object
#' @references Cumming, G. (2014). The New Statistics: Why and How - Psychological Science, 25(1): 7-29.
#' @import ggplot2
#' @details The labels of the plot are generic but they can be customised using the \code{labs()} and \code{scale_x_discrete(labels = c())} functions. For the dependent samples case the two means are joined by a line, to indicate the dependency
#' @examples
#' set.seed(101)
#' x = rnorm(20, 15, 7)
#' y = rnorm(20, 10, 5)
#' y1 = rnorm(20,x/1.1,2)
#' NewStats_2samples(x, y, dep = FALSE)
#' NewStats_2samples(x, y1, dep = TRUE)
#'
NewStats_2samples = function(x, y, dep = FALSE, conf.level = 0.95, col.x = 'blue', col.y = 'green3', col.diff = 'red', ylab.diff = 'Difference') {

  my_theme = theme(axis.text = element_text(size=12),
                   axis.title = element_text(size=14),
                   legend.position = 'none')

  G1 = x
  G2 = y

  pointEst = c(mean(G1),
               mean(G2),
               mean(G1) - mean(G2))

  lower = c(mean_cl_normal(G1, conf.int = conf.level)[2],
            mean_cl_normal(G2, conf.int = conf.level)[2],
            stats::t.test(G1, G2, paired = dep, conf.level = conf.level)$conf.int[1])

  upper = c(mean_cl_normal(G1)[3],
            mean_cl_normal(G2)[3],
            stats::t.test(G1, G2, paired = dep, conf.level = conf.level)$conf.int[2])

  estimates = data.frame(group = factor(c('1','2','3'),labels = c('G1', 'G2', 'Difference')),
                         point = pointEst,
                         lower = unlist(lower),
                         upper = unlist(upper))

  tweak = estimates$point[1] - estimates$point[3]
  estimates[3,2:4] = estimates[3,2:4] + tweak

  q = ggplot(estimates,aes(.data$group,.data$point))+
    # geom_errorbar(aes(ymin=lower,ymax=upper),width=.1)+
    geom_pointrange(aes(ymin=lower,ymax=upper,shape=(.data$group=='Difference')),col=c(col.x,col.y,col.diff),size=1)+
    scale_y_continuous(sec.axis = sec_axis(~.-tweak,name=ylab.diff))+
    geom_segment(aes(x=1,xend=4,y=.data$point[1],yend=.data$point[1]),linetype=3, size=.6)+
    geom_segment(aes(x=2,xend=4,y=.data$point[2],yend=.data$point[2]),linetype=3, size=.6)+
    {if (dep==TRUE) geom_segment(aes(x=1,xend=2,y=.data$point[1],yend=.data$point[2]))}+
    theme_bw()+
    my_theme+
    labs(x='', y='Values')

  return(q)
}
