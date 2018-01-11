#' @title kmeans_screeplot
#' @description Creates a scree plot for a given number of clusters to determine the "optimal" number of clusters.
#' @param x A data frame containing the data to use for the kmeans algorithm
#' @param n The maximum number of clusters to try
#' @export
#' @return A scree plot
#' @import stats
#' @import ggplot2
#' @examples
#' kmeans_screeplot(CPTu_data[,2:ncol(CPTu_data)], n = 15)
#'
kmeans_screeplot = function(x, n = 15) {
  # Initialize total within sum of squares error: wss
  wss <- 0

  for (i in 1:n) {
    km.out <- kmeans(x, centers = i, nstart = 20)
    # Save total within sum of squares to wss variable
    wss[i] <- km.out$tot.withinss
  }
  DF = data.frame(nk = 1:n, wss = wss)
  g = ggplot(DF, aes(nk, wss)) +
    geom_line() +
    geom_point() +
    labs(x = "Number of Clusters", y = "Within groups sum of squares") +
    theme_bw()
  return(g)
}
