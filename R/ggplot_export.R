#' @title Save ggplot
#' @description Saves/exports a ggplot object to eps.
#' @param p A ggplot object
#' @param name Name for the exported file
#' @param dpi Resolution for the exported file
#' @param w With in inches
#' @param h Height in inches
#' @details The exported "eps" file can be manipulated further for final tweaking in the desired vector software or converted to "pdf" as is. This format allows for transparency to be preserved
#' @export
#' @return Exported eps file
#' @import grDevices
#' @import ggplot2
#' @examples
#' p = ggplot(mtcars, aes(wt, mpg)) + geom_point() + geom_smooth(method = "lm")
#' ggplot_export(p)
ggplot_export = function(p, name = "Figure.eps", dpi = 300, w = 7, h = 4) {
  cairo_ps(filename = name,
           width = w, height = h, pointsize = 12,
           fallback_resolution = dpi)
  print(p)
  dev.off()
}
