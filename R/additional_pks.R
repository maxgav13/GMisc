#' @title Additional useful packages
#' @description Installs additional packages useful for geology, statistics, and visualization.
#' @export
#' @return Installed packages
#'
additional_pks <- function (){
  install.packages("DescTools", dep=T)
  install.packages("sf", dep=T)
  install.packages("raster", dep=T)
  install.packages("gstat", dep=T)
  install.packages("geoR", dep=T)
  install.packages("automap", dep=T)
  install.packages("OneTwoSamples", dep=T)
  install.packages("rgl", dep=T)
  install.packages("car", dep=T)
  install.packages("rgdal", dep=T)
  install.packages("tmap", dep=T)
  install.packages("ggpubr", dep=T)
  # install.packages("sjstats", dep=T)
  install.packages("sjPlot", dep=T)
  install.packages("rockchalk", dep=T)
  install.packages("effsize", dep=T)
  install.packages("Metrics", dep=T)
  install.packages("plotmo", dep=T)
  devtools::install_github('doomlab/MOTE')
  devtools::install_github('gitrman/itns')
}
