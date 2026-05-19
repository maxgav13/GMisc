#' @title Additional useful packages
#' @description Installs additional packages useful for geology, statistics, and visualization.
#' @export
#' @return Installed packages
#'
additional_pks <- function() {
  pak::pak("sf")
  pak::pak("raster")
  pak::pak("gstat")
  pak::pak("geoR")
  pak::pak("automap")
  pak::pak("OneTwoSamples")
  pak::pak("rgl")
  pak::pak("car")
  pak::pak("tmap")
  pak::pak("janitor")
  pak::pak("ggpubr")
  pak::pak("sjPlot")
  pak::pak("Hmisc")
  pak::pak("drc")
  # pak::pak("rockchalk")
  # pak::pak("effsize")
  # pak::pak("Metrics")
  pak::pak("highcharter")
  pak::pak("leaflet")
  pak::pak("rpart.plot")
  pak::pak("provenance")
  pak::pak("ggspatial")
  pak::pak('rayshader')
  # pak::pak("DMwR")
  pak::pak("psycho")
  pak::pak("tidymodels")
  pak::pak("tidyverse")
  pak::pak("tidybayes")
  pak::pak("latex2exp")
  pak::pak("binom")
  # pak::pak(c("RVAideMemoire", "EMT", "XNomial"))
  pak::pak(c(
    "zCompositions",
    "easyCODA",
    "robCompositions",
    "Ternary"
  )) # CoDA
  pak::pak(c(
    "ggstatsplot",
    "ggalluvial",
    "easyalluvial",
    "marimekko",
    "dendextend"
  ))
  # pak::pak(c("xaringan", "bookdown"))
  pak::pak(c("kableExtra", "quarto"))
  # pak::pak("rpivotTable")
  # pak::pak("esquisse")
  # pak::pak("ggeffects")
  pak::pak("MOTE")
  pak::pak("vcdExtra")
  # pak::pak(c("georob", "geoCount", "spBayes"))
  pak::pak("corrr")
  pak::pak("ClustGeo")
  pak::pak("factoextra")
  pak::pak("NbClust")
  # pak::pak("leafpm")
  # pak::pak("mapedit")
  pak::pak("ggforce")
  pak::pak("ggfortify")
  pak::pak("ggtext")
  pak::pak("marmap")
  pak::pak("cptcity")
  pak::pak("patchwork")
  pak::pak("gghighlight")
  # pak::pak("rstatix")
  pak::pak("summarytools")
  pak::pak("gtsummary")
  # pak::pak("explor")
  # pak::pak("dlookr")
  # pak::pak("rcompanion")
  pak::pak("modelbased")
  pak::pak("cocor")
  pak::pak("lmtest")
  # pak::pak("conflicted")
  pak::pak("vroom")
  pak::pak("flextable")
  # pak::pak("ftExtra")
  pak::pak("flexdashboard")
  pak::pak("dygraphs")
  # pak::pak("rstanarm")
  # pak::pak("brms")
  # pak::pak("devtools")
  pak::pak("modelsummary")
  pak::pak("GGally")
  pak::pak("easystats")
  pak::pak("papaja")
  pak::pak("rio")
  pak::pak("DT")
  pak::pak('gitrman/itns')
}
