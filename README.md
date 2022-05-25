# GMisc

## Description

This package is intendend mainly as a teaching and research tool for geology related areas (soil mechanics, rock mechanics, directional stats, hydrogeochem, etc., and more to come) for courses and projects at the Geology Department ([Escuela Centroamericana de Geologia](geologia.ucr.ac.cr)) of the University of Costa Rica ([Universidad de Costa Rica](ucr.ac.cr)).

## Citation

Cite package with: https://doi.org/10.5281/zenodo.4681268

## Installation

1. `install.packages("remotes")`

2. `remotes::install_github("maxgav13/GMisc", build_vignettes = TRUE)`

Additional packages that are also useful can be installed using:

* `GMisc::additional_pks()`

It is also recommended to have installed the necessary compiling tools: Xcode for Mac or Rtools for Windows.

## Extras

Some of the main functionalities have been implemented in shiny apps:

* [Layer Boundary](https://maximiliano-01.shinyapps.io/layer_boundary/): Different methods can be used to find layer boundaries in a data sequence; unvariate and multivariate methods are available.

* [Rock Mechanics](https://maximiliano-01.shinyapps.io/rock_mechanics/): Implements basic rock mechanics' classification and failure criterion.

* [Directional Statistics](https://maximiliano-01.shinyapps.io/directional/): Used to analyze directional data (strike, dip, lineations, etc.) with some basic plots, and one and two sample statistics.

* [Geotechnical Engineering / Soil Mechanics](https://maximiliano-01.shinyapps.io/soil_mechanics/): Mostly for teaching concepts in soil mechanics (bearing capacity and induced stress).

* [Geostatistical Analysis](https://maximiliano-01.shinyapps.io/geostatistics/): Takes the user in a workflow on how to perform a geostatistical analysis.

