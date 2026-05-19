# GMisc

## Description

This package is intendend mainly as a teaching and research tool for
geology related areas (soil mechanics, rock mechanics, directional
stats, hydrogeochem, etc., and more to come) for courses and projects at
the Geology Department ([Escuela Centroamericana de
Geologia](https://maxgav13.github.io/GMisc/geologia.ucr.ac.cr)) of the
University of Costa Rica ([Universidad de Costa
Rica](https://maxgav13.github.io/GMisc/ucr.ac.cr)).

## Citation

Cite package with: <https://doi.org/10.5281/zenodo.4681268>

## Installation

If you installed R using [rig](https://github.com/r-lib/rig/releases),
`pak` is already installed and can skip to step 2.

1.  `install.packages("pak")`

2.  `pak::pak("maxgav13/GMisc")`

Additional packages that are also useful can be installed using:

- [`GMisc::additional_pks()`](https://maxgav13.github.io/GMisc/reference/additional_pks.md)

It is also recommended to have installed the necessary compiling tools:
[Xcode](https://apps.apple.com/us/app/xcode/id497799835?mt=12) for Mac
or [Rtools](https://cran.r-project.org/bin/windows/Rtools/) for Windows.

If you want to install the vignettes you can do:

1.  `install.packages("remotes")`

2.  `remotes::install_github("maxgav13/GMisc", build_vignettes = TRUE)`

## Extras

Some of the main functionalities have been implemented in shiny apps:

- [Layer Boundary](https://maximiliano-01.shinyapps.io/layer_boundary/):
  Different methods can be used to find layer boundaries in a data
  sequence; unvariate and multivariate methods are available.

- [Rock Mechanics](https://maximiliano-01.shinyapps.io/rock_mechanics/):
  Implements basic rock mechanics’ classification and failure criterion.

- [Directional
  Statistics](https://maximiliano-01.shinyapps.io/directional/): Used to
  analyze directional data (strike, dip, lineations, etc.) with some
  basic plots, and one and two sample statistics.

- [Geotechnical Engineering / Soil
  Mechanics](https://maximiliano-01.shinyapps.io/soil_mechanics/):
  Mostly for teaching concepts in soil mechanics (bearing capacity and
  induced stress).

- [Geostatistical
  Analysis](https://maximiliano-01.shinyapps.io/geostatistics/): Takes
  the user in a workflow on how to perform a geostatistical analysis.
