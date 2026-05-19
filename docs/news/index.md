# Changelog

## GMisc 0.4.0

- Change package installation to `pak`.
- Update documentation by merging help pages for similar functions.
- Updated `bearing_capacity` and `induced_stress` functions.
- Added functions for family *soil mechanics*: `insitu_stress`,
  `Casagrande_chart`, `Casagrande_LL`, `fallcone_limits`,
  `compaction_test`, `particle_size_graph`, and several for soil
  classification using the USCS guidelines.
- Added plots for difficult clayey soil: `soil_collapse`,
  `soil_expansion`, and `soil_swelling`.
- Updated and added several ternary diagrams.
- Added non-linear functions and corresponding self-starters for
  [`nls()`](https://rdrr.io/r/stats/nls.html) and
  [`drc::drm()`](https://rdrr.io/pkg/drc/man/drm.html).
- Updated all functions related to directional statistics, mostly change
  names from `2D` to `circ` and `3D` to `spher`, and added more
  functionality.
- Most of the changes can be seen in the updated corresponding
  vignettes.
