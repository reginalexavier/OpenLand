# OpenLand 1.0.4

## Bug Fixes
* **fixed global variable binding issue**: resolved CRAN NOTE "no visible binding for global variable 'changes'" by properly declaring `changes` as a global variable used in dplyr operations. This ensures compatibility with  current dplyr releases where the deprecated `dplyr::changes()` function is completely removed (@DavisVaughan, #13)

## New Features
* implemented `.openland_try_download_and_load_rda()` function for graceful dataset loading with informative error messages
* enhanced dataset availability checks in vignette and examples with automatic error recovery


# OpenLand 1.0.3

* fixed unit test related to the plot function depending on the ggplot package (@teunbrand, #9)

# OpenLand 1.0.2

* if the dataset url is not accessible, the vignette fails gracefully with an informative message without an error
* memory allocation error fixed in `contingencyTable()` function for when it is used on rasters containing many years/layers or large areas

# OpenLand 1.0.1

* fixed summary_map bug

# OpenLand 1.0.0

* this is the the first CRAN version
* a newer version may be available on https://github.com/reginalexavier/OpenLand
* to get started, see the package vignette "Quick introduction to the OpenLand package" and the help files
* if you have any questions or suggestions, please contact me (reginalexavier at rocketmail dot com)
