
<!-- README.md is generated from README.Rmd. Please edit that file -->

# OpenLand

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/tredgi/OpenLand.svg?branch=master)](https://travis-ci.com/tredgi/OpenLand)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/tredgi/OpenLand?branch=master&svg=true)](https://ci.appveyor.com/project/tredgi/OpenLand)
<!-- badges: end -->

OpenLand is an open-source R package for the analysis of land use and
land cover time series. It includes support for loading spatiotemporal
data ‘raster map’; simulating landscapes with some degree of spatial
correlation; extraction of land use transition matrices; computing of
net change and gross changes in a given interval; performing a complete
intensity analysis based on Pontius. It provides methods for
visualizations of land use and land cover change like the outcomes of
intensity analysis, as other graphs like sankey diagram (onestep or
multistep) and chord diagram displaying the inter-relationships between
the land uses.

## Installation

You can install the released version of OpenLand from GitHub repository
with:

``` r
devtools::install_github("tredgi/OpenLand")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(OpenLand)
## basic example code
```
