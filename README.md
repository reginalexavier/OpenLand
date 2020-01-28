
<!-- README.md is generated from README.Rmd. Please edit that file -->

# OpenLand <img src='man/figures/logo.png' align="right" height="120" />

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/reginalexavier/OpenLand.svg?branch=master)](https://travis-ci.com/reginalexavier/OpenLand)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/reginalexavier/OpenLand?branch=master&svg=true)](https://ci.appveyor.com/project/reginalexavier/OpenLand)
[![Codecov test
coverage](https://codecov.io/gh/reginalexavier/OpenLand/branch/master/graph/badge.svg)](https://codecov.io/gh/reginalexavier/OpenLand?branch=master)
<!-- badges: end -->

OpenLand is an open-source R package for the analysis of land use and
cover (LUC) time series. It includes support for consistency check and
loading spatiotemporal raster data and synthesized spatial plotting.
Several LUC change (LUCC) metrics in regular or irregular time intervals
can be extracted and visualized through one- and multistep sankey and
chord diagrams. A complete intensity analysis according to
\[@Aldwaik2012\] is implemented, including tools for the generation of
standardized multilevel output graphics.

## Installation

You can install the released version of OpenLand from GitHub repository
with:

``` r
devtools::install_github("reginalexavier/OpenLand")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(OpenLand)
## basic example code
```
