# Changelog

## OpenLand (development version)

## OpenLand 1.0.3

CRAN release: 2024-05-03

- fixed unit test related to the plot function depending on the ggplot
  package ([@teunbrand](https://github.com/teunbrand),
  [\#9](https://github.com/reginalexavier/OpenLand/issues/9))

## OpenLand 1.0.2

CRAN release: 2021-11-02

- if the dataset url is not accessible, the vignette fails gracefully
  with an informative message without an error
- memory allocation error fixed in
  [`contingencyTable()`](https://reginalexavier.github.io/OpenLand/reference/contingencyTable.md)
  function for when it is used on rasters containing many years/layers
  or large areas

## OpenLand 1.0.1

CRAN release: 2020-04-19

- fixed summary_map bug

## OpenLand 1.0.0

CRAN release: 2020-03-23

- this is the the first CRAN version
- a newer version may be available on
  <https://github.com/reginalexavier/OpenLand>
- to get started, see the package vignette “Quick introduction to the
  OpenLand package” and the help files
- if you have any questions or suggestions, please contact me
  (reginalexavier at rocketmail dot com)
