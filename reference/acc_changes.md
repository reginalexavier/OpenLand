# Accumulates changes in a LULC raster time series

This function calculates the number of times a pixel has changed during
the analysed period. It returns a raster with the number of changes as
pixel value and a table containing the areal percentage of every pixel
value (number of changes).

## Usage

``` r
acc_changes(path)
```

## Arguments

- path:

  The path for the Raster\* directory or list of Raster\* to be
  analysed.

## Value

Two objects, a RasterLayer and a table.

## Examples

``` r
# \donttest{
url <- "https://zenodo.org/record/3685230/files/SaoLourencoBasin.rda?download=1"
if (OpenLand:::.openland_try_download_and_load_rda(url,
  object = "SaoLourencoBasin", timeout = 10
)) {
  acc_changes(SaoLourencoBasin)
}
# }
```
