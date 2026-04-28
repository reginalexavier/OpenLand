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
#> [[1]]
#> class      : RasterLayer 
#> dimensions : 6372, 6546, 41711112  (nrow, ncol, ncell)
#> resolution : 30, 30  (x, y)
#> extent     : 654007.5, 850387.5, 8099064, 8290224  (xmin, xmax, ymin, ymax)
#> crs        : +proj=utm +zone=21 +south +ellps=GRS80 +units=m +no_defs 
#> source     : r_tmp_2026-04-28_175429.506342_7750_64535.grd 
#> names      : layer 
#> values     : 0, 2  (min, max)
#> 
#> 
#> [[2]]
#> # A tibble: 3 × 3
#>   PxValue       Qt Percent
#>     <int>    <int>   <dbl>
#> 1       0 21819779   87.6 
#> 2       1  2787995   11.2 
#> 3       2   301086    1.21
#> 
# }
```
