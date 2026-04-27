# Summary of multiple parameters in a raster directory

Listing major characteristics of raster inputs. Those characteristics
are the dimensions, the resolution, the extent, the values (min, max)
and the coordinate reference system.

## Usage

``` r
summary_dir(path)
```

## Arguments

- path:

  The path for the Raster\* directory or list of Raster\* to be
  analysed.

## Value

Table with the raster parameters in columns

## Examples

``` r
# \donttest{
url <- "https://zenodo.org/record/3685230/files/SaoLourencoBasin.rda?download=1"
temp <- tempfile()
download.file(url, temp, mode = "wb") # downloading the SaoLourencoBasin dataset
load(temp)
# the acc_changes() function, with the SaoLourencoBasin dataset

summary_dir(raster::unstack(SaoLourencoBasin))
#> # A tibble: 5 × 12
#>   file_name    xmin   xmax   ymin   ymax res_x res_y  nrow  ncol min_val max_val
#>   <chr>       <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <int> <int>   <int>   <int>
#> 1 landscape… 6.54e5 8.50e5 8.10e6 8.29e6    30    30  6372  6546       2      13
#> 2 landscape… 6.54e5 8.50e5 8.10e6 8.29e6    30    30  6372  6546       2      13
#> 3 landscape… 6.54e5 8.50e5 8.10e6 8.29e6    30    30  6372  6546       2      13
#> 4 landscape… 6.54e5 8.50e5 8.10e6 8.29e6    30    30  6372  6546       2      13
#> 5 landscape… 6.54e5 8.50e5 8.10e6 8.29e6    30    30  6372  6546       2      13
#> # ℹ 1 more variable: crs <chr>
# }
```
