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
if (OpenLand:::.openland_try_download_and_load_rda(url,
  object = "SaoLourencoBasin", timeout = 10,
  cache = FALSE
)) {
  summary_dir(raster::unstack(SaoLourencoBasin))
}
# }
```
