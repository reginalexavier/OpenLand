# Quantitative summary of a unique categorical raster

This function presents a summary with the pixel quantity of each
category present in a categorical raster.

## Usage

``` r
summary_map(path)
```

## Arguments

- path:

  The path for the raster to be analysed, if path is a multilayer raster
  only the first RasterLayer will be analysed.

## Value

A table containing in columns the pixel counts for each pixel value

## Examples

``` r
# \donttest{
url <- "https://zenodo.org/record/3685230/files/SaoLourencoBasin.rda?download=1"
temp <- tempfile()
download.file(url, temp, mode = "wb") # downloading the SaoLourencoBasin dataset
load(temp)
summary_map(SaoLourencoBasin[[1]])
#> # A tibble: 11 × 2
#>    pixvalue      Qt
#>       <dbl>   <dbl>
#>  1        2 7359626
#>  2        3 2593349
#>  3        4 2006795
#>  4        5 4519210
#>  5        7 2365650
#>  6        8 1012738
#>  7        9   18895
#>  8       10  119314
#>  9       11 4784531
#> 10       12   97073
#> 11       13   31679
# }
```
