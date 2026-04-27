# Create Raster with Random pixel Value

This function creates a raster series with some setup like the layer
name and the sample value for the lulc

## Usage

``` r
.demo_landscape(
  year,
  nrows = 100,
  ncols = 100,
  res = 1,
  xmn = 0,
  xmx = 100,
  ymn = 0,
  ymx = 100,
  crs = NA,
  category = 1:5,
  prob = NULL
)
```

## Arguments

- year:

  numeric. A vector of year, first and last included.

- nrows:

  numeric. nrows of the raster to be created.

- ncols:

  numeric. ncols of the raster to be created.

- res:

  numeric. the resolution of the raster to be created.

- xmn:

  numeric. x minimum extent.

- xmx:

  numeric. x maximum extent.

- ymn:

  numeric. y minimum extent.

- ymx:

  numeric. y maximum extent.

- crs:

  character. the coordinate referencing system.

- category:

  A numeric vector of the raster categories.

- prob:

  A numeric vector of the probability of occurrence for the category
  list.

## Value

list

## See also

[`raster`](https://rdrr.io/pkg/raster/man/raster.html)

## Examples

``` r
.demo_landscape(
  year = 2000:2005,
  res = 1,
  crs = "+proj=utm +zone=21 +south +ellps=GRS80 +units=m +no_defs"
)
#> $landscape_2000
#> class      : RasterLayer 
#> dimensions : 100, 100, 10000  (nrow, ncol, ncell)
#> resolution : 1, 1  (x, y)
#> extent     : 0, 100, 0, 100  (xmin, xmax, ymin, ymax)
#> crs        : +proj=utm +zone=21 +south +ellps=GRS80 +units=m +no_defs 
#> source     : memory
#> names      : landscape_2000 
#> values     : 1, 5  (min, max)
#> 
#> 
#> $landscape_2001
#> class      : RasterLayer 
#> dimensions : 100, 100, 10000  (nrow, ncol, ncell)
#> resolution : 1, 1  (x, y)
#> extent     : 0, 100, 0, 100  (xmin, xmax, ymin, ymax)
#> crs        : +proj=utm +zone=21 +south +ellps=GRS80 +units=m +no_defs 
#> source     : memory
#> names      : landscape_2001 
#> values     : 1, 5  (min, max)
#> 
#> 
#> $landscape_2002
#> class      : RasterLayer 
#> dimensions : 100, 100, 10000  (nrow, ncol, ncell)
#> resolution : 1, 1  (x, y)
#> extent     : 0, 100, 0, 100  (xmin, xmax, ymin, ymax)
#> crs        : +proj=utm +zone=21 +south +ellps=GRS80 +units=m +no_defs 
#> source     : memory
#> names      : landscape_2002 
#> values     : 1, 5  (min, max)
#> 
#> 
#> $landscape_2003
#> class      : RasterLayer 
#> dimensions : 100, 100, 10000  (nrow, ncol, ncell)
#> resolution : 1, 1  (x, y)
#> extent     : 0, 100, 0, 100  (xmin, xmax, ymin, ymax)
#> crs        : +proj=utm +zone=21 +south +ellps=GRS80 +units=m +no_defs 
#> source     : memory
#> names      : landscape_2003 
#> values     : 1, 5  (min, max)
#> 
#> 
#> $landscape_2004
#> class      : RasterLayer 
#> dimensions : 100, 100, 10000  (nrow, ncol, ncell)
#> resolution : 1, 1  (x, y)
#> extent     : 0, 100, 0, 100  (xmin, xmax, ymin, ymax)
#> crs        : +proj=utm +zone=21 +south +ellps=GRS80 +units=m +no_defs 
#> source     : memory
#> names      : landscape_2004 
#> values     : 1, 5  (min, max)
#> 
#> 
#> $landscape_2005
#> class      : RasterLayer 
#> dimensions : 100, 100, 10000  (nrow, ncol, ncell)
#> resolution : 1, 1  (x, y)
#> extent     : 0, 100, 0, 100  (xmin, xmax, ymin, ymax)
#> crs        : +proj=utm +zone=21 +south +ellps=GRS80 +units=m +no_defs 
#> source     : memory
#> names      : landscape_2005 
#> values     : 1, 5  (min, max)
#> 
#> 
```
