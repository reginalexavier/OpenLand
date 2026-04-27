# input_rasters

A methods for loading the raster into OpenLand

## Usage

``` r
.input_rasters(x, ...)

# S4 method for class 'character'
.input_rasters(x, ...)

# S4 method for class 'list'
.input_rasters(x, ...)

# S4 method for class 'RasterLayer'
.input_rasters(x, ...)

# S4 method for class 'RasterBrick'
.input_rasters(x, ...)

# S4 method for class 'RasterStack'
.input_rasters(x, ...)
```

## Arguments

- x:

  path (character), Raster\* object or list of Raster\* objects.

- ...:

  additional arguments to
  `raster::`[`raster`](https://rdrr.io/pkg/raster/man/raster.html).

## Value

A RasterStack

## See also

`raster::`[`raster`](https://rdrr.io/pkg/raster/man/raster.html),
`raster::`[`stack`](https://rdrr.io/pkg/raster/man/stack.html)
