# Contingency table

Extracts LUC transitions for all input grids of the time series.

## Usage

``` r
contingencyTable(input_raster, pixelresolution = 30)
```

## Arguments

- input_raster:

  path (character), Raster\* object or list of Raster\* objects. See  
  [`raster`](https://rdrr.io/pkg/raster/man/raster.html) for more
  information about supported file types.

- pixelresolution:

  numeric. The pixel spatial resolution in meter.

## Value

A list that contains 5 objects.

- `lulc_Mulstistep`: `<tibble>` Contingency table for all analysed time
  steps, containing 8 columns:

  1.  Period: `<chr>` The period *\[Yt, Yt+1\]*.

  2.  From: `<dbl>` numerical code of a LUC category *i*.

  3.  To: `<dbl>` numerical code of a LUC category *j*.

  4.  km2: `<dbl>` Area in square kilometers that transited from the
      category *i* to category *j* in the period from *Yt* to *Yt+1*.

  5.  Interval: `<dbl>` Interval of years between the first and the last
      year of the period *\[Yt, Yt+1\]*.

  6.  QtPixel: `<int>` Pixel count that transited from the categories
      *i* to category *j* in the period from *Yt* to *Yt+1*.

  7.  yearFrom: `<chr>` The year that the change comes from *\[Yt\]*.

  8.  yearTo: `<chr>` The year that the change goes for *\[Yt+1\]*.

- `lulc_Onestep`:`<tibble>` Contingency table for the entire analysed
  period *\[Y1, YT\]*, containing 8 columns identical with
  `lulc_Mulstistep`.

- `tb_legend`: `<tibble>` A table of the pixel value, his name and color
  containing 3 columns:

  1.  categoryValue: `<dbl>` the pixel value of the LUC category.

  2.  categoryName: `<factor>` randomly created string associated with a
      given pixel value of a LUC category.

  3.  color: `<chr>` random color associated with the given pixel value
      of a LUC category. Before further analysis, one would like to
      change the `categoryName` and `color` values.

      - Therefore the category names have to be in the same order as the
        `categoryValue` and the `levels` should be put in the right
        order for legend plotting. Like:



                    myobject$tb_legend$categoryName <- factor(c("name1", "name2", "name3", "name4"),
                                                           levels = c("name3", "name2", "name1", "name4"))

      - The colors have to in the same order as the values in the
        `categoryValue` column. Colors can be given by the color name
        (eg. "black") or an HEX value (eg. \#FFFFFF). Like:



                   myobject$tb_legend$color <- c("#CDB79E", "red", "#66CD00", "yellow")

- `totalArea`: `<tibble>` A table with the total area of the study area
  containing 2 columns:

  1.  area_km2: `<numeric>` The total area in square kilometers.

  2.  QtPixel: `<numeric>` The total area in pixel counts.

- `totalInterval`: `<numeric>` Total interval of the analysed time
  series in years.

## Examples

``` r
# \donttest{
url <- "https://zenodo.org/record/3685230/files/SaoLourencoBasin.rda?download=1"
if (OpenLand:::.openland_try_download_and_load_rda(url,
  object = "SaoLourencoBasin", timeout = 10
)) {
  # the contingencyTable() function, with the SaoLourencoBasin dataset
  contingencyTable(input_raster = SaoLourencoBasin, pixelresolution = 30)
}
# }
```
