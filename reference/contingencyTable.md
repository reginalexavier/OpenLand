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
temp <- tempfile()
download.file(url, temp, mode = "wb") #downloading the online dataset
load(temp)
# the contingencyTable() function, with the SaoLourencoBasin dataset
contingencyTable(input_raster = SaoLourencoBasin, pixelresolution = 30)
#> $lulc_Multistep
#> # A tibble: 130 × 8
#>    Period     From    To      km2 QtPixel Interval yearFrom yearTo
#>    <chr>     <int> <int>    <dbl>   <int>    <int>    <int>  <int>
#>  1 2002-2008     2     2 6543.    7269961        6     2002   2008
#>  2 2002-2008     2    10    1.56     1736        6     2002   2008
#>  3 2002-2008     2    11   55.2     61320        6     2002   2008
#>  4 2002-2008     2    12   23.9     26609        6     2002   2008
#>  5 2002-2008     3     2   37.5     41649        6     2002   2008
#>  6 2002-2008     3     3 2133.    2370190        6     2002   2008
#>  7 2002-2008     3     7  155.     172718        6     2002   2008
#>  8 2002-2008     3    11    7.48     8307        6     2002   2008
#>  9 2002-2008     3    12    0.356     395        6     2002   2008
#> 10 2002-2008     3    13    0.081      90        6     2002   2008
#> # ℹ 120 more rows
#> 
#> $lulc_Onestep
#> # A tibble: 45 × 8
#>    Period     From    To     km2 QtPixel Interval yearFrom yearTo
#>    <chr>     <int> <int>   <dbl>   <int>    <int>    <int>  <int>
#>  1 2002-2014     2     2 6169.   6854816       12     2002   2014
#>  2 2002-2014     2     9    2.39    2651       12     2002   2014
#>  3 2002-2014     2    10   10.4    11513       12     2002   2014
#>  4 2002-2014     2    11  412.    457631       12     2002   2014
#>  5 2002-2014     2    12   29.7    33015       12     2002   2014
#>  6 2002-2014     3     2  110.    121762       12     2002   2014
#>  7 2002-2014     3     3 2091.   2323665       12     2002   2014
#>  8 2002-2014     3     7  116.    129304       12     2002   2014
#>  9 2002-2014     3     9    7.00    7774       12     2002   2014
#> 10 2002-2014     3    11    9.32   10359       12     2002   2014
#> # ℹ 35 more rows
#> 
#> $tb_legend
#> # A tibble: 11 × 3
#>    categoryValue categoryName color  
#>            <int> <fct>        <chr>  
#>  1             2 IEY          #002F70
#>  2             3 XOM          #8EA4DE
#>  3             4 BWK          #0A468D
#>  4             5 KEU          #EAACAC
#>  5             7 LBF          #5F1415
#>  6             8 DPB          #DCE2F6
#>  7             9 CYV          #A13F3F
#>  8            10 KGU          #6F8DD2
#>  9            11 FIV          #C5CFF0
#> 10            12 JUB          #CE7575
#> 11            13 QTX          #295EAE
#> 
#> $totalArea
#> # A tibble: 1 × 2
#>   area_km2  QtPixel
#>      <dbl>    <int>
#> 1   22418. 24908860
#> 
#> $totalInterval
#> [1] 12
#> 
# }

```
