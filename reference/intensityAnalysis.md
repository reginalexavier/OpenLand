# Performs the intensity analysis based on cross-tabulation matrices of each time step

This function implements an Intensity Analysis (IA) according to Aldwaik
& Pontius (2012), a quantitative method to analyze time series of land
use and cover (LUC) maps. For IA, a cross-tabulation matrix is composed
for each LUC transition step in time.

## Usage

``` r
intensityAnalysis(dataset, category_n, category_m, area_km2 = TRUE)
```

## Arguments

- dataset:

  list. The result object from
  [`contingencyTable`](https://reginalexavier.github.io/OpenLand/reference/contingencyTable.md).

- category_n:

  character. The gaining category in the transition of interest (*n*).

- category_m:

  character. The losing category in the transition of interest (*m*).

- area_km2:

  logical. If TRUE the change is computed in km2, if FALSE in pixel
  counts.

## Value

Intensity object

## Details

IA includes three levels of analysis of LUC changes. Consecutive
analysis levels detail hereby information given by the previous analysis
level (Aldwaik and Pontius, 2012, 2013).

1.  The *interval level* examines how the size and speed of change vary
    across time intervals.

2.  The *category level* examines how the size and intensity of gross
    losses and gross gains in each category vary across categories for
    each time interval.

3.  The *transition level* examines how the size and intensity of a
    category’s transitions vary across the other categories that are
    available for that transition.

At each analysis level, the method tests for stationarity of patterns
across time intervals.

**The function returns a list with 6 objects:**

1.  lulc_table: `tibble`. Contingency table of LUC transitions at all
    analysed time steps, containing 6 columns:

    1.  Period: `<fct>`. Evaluated period of transition in the format
        `year t - year t+1`.

    2.  From: `<fct>`. The category in year t.

    3.  To: `<fct>`. The category in year t+1.

    4.  km2: `<dbl>`. Area in square kilometers that transited from the
        category `From`. to the category `To` in the period.

    5.  QtPixel: `<int>`. Number of pixels that transited from. the
        category `From` to the category `To` in the period.

    6.  Interval: `<int>`. Interval in years of the evaluated period.

2.  *lv1_tbl*: An
    [`Interval`](https://reginalexavier.github.io/OpenLand/reference/Interval-class.md)
    object containing the *St* and *U* values.

3.  *category_lvlGain*: A
    [`Category`](https://reginalexavier.github.io/OpenLand/reference/Category-class.md)
    object containing the gain of the LUC category in a period (*Gtj*).

4.  *category_lvlLoss*: A
    [`Category`](https://reginalexavier.github.io/OpenLand/reference/Category-class.md)
    object containing the loss of the LUC category in a period (*Lti*).

5.  *transition_lvlGain_n*: A
    [`Transition`](https://reginalexavier.github.io/OpenLand/reference/Transition-class.md)
    object containing the annualized rate of gain in *category n*
    (*Rtin*) and the respective Uniform Intensity (*Wtn*).

6.  *transition_lvlLoss_m*: A
    [`Transition`](https://reginalexavier.github.io/OpenLand/reference/Transition-class.md)
    object containing the annualized rate of loss in *category m*
    (*Qtmj*) and the respective Uniform Intensity (*Vtm*).

## References

Aldwaik, S. Z. and Pontius, R. G. (2012) ‘Intensity analysis to unify
measurements of size and stationarity of land changes by interval,
category, and transition, Landscape and Urban Planning. Elsevier B.V.,
106(1), pp. 103–114.
[doi:10.1016/j.landurbplan.2012.02.010](https://doi.org/10.1016/j.landurbplan.2012.02.010)
.

Aldwaik, S. Z. and Pontius, R. G. (2013) ‘Map errors that could account
for deviations from a uniform intensity of land change, International
Journal of Geographical Information Science. Taylor & Francis, 27(9),
pp. 1717–1739.
[doi:10.1080/13658816.2013.787618](https://doi.org/10.1080/13658816.2013.787618)
.

## Examples

``` r
# editing the category name

SL_2002_2014$tb_legend$categoryName <- factor(
  c(
    "Ap", "FF", "SA", "SG", "aa", "SF",
    "Agua", "Iu", "Ac", "R", "Im"
  ),
  levels = c(
    "FF", "SF", "SA", "SG", "aa", "Ap",
    "Ac", "Im", "Iu", "Agua", "R"
  )
)

SL_2002_2014$tb_legend$color <- c(
  "#FFE4B5", "#228B22", "#00FF00", "#CAFF70",
  "#EE6363", "#00CD00", "#436EEE", "#FFAEB9",
  "#FFA54F", "#68228B", "#636363"
)

intensityAnalysis(dataset = SL_2002_2014, category_n = "Ap", category_m = "SG", area_km2 = TRUE)
#> $lulc_table
#> # A tibble: 130 × 6
#>    Period    From  To         km2 QtPixel Interval
#>    <fct>     <fct> <fct>    <dbl>   <int>    <int>
#>  1 2002-2008 Ap    Ap    6543.    7269961        6
#>  2 2002-2008 Ap    Iu       1.56     1736        6
#>  3 2002-2008 Ap    Ac      55.2     61320        6
#>  4 2002-2008 Ap    R       23.9     26609        6
#>  5 2002-2008 FF    Ap      37.5     41649        6
#>  6 2002-2008 FF    FF    2133.    2370190        6
#>  7 2002-2008 FF    aa     155.     172718        6
#>  8 2002-2008 FF    Ac       7.48     8307        6
#>  9 2002-2008 FF    R        0.356     395        6
#> 10 2002-2008 FF    Im       0.081      90        6
#> # ℹ 120 more rows
#> 
#> $interval_lvl
#> An object of class "Interval"
#> Slot "intervalData":
#> # A tibble: 4 × 4
#> # Groups:   Period [4]
#>   Period    PercentChange    St     U
#>   <fct>             <dbl> <dbl> <dbl>
#> 1 2012-2014         3.32  1.66   1.13
#> 2 2010-2012         4.23  2.12   1.13
#> 3 2008-2010         0.880 0.440  1.13
#> 4 2002-2008         5.18  0.864  1.13
#> 
#> 
#> $category_lvlGain
#> An object of class "Category"
#> Slot "lookupcolor":
#>        Ap        FF        SA        SG        aa        SF      Agua        Iu 
#> "#FFE4B5" "#228B22" "#00FF00" "#CAFF70" "#EE6363" "#00CD00" "#436EEE" "#FFAEB9" 
#>        Ac         R        Im 
#> "#FFA54F" "#68228B" "#636363" 
#> 
#> Slot "categoryData":
#> # A tibble: 23 × 6
#> # Groups:   Period, To [23]
#>    Period    To    Interval  GG_km2   Gtj    St
#>    <fct>     <fct>    <int>   <dbl> <dbl> <dbl>
#>  1 2012-2014 aa           2  14.9   0.510  1.66
#>  2 2012-2014 Ap           2 612.    3.92   1.66
#>  3 2012-2014 Ac           2 110.    1.14   1.66
#>  4 2012-2014 Im           2   0.195 0.337  1.66
#>  5 2012-2014 Iu           2   6.79  2.67   1.66
#>  6 2010-2012 aa           2  47.0   1.18   2.12
#>  7 2010-2012 Ap           2 707.    4.84   2.12
#>  8 2010-2012 Ac           2 189.    2.00   2.12
#>  9 2010-2012 Iu           2   1.90  0.792  2.12
#> 10 2010-2012 R            2   2.76  0.951  2.12
#> # ℹ 13 more rows
#> 
#> Slot "categoryStationarity":
#> # A tibble: 12 × 5
#>    To     Gain     N Stationarity Test 
#>    <fct> <int> <int> <chr>        <chr>
#>  1 aa        2     4 Active Gain  N    
#>  2 Ap        2     4 Active Gain  N    
#>  3 Ac        1     4 Active Gain  N    
#>  4 Iu        2     4 Active Gain  N    
#>  5 Agua      1     4 Active Gain  N    
#>  6 R         2     4 Active Gain  N    
#>  7 aa        2     4 Dormant Gain N    
#>  8 Ap        2     4 Dormant Gain N    
#>  9 Ac        3     4 Dormant Gain N    
#> 10 Im        3     4 Dormant Gain N    
#> 11 Iu        2     4 Dormant Gain N    
#> 12 R         1     4 Dormant Gain N    
#> 
#> 
#> $category_lvlLoss
#> An object of class "Category"
#> Slot "lookupcolor":
#>        Ap        FF        SA        SG        aa        SF      Agua        Iu 
#> "#FFE4B5" "#228B22" "#00FF00" "#CAFF70" "#EE6363" "#00CD00" "#436EEE" "#FFAEB9" 
#>        Ac         R        Im 
#> "#FFA54F" "#68228B" "#636363" 
#> 
#> Slot "categoryData":
#> # A tibble: 29 × 6
#> # Groups:   Period, From [29]
#>    Period    From  Interval GL_km2     Lti    St
#>    <fct>     <fct>    <int>  <dbl>   <dbl> <dbl>
#>  1 2012-2014 FF           2  15.4   0.365   1.66
#>  2 2012-2014 SF           2   3.49  0.224   1.66
#>  3 2012-2014 SA           2  25.3   0.845   1.66
#>  4 2012-2014 SG           2  46.9   0.634   1.66
#>  5 2012-2014 aa           2 541.   13.6     1.66
#>  6 2012-2014 Ap           2 111.    0.759   1.66
#>  7 2012-2014 Ac           2   1.26  0.0133  1.66
#>  8 2010-2012 FF           2  11.1   0.263   2.12
#>  9 2010-2012 SF           2   4.44  0.283   2.12
#> 10 2010-2012 SA           2  29.7   0.974   2.12
#> # ℹ 19 more rows
#> 
#> Slot "categoryStationarity":
#> # A tibble: 14 × 5
#>    From   Loss     N Stationarity Test 
#>    <fct> <int> <int> <chr>        <chr>
#>  1 FF        1     4 Active Loss  N    
#>  2 SF        2     4 Active Loss  N    
#>  3 SA        2     4 Active Loss  N    
#>  4 SG        2     4 Active Loss  N    
#>  5 aa        3     4 Active Loss  N    
#>  6 Ap        1     4 Active Loss  N    
#>  7 R         1     4 Active Loss  N    
#>  8 FF        3     4 Dormant Loss N    
#>  9 SF        2     4 Dormant Loss N    
#> 10 SA        2     4 Dormant Loss N    
#> 11 SG        2     4 Dormant Loss N    
#> 12 aa        1     4 Dormant Loss N    
#> 13 Ap        3     4 Dormant Loss N    
#> 14 Ac        4     4 Dormant Loss Y    
#> 
#> 
#> $transition_lvlGain_n
#> An object of class "Transition"
#> Slot "lookupcolor":
#>        Ap        FF        SA        SG        aa        SF      Agua        Iu 
#> "#FFE4B5" "#228B22" "#00FF00" "#CAFF70" "#EE6363" "#00CD00" "#436EEE" "#FFAEB9" 
#>        Ac         R        Im 
#> "#FFA54F" "#68228B" "#636363" 
#> 
#> Slot "transitionData":
#> # A tibble: 20 × 7
#> # Groups:   Period, From [20]
#>    Period    From  To    Interval T_i2n_km2    Rtin    Wtn
#>    <fct>     <fct> <fct>    <int>     <dbl>   <dbl>  <dbl>
#>  1 2012-2014 FF    Ap           2    12.9    0.307  2.03  
#>  2 2012-2014 SF    Ap           2     2.38   0.152  2.03  
#>  3 2012-2014 SA    Ap           2    18.1    0.605  2.03  
#>  4 2012-2014 SG    Ap           2    42.1    0.569  2.03  
#>  5 2012-2014 aa    Ap           2   537.    13.5    2.03  
#>  6 2010-2012 FF    Ap           2     3.63   0.0856 2.26  
#>  7 2010-2012 SF    Ap           2     1.08   0.0685 2.26  
#>  8 2010-2012 SA    Ap           2    12.3    0.403  2.26  
#>  9 2010-2012 SG    Ap           2     8.53   0.114  2.26  
#> 10 2010-2012 aa    Ap           2   682.    13.0    2.26  
#> 11 2008-2010 FF    Ap           2     0.968  0.0227 0.0610
#> 12 2008-2010 SF    Ap           2     1.10   0.0694 0.0610
#> 13 2008-2010 SA    Ap           2     0.971  0.0314 0.0610
#> 14 2008-2010 SG    Ap           2     3.94   0.0524 0.0610
#> 15 2008-2010 aa    Ap           2    12.0    0.233  0.0610
#> 16 2002-2008 FF    Ap           6    37.5    0.268  0.323 
#> 17 2002-2008 SF    Ap           6    24.8    0.453  0.323 
#> 18 2002-2008 SA    Ap           6    65.8    0.608  0.323 
#> 19 2002-2008 SG    Ap           6    48.3    0.198  0.323 
#> 20 2002-2008 aa    Ap           6   130.     1.02   0.323 
#> 
#> Slot "transitionStationarity":
#> # A tibble: 7 × 5
#>   From   Loss     N Stationarity   Test 
#>   <fct> <int> <int> <chr>          <chr>
#> 1 SF        2     4 targeted by Ap N    
#> 2 SA        1     4 targeted by Ap N    
#> 3 aa        4     4 targeted by Ap Y    
#> 4 FF        4     4 avoided by Ap  Y    
#> 5 SF        2     4 avoided by Ap  N    
#> 6 SA        3     4 avoided by Ap  N    
#> 7 SG        4     4 avoided by Ap  Y    
#> 
#> 
#> $transition_lvlLoss_m
#> An object of class "Transition"
#> Slot "lookupcolor":
#>        Ap        FF        SA        SG        aa        SF      Agua        Iu 
#> "#FFE4B5" "#228B22" "#00FF00" "#CAFF70" "#EE6363" "#00CD00" "#436EEE" "#FFAEB9" 
#>        Ac         R        Im 
#> "#FFA54F" "#68228B" "#636363" 
#> 
#> Slot "transitionData":
#> # A tibble: 14 × 7
#> # Groups:   Period, To [14]
#>    Period    To    From  Interval T_m2j_km2     Qtmj    Vtm
#>    <fct>     <fct> <fct>    <int>     <dbl>    <dbl>  <dbl>
#>  1 2012-2014 aa    SG           2    4.76   0.163    0.125 
#>  2 2012-2014 Ap    SG           2   42.1    0.270    0.125 
#>  3 2012-2014 Ac    SG           2    0.0621 0.000642 0.125 
#>  4 2010-2012 aa    SG           2   18.9    0.475    0.0749
#>  5 2010-2012 Ap    SG           2    8.53   0.0584   0.0749
#>  6 2010-2012 Ac    SG           2    0.632  0.00669  0.0749
#>  7 2008-2010 aa    SG           2   30.6    0.584    0.0929
#>  8 2008-2010 Ap    SG           2    3.94   0.0290   0.0929
#>  9 2008-2010 Ac    SG           2    0.0873 0.000962 0.0929
#> 10 2008-2010 Iu    SG           2    0.0504 0.0213   0.0929
#> 11 2002-2008 aa    SG           6  244.     1.58     0.273 
#> 12 2002-2008 Ap    SG           6   48.3    0.118    0.273 
#> 13 2002-2008 Ac    SG           6   13.1    0.0489   0.273 
#> 14 2002-2008 R     SG           6    0.0999 0.0129   0.273 
#> 
#> Slot "transitionStationarity":
#> # A tibble: 6 × 5
#>   To     Gain     N Stationarity Test 
#>   <fct> <int> <int> <chr>        <chr>
#> 1 aa        4     4 targeted SG  Y    
#> 2 Ap        1     4 targeted SG  N    
#> 3 Ap        3     4 avoided SG   N    
#> 4 Ac        4     4 avoided SG   Y    
#> 5 Iu        1     4 avoided SG   N    
#> 6 R         1     4 avoided SG   N    
#> 
#> 
```
