# Class Category

A S4 class for the Category level result of an Intensity analysis. Can
be plotted with the plot method
[`plot`](https://reginalexavier.github.io/OpenLand/reference/plot.md).

## Details

The slots `categoryData` and `categoryStationarity` can receive tables
for "Gain" or "Loss" in the following format:

1.  Gain

    - `categoryData`: `<tibble>`. A table containing 6 columns:

      1.  Period: `<fct>`. The period *\[Yt, Yt+1\]*.

      2.  To: `<fct>`. A LUC category *j*.

      3.  Interval: `<int>`. Duration of the period *\[Yt, Yt+1\]* in
          years.

      4.  GG_km2/GG_pixel: `<dbl>/<int>`. Area of gross gain of category
          *j* during *\[Yt, Yt+1\]*.

      5.  Gtj: `<dbl>`. Annual intensity of gross gain of category *j*
          for time interval *\[Yt, Yt+1\]*.

      6.  St: `<dbl>`. Annual intensity of change for time interval
          *\[Yt, Yt+1\]*.

    - categoryStationarity: `<tibble>`. A table with the results of a
      stationarity test of the gain of the categories on the Category
      level, containing 5 columns:

      1.  To: `<fct>`. A category of interest *j*.

      2.  gain: `<int>`. Number of times a category had gains during all
          time intervals *\[Y1, YT\]*.

      3.  N: `<int>`. Total number of evaluated time points (T).

      4.  Stationarity: `<chr>`. *Active Gain* or *Dormant Gain*.

      5.  Test: `<chr>`. *Y* if stationarity was detected and *N* if
          not.

2.  Loss

    - `categoryData`: `<tibble>`. A table containing 6 columns:

      1.  Period: `<fct>`. The period *\[Yt, Yt+1\]*.

      2.  From: `<fct>`. A LUC category *i*.

      3.  Interval: `<int>`. Duration of the period *\[Yt, Yt+1\]* in
          years.

      4.  GG_km2/GG_pixel: `<dbl>/<int>`. Area of gross loss of category
          *i* during *\[Yt, Yt+1\]*.

      5.  Lti: `<dbl>`. Annual intensity of gross loss of category *i*
          for time interval *\[Yt, Yt+1\]*.

      6.  STt: `<dbl>`. Annual intensity of change for time interval
          *\[Yt, Yt+1\]*.

    - categoryStationarity: `<tibble>`. A table of stationarity test
      over the loss of the categories in the Category level, containing
      5 columns:

      1.  From: `<fct>`. A category of interest *i*.

      2.  loss: `<int>`. Number of times a category had losses during
          all time intervals *\[Y1, YT\]*.

      3.  N: `<int>`. Total number of evaluated time points (T).

      4.  Stationarity: `<chr>`. *Active Loss* or *Dormant Loss*.

      5.  Test: `<chr>`. *Y* if stationarity was detected and *N* if
          not.

## Slots

- `lookupcolor`:

  The colors (character vector) associated with the LUC legend items.

- `categoryData`:

  tibble. A table of Category level's results (gain (*Gtj*) or loss
  (*Lti*) values).

- `categoryStationarity`:

  tibble. A table containing results of a stationarity test. A change is
  considered stationary only if the intensities for all time intervals
  reside on one side of the uniform intensity, i.e are smaller or bigger
  than the uniform rate over the whole period.
