# Class Interval

A S4 class for the Interval level result of an Intensity analysis. Can
be plotted with the plot method
[`plot`](https://reginalexavier.github.io/OpenLand/reference/plot.md).

## Details

The slot `intervalData` receives a table containing 4 columns in the
following format:

1.  Period: `<fct>`. The period of interest *\[Yt, Yt+1\]*.

2.  PercentChange: `<dbl>`. Changed area on the Interval level (%).

3.  St: `<dbl>`. Annual intensity of change for a time period \[Yt,
    Yt+1\].

4.  U: `<dbl>`. Uniform intensity for a LUC category change in a time
    period of interest.

## Slots

- `intervalData`:

  tibble. A table with the results of an Intensity analysis at the
  Interval level (*St* and *U* values).
