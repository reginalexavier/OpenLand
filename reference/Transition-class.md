# Class Transition

A S4 class for the Transition level result of an Intensity analysis. Can
be plotted with the plot method
[`plot`](https://reginalexavier.github.io/OpenLand/reference/plot.md).

## Details

The slots `transitionData` and `transitionStationarity` can receive
tables for "Gain of category n" or "Loss of category m" in the following
format:

1.  Gain of category n:

    - `transitionData`: `<tibble>`. A table with 7 columns:

      1.  Period: `<fct>`. The period *\[Yt, Yt+1\]*.

      2.  From: `<fct>`. A category i.

      3.  To: `<fct>`. The gaining category in the transition of
          interest *(n)*.

      4.  Interval: `<int>`. Duration of the period *\[Yt, Yt+1\]*.

      5.  T_i2n_km2/T_i2n_pixel: `<dbl>`. Area with transition from
          category i to category n during time interval *\[Yt, Yt+1\]*
          where *i* `is not equal to` *n*.

      6.  Rtin: `<dbl>`. Annual intensity of transition from category i
          to category n during time interval *\[Yt, Yt+1\]* where *i*
          `is not equal to` *n*.

      7.  Wtn: `<dbl>`. Value of the uniform intensity of the transition
          to category n from all non-n categories at time Yt during time
          interval *\[Yt, Yt+1\]*.

    - transitionStationarity: `<tibble>`. A table containing results of
      a stationarity test over the gain on *category n* containing 5
      columns:

      1.  From: `<fct>`. The losing category in the transition of
          interest to the category n.

      2.  loss: `<int>`. Number of times the category had losses to the
          category n.

      3.  N: `<int>`. Total number of transitions to be considered as
          stationary (T).

      4.  Stationarity: `<chr>`. *targeted by* or *avoided by* the
          category `n`.

      5.  Test: `<chr>`. *Y* for stationarity detected and *N* when not.

2.  Loss of category m:

    - `transitionData`: `<tibble>`. A table with 7 columns:

      1.  Period: `<fct>`. The period *\[Yt, Yt+1\]*.

      2.  To: `<fct>`. A category *j*.

      3.  From: `<fct>`. The losing category in the transition of
          interest (m).

      4.  Interval: `<dbl>`. Duration of the period *\[Yt, Yt+1\]*.

      5.  T_m2j_km2/T_m2j_pixel: `<dbl>`. Area with transition from
          category *m* to category *j* during time interval *\[Yt,
          Yt+1\]* where *j* `is not equal to` *m*.

      6.  Qtmj: `<dbl>`. Annual intensity of transition from category
          *m* to category *j* during time interval *\[Yt, Yt+1\]* where
          *j* `is not equal to` *m*.

      7.  Vtm: `<dbl>`. Value of the uniform intensity of the transition
          from category *m* to all *non-m* categories at time Y_(t+1)
          during time interval *\[Yt, Yt+1\]*.

    - transitionStationarity: `<tibble>`. A table containing results of
      a stationarity test over the loss of *category m* containing 5
      columns:

      1.  To: `<fct>`. The gaining category in the transition of
          interest from the category m.

      2.  gain: `<int>`. Number of times the category had gains from the
          category m.

      3.  N: `<int>`. Total number of transitions to be considered as
          stationary (T).

      4.  Stationarity: `<chr>`. *targeted* or *avoided* the category
          `m`.

      5.  Test: `<chr>`. *Y* for stationarity detected and *N* when not.

## Slots

- `lookupcolor`:

  The colors (character vector) associated with the LUC legend items.

- `transitionData`:

  tibble. A table of Transition level's results (gain n (*Rtin* & *Wtn*)
  or loss m (*Qtmj* & *Vtm*) values).

- `transitionStationarity`:

  tibble. A table containing results of a stationarity test. A change is
  considered stationary only if the intensities for all time intervals
  reside on one side of the uniform intensity, i.e are smaller or bigger
  than the uniform rate over the whole period.
