## OpenLand 1.0.4 — CRAN Submission

This is a resubmission of a previously archived package.

### Changes since last submission

* Removed or replaced problematic external URLs that failed CRAN checks (SSL/redirect issues)

* Resolved "no visible binding for global variable 'changes'"
  by properly declaring `changes` as a global variable in accordance with dplyr's
  removal of the deprecated `dplyr::changes()` function.

### Additional Enhancements

* Implemented `.openland_try_download_and_load_rda()` function for graceful
  dataset loading with informative error messages
* Enhanced vignette and examples dataset availability checks

## R CMD check results

0 errors | 0 warnings | 0 notes

## Downstream dependencies

There are currently no downstream dependencies for this package
