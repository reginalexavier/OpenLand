## OpenLand 1.0.5 -- CRAN Submission

This is a patch release to address an additional CRAN check issue reported for
OpenLand 1.0.4.

### Changes since last submission

* Prevented examples and the vignette from writing downloaded Zenodo data to the
  persistent user cache directory during CRAN checks.
* Kept the existing behavior of gracefully trying to download and load the
  `SaoLourencoBasin` dataset when the Zenodo link is available.
* Updated `.openland_try_download_and_load_rda()` so `cache = FALSE` downloads
  to a temporary file and does not create or write to `tools::R_user_dir()`.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Downstream dependencies

There are currently no downstream dependencies for this package
