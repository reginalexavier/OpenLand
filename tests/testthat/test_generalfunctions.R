context("generalfunctions")

set.seed(159)
demo_raster <- .demo_landscape(
  year = 2000:2005,
  res = 1,
  crs = "+proj=utm +zone=21 +south +ellps=GRS80 +units=m +no_defs"
)
demo_acc_changes <- acc_changes(demo_raster)

test_that("Behavior of acc_changes", {
  expect_error(acc_changes(.demo_landscape(2000)))
  expect_error(acc_changes(2000))
  expect_silent(acc_changes(demo_raster))
  expect_silent(acc_changes(raster::stack(demo_raster)))
  expect_silent(acc_changes(raster::brick(demo_raster)))
  expect_length(demo_acc_changes, 2)
  expect_output(str(demo_acc_changes), "List of 2")
  expect_match(class(demo_acc_changes[[1]]), "RasterLayer")
  expect_setequal(class(demo_acc_changes[[2]]), c("tbl_df", "tbl", "data.frame"))
  expect_equal(sum(demo_acc_changes[[2]][, 3]), 100)
  expect_equal(demo_acc_changes[[2]][[4, 2]], 1998)
  expect_equal(ncol(demo_acc_changes[[2]]), 3)
})


if (FALSE) {
  testFolder <- tempdir()

  lapply(.demo_landscape(year = 2000:2004), function(x) {
    raster::writeRaster(x,
      filename = file.path(testFolder, paste0(names(x), ".tif")),
      datatype = "INT1U",
      overwrite = TRUE
    )
  })

  expect_silent(summary_dir(testFolder))
  expect_equal(nrow(summary_dir(testFolder)), 5)
}


test_that("Behavior of summary_dir", {
  expect_silent(summary_dir(demo_raster))
  expect_visible(summary_dir(demo_raster))
  expect_error(summary_dir(raster::stack(demo_raster)))
  expect_error(summary_dir(raster::brick(demo_raster)))
  expect_equal(ncol(summary_dir(demo_raster)), 12)
  expect_equal(nrow(summary_dir(demo_raster)), length(demo_raster))
  expect_setequal(class(summary_dir(demo_raster)), c("tbl_df", "tbl", "data.frame"))
})


test_that("summary_dir and summary_map work with file paths", {
  testFolder <- tempfile(pattern = "OpenLand-raster-dir-")
  dir.create(testFolder)

  demo_small <- .demo_landscape(
    year = 2000:2002,
    res = 1,
    crs = NA
  )

  lapply(demo_small, function(x) {
    raster::writeRaster(x,
      filename = file.path(testFolder, paste0(names(x), ".tif")),
      datatype = "INT1U",
      overwrite = TRUE
    )
  })

  out_dir <- summary_dir(testFolder)
  expect_setequal(class(out_dir), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(out_dir), length(demo_small))

  tif1 <- list.files(testFolder, pattern = "\\.tif$", full.names = TRUE)[1]
  out_map <- summary_map(tif1)
  expect_setequal(class(out_map), c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(out_map), 2)
})


test_that("Behavior of summary_map", {
  # expect_silent(summary_map(list.files(testFolder, pattern = "tif$", full.names = TRUE)[1]))
  # expect_silent(summary_map(.input_rasters(testFolder)))
  expect_silent(summary_map(demo_raster[[1]]))
  expect_visible(summary_map(demo_raster[[1]]))
  expect_equal(ncol(summary_map(demo_raster[[1]])), 2)
  expect_equal(nrow(summary_map(demo_raster[[1]])), 5)
})


test_that(".openland_try_download_and_load_rda loads from cache", {
  cache_dir <- tempfile(pattern = "OpenLand-cache-")
  dir.create(cache_dir)

  url <- "https://example.com/SaoLourencoBasin.rda?download=1"
  cached_path <- file.path(cache_dir, "SaoLourencoBasin.rda")

  a <- 123
  save(a, file = cached_path)

  env <- new.env(parent = emptyenv())
  ok <- OpenLand:::.openland_try_download_and_load_rda(url,
    object = "a",
    envir = env,
    cache = TRUE,
    cache_dir = cache_dir,
    timeout = 1,
    quiet = TRUE
  )

  expect_true(ok)
  expect_true(exists("a", envir = env, inherits = FALSE))
  expect_identical(env$a, 123)
})


test_that(".openland_try_download_and_load_rda handles missing .rda extension", {
  cache_dir <- tempfile(pattern = "OpenLand-cache-")
  dir.create(cache_dir)

  url <- "https://example.com/foo"
  cached_path <- file.path(cache_dir, "foo.rda")

  b <- 999
  save(b, file = cached_path)

  env <- new.env(parent = emptyenv())
  ok <- OpenLand:::.openland_try_download_and_load_rda(url,
    object = "b",
    envir = env,
    cache = TRUE,
    cache_dir = cache_dir,
    timeout = 1,
    quiet = TRUE
  )

  expect_true(ok)
  expect_true(exists("b", envir = env, inherits = FALSE))
  expect_identical(env$b, 999)
})


test_that(".openland_try_download_and_load_rda invalidates bad cache then downloads from file URL", {
  as_file_url <- function(path) {
    p <- normalizePath(path, winslash = "/", mustWork = TRUE)
    if (grepl("^[A-Za-z]:", p)) paste0("file:///", p) else paste0("file://", p)
  }

  src <- tempfile(pattern = "OpenLand-src-", fileext = ".rda")
  b <- 42
  save(b, file = src)
  url <- as_file_url(src)

  cache_dir <- tempfile(pattern = "OpenLand-cache-")
  dir.create(cache_dir)

  cached_path <- file.path(cache_dir, basename(sub("\\?.*$", "", url)))
  a <- 7
  save(a, file = cached_path)

  env <- new.env(parent = emptyenv())
  ok <- OpenLand:::.openland_try_download_and_load_rda(url,
    object = "b",
    envir = env,
    cache = TRUE,
    cache_dir = cache_dir,
    timeout = 5,
    quiet = TRUE
  )

  expect_true(ok)
  expect_true(exists("b", envir = env, inherits = FALSE))
  expect_identical(env$b, 42)

  env_cached <- new.env(parent = emptyenv())
  load(cached_path, envir = env_cached)
  expect_true(exists("b", envir = env_cached, inherits = FALSE))
})


test_that(".openland_try_download_and_load_rda cache=FALSE still loads from file URL", {
  as_file_url <- function(path) {
    p <- normalizePath(path, winslash = "/", mustWork = TRUE)
    if (grepl("^[A-Za-z]:", p)) paste0("file:///", p) else paste0("file://", p)
  }

  src <- tempfile(pattern = "OpenLand-src-", fileext = ".rda")
  c_obj <- "ok"
  save(c_obj, file = src)
  url <- as_file_url(src)

  cache_dir <- tempfile(pattern = "OpenLand-cache-")
  dir.create(cache_dir)

  env <- new.env(parent = emptyenv())
  ok <- OpenLand:::.openland_try_download_and_load_rda(url,
    object = "c_obj",
    envir = env,
    cache = FALSE,
    cache_dir = cache_dir,
    timeout = 5,
    quiet = TRUE
  )

  expect_true(ok)
  expect_true(exists("c_obj", envir = env, inherits = FALSE))
  expect_identical(env$c_obj, "ok")

  cached_path <- file.path(cache_dir, basename(sub("\\?.*$", "", url)))
  expect_false(file.exists(cached_path))
})


test_that(".openland_try_download_and_load_rda returns FALSE for invalid url", {
  env <- new.env(parent = emptyenv())
  expect_false(OpenLand:::.openland_try_download_and_load_rda("", envir = env))
  expect_false(OpenLand:::.openland_try_download_and_load_rda(NA_character_, envir = env))
})

# file.remove(list.files(testFolder, pattern = "landscape", full.names = TRUE))
