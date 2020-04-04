context("generalfunctions")

set.seed(159)
demo_raster <- .demo_landscape(year =  2000:2005,
                               res = 1,
                               crs = "+proj=utm +zone=21 +south +ellps=GRS80 +units=m +no_defs")
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
  expect_equal(sum(demo_acc_changes[[2]][ , 3]), 100)
  expect_equal(demo_acc_changes[[2]][[4 , 2]], 1998)
  expect_equal(ncol(demo_acc_changes[[2]]), 3)

})


# testFolder <- tempdir()
#
# lapply(.demo_landscape(year = 2000:2004), function(x)
#   raster::writeRaster(x,
#                       filename = file.path(testFolder, paste0(names(x), ".tif")),
#                       datatype = 'INT1U',
#                       overwrite = TRUE
#   ))

test_that("Behavior of summary_dir", {

  # expect_silent(summary_dir(testFolder))
  # expect_equal(nrow(summary_dir(testFolder)), 5)
  expect_silent(summary_dir(demo_raster))
  expect_visible(summary_dir(demo_raster))
  expect_error(summary_dir(raster::stack(demo_raster)))
  expect_error(summary_dir(raster::brick(demo_raster)))
  expect_equal(ncol(summary_dir(demo_raster)), 12)
  expect_equal(nrow(summary_dir(demo_raster)), length(demo_raster))
  expect_setequal(class(summary_dir(demo_raster)), c("tbl_df", "tbl", "data.frame"))

})


test_that("Behavior of summary_map", {

  # expect_silent(summary_map(list.files(testFolder, pattern = "tif$", full.names = TRUE)[1]))
  # expect_silent(summary_map(.input_rasters(testFolder)))
  expect_silent(summary_map(demo_raster[[1]]))
  expect_visible(summary_map(demo_raster[[1]]))
  expect_equal(ncol(summary_map(demo_raster[[1]])), 2)
  expect_equal(nrow(summary_map(demo_raster[[1]])), 5)


})

# file.remove(list.files(testFolder, pattern = "landscape", full.names = TRUE))

