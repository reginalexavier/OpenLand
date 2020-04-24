context("input_rasters")

# testFolder <- tempdir()
#
# lapply(.demo_landscape(year = 2000:2004), function(x)
#   raster::writeRaster(x,
#                       filename = file.path(testFolder, paste0(names(x), ".tif")),
#                       datatype = 'INT1U',
#                       overwrite = TRUE
#   ))


test_that("Behavior of the input_raster", {

  #expect_silent(.input_rasters(testFolder))

  expect_error(.input_rasters(c(.demo_landscape(year = 2000),
                                .demo_landscape(year = 2002, xmx = 50))))

  expect_silent(.input_rasters(.demo_landscape(year = 2000)[[1]]))
  expect_silent(.input_rasters(.demo_landscape(year = 2000)))
  expect_error(.input_rasters(raster::unstack(.input_rasters(.demo_landscape(year = 2000:2004)))))

  expect_error(.input_rasters(list(a = 1, b = 2)))

})


#file.remove(list.files(testFolder, pattern = "landscape", full.names = TRUE))


# test_that("Behavior for case maps not found in the input_raster function", {
#
#   expect_error(.input_rasters(testFolder))
#
#   })
