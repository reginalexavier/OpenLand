context("input_rasters")

set.seed(159)

demo_raster <- .demo_landscape(2000:2005, res = 1)


test_that("Behavior of the input_raster", {

  expect_error(.input_rasters(c(.demo_landscape(year = 2000),
                                .demo_landscape(year = 2002, xmx = 50))))

})
