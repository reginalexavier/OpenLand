context("demo_landscape")


test_that("whether the demo gives the same output", {

  set.seed(159)

  demo_raster <- .demo_landscape(2000:2005, res = 1)

  expect_equal(c(class(demo_raster[[1]])), "RasterLayer")

  expect_equal(length(demo_raster), 6)


})
