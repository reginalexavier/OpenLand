# Test for contingencyTable function
test_that("contingencyTable works correctly", {
  skip_if_not_installed("terra")
  
  # Basic test with sample data
  data(SL_2002_2014)
  expect_s4_class(SL_2002_2014, "SpatRaster")
  
  # Test contingencyTable function
  result <- contingencyTable(input_raster = SL_2002_2014, 
                           pixelresolution = 30)
  
  expect_s3_class(result, "OpenLand")
  expect_true("lulc_Multistep" %in% names(result))
  expect_true(is.matrix(result$lulc_Multistep[[1]]))
})

test_that("contingencyTable handles errors gracefully", {
  # Test with invalid input
  expect_error(contingencyTable(input_raster = NULL))
  expect_error(contingencyTable(input_raster = "invalid"))
})
