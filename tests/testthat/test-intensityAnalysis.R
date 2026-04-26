# Test for intensityAnalysis function
test_that("intensityAnalysis works correctly", {
  skip_if_not_installed("terra")
  
  # Basic test with sample data
  data(SL_2002_2014)
  result_ct <- contingencyTable(input_raster = SL_2002_2014, 
                              pixelresolution = 30)
  
  # Test intensityAnalysis function
  result_ia <- intensityAnalysis(dataset = result_ct, 
                               category_n = "Ap", 
                               category_m = "SG")
  
  expect_s3_class(result_ia, "intensity")
  expect_true("interval_lvl" %in% names(result_ia))
  expect_true("category_lvlGain" %in% names(result_ia))
  expect_true("category_lvlLoss" %in% names(result_ia))
})

test_that("intensityAnalysis handles errors gracefully", {
  # Test with invalid input
  expect_error(intensityAnalysis(dataset = NULL))
  expect_error(intensityAnalysis(dataset = "invalid"))
})
