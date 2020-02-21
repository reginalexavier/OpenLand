context("intensityClass & generic_method")

set.seed(159)
demo_raster <- .demo_landscape(year =  2000:2005,
                               res = 1,
                               crs = "+proj=utm +zone=21 +south +ellps=GRS80 +units=m +no_defs")
demo_cont <- contingencyTable(demo_raster, pixelresolution = 1)

demo_int_pixel <-
  intensityAnalysis(
    dataset = demo_cont,
    category_n = "GUP",
    category_m = "SXQ",
    area_km2 = FALSE
  )


test_that("Behavior of the 3 s4 classes", {



  expect_visible(
  new("Interval", intervalData = demo_int_pixel$interval_lvl$intervalData))
  expect_visible(
  new("Category",
      categoryData = demo_int_pixel$category_lvlGain$categoryData,
      lookupcolor = demo_int_pixel$category_lvlGain$lookupcolor,
      categoryStationarity = demo_int_pixel$category_lvlGain$categoryStationarity))
  expect_visible(
  new("Transition",
      transitionData = demo_int_pixel$transition_lvlGain_n$transitionData,
      lookupcolor = demo_int_pixel$transition_lvlGain_n$lookupcolor,
      transitionStationarity = demo_int_pixel$transition_lvlGain_n$transitionStationarity))


  expect_s4_class(
    new("Interval", intervalData = demo_int_pixel$interval_lvl$intervalData),
    "Interval"
  )
  expect_s4_class(
    new("Category",
        categoryData = demo_int_pixel$category_lvlGain$categoryData,
        lookupcolor = demo_int_pixel$category_lvlGain$lookupcolor,
        categoryStationarity = demo_int_pixel$category_lvlGain$categoryStationarity),
    "Category"
  )
  expect_s4_class(
    new("Transition",
        transitionData = demo_int_pixel$transition_lvlGain_n$transitionData,
        lookupcolor = demo_int_pixel$transition_lvlGain_n$lookupcolor,
        transitionStationarity = demo_int_pixel$transition_lvlGain_n$transitionStationarity),
    "Transition"
  )
  expect_error(
    new("Interval", intervalData = demo_int_pixel$interval_lvl$intervalData[1:2]))
  expect_error(
    new("Category",
        categoryData = demo_int_pixel$category_lvlGain$categoryData[1:4],
        lookupcolor = demo_int_pixel$category_lvlGain$lookupcolor,
        categoryStationarity = demo_int_pixel$category_lvlGain$categoryStationarity))
  expect_error(
    new("Transition",
        transitionData = demo_int_pixel$transition_lvlGain_n$transitionData[1:4],
        lookupcolor = demo_int_pixel$transition_lvlGain_n$lookupcolor,
        transitionStationarity = demo_int_pixel$transition_lvlGain_n$transitionStationarity))



  expect_error(
    new("Interval", intervalData = as.data.frame(demo_int_pixel$interval_lvl$intervalData)))
  expect_error(
    new("Category",
        categoryData = as.data.frame(demo_int_pixel$category_lvlGain$categoryData),
        lookupcolor = demo_int_pixel$category_lvlGain$lookupcolor,
        categoryStationarity = demo_int_pixel$category_lvlGain$categoryStationarity))
  expect_error(
    new("Transition",
        transitionData = as.data.frame(demo_int_pixel$transition_lvlGain_n$transitionData),
        lookupcolor = demo_int_pixel$transition_lvlGain_n$lookupcolor,
        transitionStationarity = demo_int_pixel$transition_lvlGain_n$transitionStationarity))



})




test_that("Behavior of the acessor", {

  expect_equal(demo_int_pixel$interval_lvl@intervalData,
               demo_int_pixel$interval_lvl$intervalData)


  expect_equal(demo_int_pixel$category_lvlGain@lookupcolor,
               demo_int_pixel$category_lvlGain$lookupcolor)

  expect_equal(demo_int_pixel$transition_lvlGain_n@lookupcolor,
               demo_int_pixel$transition_lvlGain_n$lookupcolor)

})
