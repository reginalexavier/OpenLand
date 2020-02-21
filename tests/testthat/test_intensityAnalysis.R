context("intensityAnalysis")

set.seed(159)
demo_raster <- .demo_landscape(year =  2000:2005,
                               res = 1,
                               crs = "+proj=utm +zone=21 +south +ellps=GRS80 +units=m +no_defs")
demo_cont <- contingencyTable(demo_raster, pixelresolution = 1)



test_that("Behavior of the intensityAnalysis", {


  demo_int_km2 <-
    intensityAnalysis(
      dataset = demo_cont,
      category_n = "GUP",
      category_m = "SXQ",
      area_km2 = TRUE
    )

  demo_int_pixel <-
    intensityAnalysis(
      dataset = demo_cont,
      category_n = "GUP",
      category_m = "SXQ",
      area_km2 = FALSE
    )


  expect_visible(intensityAnalysis(
    dataset = demo_cont,
    category_n = "GUP",
    category_m = "SXQ",
    area_km2 = FALSE
  ))

  expect_visible(intensityAnalysis(
    dataset = demo_cont,
    category_n = "GUP",
    category_m = "SXQ",
    area_km2 = TRUE
  ))


  expect_equal(demo_int_km2$interval_lvl$intervalData$U,
               demo_int_pixel$interval_lvl$intervalData$U)

  expect_equal(demo_int_km2$interval_lvl$intervalData$PercentChange[3], 80.26)


  expect_equal(demo_int_km2$category_lvlGain$categoryData$GG_km2,
               demo_int_pixel$category_lvlGain$categoryData$GG_pixel/1000000)
  expect_equal(demo_int_km2$category_lvlLoss$categoryData$GL_km2,
               demo_int_pixel$category_lvlLoss$categoryData$GL_pixel/1000000)

  expect_equal(demo_int_pixel$category_lvlGain$categoryData$GG_pixel[8], 1587)
  expect_equal(demo_int_pixel$category_lvlLoss$categoryData$GL_pixel[10], 1684)

  expect_equal(demo_int_km2$transition_lvlGain_n$transitionData$T_i2n_km2,
               demo_int_pixel$transition_lvlGain_n$transitionData$T_i2n_pixel/1000000)
  expect_equal(demo_int_km2$transition_lvlLoss_m$transitionData$T_m2j_km2,
               demo_int_pixel$transition_lvlLoss_m$transitionData$T_m2j_pixel/1000000)

  expect_equal(demo_int_pixel$transition_lvlGain_n$transitionData$T_i2n_pixel[18], 380)
  expect_equal(demo_int_pixel$transition_lvlLoss_m$transitionData$T_m2j_pixel[15], 382)

  #stationarity

  expect_equal(demo_int_km2$category_lvlGain$categoryStationarity$Stationarity,
               demo_int_pixel$category_lvlGain$categoryStationarity$Stationarity)
  expect_equal(demo_int_km2$category_lvlLoss$categoryStationarity$Stationarity,
               demo_int_pixel$category_lvlLoss$categoryStationarity$Stationarity)

  expect_equal(demo_int_km2$transition_lvlGain_n$transitionStationarity$Stationarity,
               demo_int_pixel$transition_lvlGain_n$transitionStationarity$Stationarity)
  expect_equal(demo_int_km2$transition_lvlLoss_m$transitionStationarity$Stationarity,
               demo_int_pixel$transition_lvlLoss_m$transitionStationarity$Stationarity)

  #class

  expect_s4_class(demo_int_km2$interval_lvl, "Interval")
  expect_s4_class(demo_int_pixel$interval_lvl, "Interval")

  expect_s4_class(demo_int_km2$category_lvlGain, "Category")
  expect_s4_class(demo_int_pixel$category_lvlGain, "Category")
  expect_s4_class(demo_int_km2$category_lvlLoss, "Category")
  expect_s4_class(demo_int_pixel$category_lvlLoss, "Category")

  expect_s4_class(demo_int_km2$transition_lvlGain_n, "Transition")
  expect_s4_class(demo_int_pixel$transition_lvlGain_n, "Transition")
  expect_s4_class(demo_int_km2$transition_lvlLoss_m, "Transition")
  expect_s4_class(demo_int_pixel$transition_lvlLoss_m, "Transition")


})
