context("The plots")

set.seed(159)
demo_raster <- .demo_landscape(year =  2000:2005,
                               res = 1,
                               crs = "+proj=utm +zone=21 +south +ellps=GRS80 +units=m +no_defs")
demo_cont <- contingencyTable(demo_raster, pixelresolution = 1)

demo_int_km2 <-
  intensityAnalysis(
    dataset = demo_cont,
    category_n = "GUP",
    category_m = "SXQ",
    area_km2 = TRUE
  )



test_that("Behavior of the plot methods", {

  expect_invisible(OpenLand::plot(demo_int_km2$interval_lvl))
  expect_invisible(OpenLand::plot(demo_int_km2$interval_lvl, title = "Main Title"))
  expect_output(str(OpenLand::plot(demo_int_km2$interval_lvl)), "gtable, containing")



  expect_invisible(OpenLand::plot(demo_int_km2$category_lvlGain))
  expect_invisible(OpenLand::plot(demo_int_km2$category_lvlGain, title = "Main Title"))
  expect_output(str(OpenLand::plot(demo_int_km2$category_lvlGain)), "gtable, containing")



  expect_invisible(OpenLand::plot(demo_int_km2$transition_lvlGain_n))
  expect_invisible(OpenLand::plot(demo_int_km2$transition_lvlGain_n, title = "Main Title"))
  expect_output(str(OpenLand::plot(demo_int_km2$transition_lvlGain_n)), "gtable, containing")


})



test_that("Behavior of the other plots", {

  expect_visible(barplotLand(demo_cont$lulc_Multistep, demo_cont$tb_legend, area_km2 = TRUE))
  expect_invisible(chordDiagramLand(demo_cont$lulc_Onestep, demo_cont$tb_legend, area_km2 = TRUE))
  expect_visible(netgrossplot(demo_cont$lulc_Multistep, demo_cont$tb_legend, area_km2 = TRUE))

  expect_visible(barplotLand(demo_cont$lulc_Multistep, demo_cont$tb_legend, area_km2 = FALSE))
  expect_invisible(chordDiagramLand(demo_cont$lulc_Onestep, demo_cont$tb_legend, area_km2 = FALSE))
  expect_visible(netgrossplot(demo_cont$lulc_Multistep, demo_cont$tb_legend, area_km2 = FALSE))

  expect_visible(sankeyLand(demo_cont$lulc_Onestep, demo_cont$tb_legend))
  expect_visible(sankeyLand(demo_cont$lulc_Multistep, demo_cont$tb_legend))



  expect_output(str(barplotLand(demo_cont$lulc_Multistep, demo_cont$tb_legend)), "List of 9")
  #expect_null(str(chordDiagramLand(demo_cont$lulc_Onestep, demo_cont$tb_legend)))
  expect_output(str(netgrossplot(demo_cont$lulc_Multistep, demo_cont$tb_legend)), "List of 9")
  expect_output(str(sankeyLand(demo_cont$lulc_Onestep, demo_cont$tb_legend)), "List of 8")
  expect_output(str(sankeyLand(demo_cont$lulc_Multistep, demo_cont$tb_legend)), "List of 8")


})
