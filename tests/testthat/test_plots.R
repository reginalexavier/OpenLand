context("The plots")

test_that("Behavior of the plot methods", {

  set.seed(159)

  demo_raster <- demo_landscape(2000:2005, res = 1)

  demo_cont <- contingencyTable(demo_raster, pixelresolution = 1)

  demo_int_km2 <-
    intensityAnalysis(
      dataset = demo_cont,
      class_n = "GUP",
      class_m = "SXQ",
      area_km2 = TRUE
    )

  expect_invisible(OpenLand::plot(demo_int_km2$interval_lvl))
  expect_output(str(OpenLand::plot(demo_int_km2$interval_lvl)), "gtable, containing")



  expect_invisible(OpenLand::plot(demo_int_km2$category_lvlGain))
  expect_output(str(OpenLand::plot(demo_int_km2$category_lvlGain)), "gtable, containing")



  expect_invisible(OpenLand::plot(demo_int_km2$transition_lvlGain_n))
  expect_output(str(OpenLand::plot(demo_int_km2$transition_lvlGain_n)), "gtable, containing")


})



test_that("Behavior of the other plots", {

  set.seed(159)

  demo_raster <- demo_landscape(2000:2005, res = 1)

  demo_cont <- contingencyTable(demo_raster, pixelresolution = 1)



  expect_visible(barplotLand(demo_cont$lulc_Multistep, demo_cont$tb_legend))
  expect_invisible(chordDiagramLand(demo_cont$lulc_Onestep, demo_cont$tb_legend))
  expect_visible(netgrossplot(demo_cont$lulc_Multistep, demo_cont$tb_legend))
  expect_visible(sankeyLand(demo_cont$lulc_Onestep, demo_cont$tb_legend))
  expect_visible(sankeyLand(demo_cont$lulc_Multistep, demo_cont$tb_legend))



  expect_output(str(barplotLand(demo_cont$lulc_Multistep, demo_cont$tb_legend)), "List of 9")
  #expect_null(str(chordDiagramLand(demo_cont$lulc_Onestep, demo_cont$tb_legend)))
  expect_output(str(netgrossplot(demo_cont$lulc_Multistep, demo_cont$tb_legend)), "List of 9")
  expect_output(str(sankeyLand(demo_cont$lulc_Onestep, demo_cont$tb_legend)), "List of 8")
  expect_output(str(sankeyLand(demo_cont$lulc_Multistep, demo_cont$tb_legend)), "List of 8")


})
