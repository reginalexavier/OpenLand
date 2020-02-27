context("contingencyTable")

set.seed(159)
demo_raster <- .demo_landscape(year =  2000:2005,
                               res = 1,
                               crs = "+proj=utm +zone=21 +south +ellps=GRS80 +units=m +no_defs")
demo_cont <- contingencyTable(demo_raster, pixelresolution = 1)



test_that("Behavior of the contingencyTable", {

  expect_error(contingencyTable(
    input_raster = c(
      .demo_landscape(year = 2000),
      .demo_landscape(year = 2002, xmx = 50)
    ),
    pixelresolution = 1
  ))

  expect_error(contingencyTable(
    input_raster = .demo_landscape(year = 2000),
    pixelresolution = 1
  ))

  expect_error(contingencyTable(input_raster = 5L))

  expect_error(contingencyTable(
    input_raster = c(
      .demo_landscape(year = 2000,
                      crs = "+proj=utm +zone=22 +south +ellps=GRS80 +units=m +no_defs"),
      .demo_landscape(year = 2002,
                      crs = "+proj=utm +zone=21 +south +ellps=GRS80 +units=m +no_defs")
    ),
    pixelresolution = 1
  ))

  expect_silent(contingencyTable(.demo_landscape(2000:2001)))
  expect_silent(contingencyTable(demo_raster, pixelresolution = 1))
  expect_silent(contingencyTable(raster::stack(demo_raster), pixelresolution = 1))
  expect_silent(contingencyTable(raster::brick(demo_raster), pixelresolution = 1))
  expect_length(demo_cont, 5)
  expect_output(str(demo_cont), "List of 5")
  expect_equal(ncol(demo_cont$lulc_Multistep), 8)
  expect_named(demo_cont$lulc_Multistep, colnames(demo_cont$lulc_Onestep))
  expect_equal(
    head(demo_cont$lulc_Multistep$yearFrom, 1),
    head(demo_cont$lulc_Onestep$yearFrom, 1)
  )
  expect_equal(tail(demo_cont$lulc_Multistep$yearTo, 1),
               head(demo_cont$lulc_Onestep$yearTo, 1))
  expect_equal(
    head(demo_cont$lulc_Onestep$yearTo, 1) - head(demo_cont$lulc_Multistep$yearFrom, 1),
    demo_cont$totalInterval
  )
  expect_equal(class(demo_cont$tb_legend$categoryName), "factor")
  expect_type(demo_cont$tb_legend$categoryName, "integer")
  expect_equal(as.character(demo_cont$tb_legend$categoryName[3]), "OZS")
  expect_equal(class(demo_cont$tb_legend$color), "character")
  expect_equal(demo_cont$tb_legend$color[4], "#EAACAC")
})
