#' @include demolandscape.R
NULL

#' Contingence Table
#'
#' @param input_raster a raster list
#' @param pixelresolution the pixel resolution in meter
#'
#' @import dplyr
#' @importFrom grDevices hcl.colors
#'
#' @return a list of table contengency
#' @export
#'
#' @examples
#'
#'contingenceTable(demo_landscape(year = 2000:2005, res = 1,
#'prob = c(0.05, 0.3, 0.05, 0.4, 0.2)), pixelresolution = 1)

contingenceTable <-
  function(input_raster, pixelresolution = 30) {
    #importing the rasters
    if ((c(class(input_raster[[1]]))) == "RasterLayer") {
      rList <- input_raster
    } else if (class(input_raster) == "character") {
      raster_files <-
        list.files(input_raster,
                   pattern = ".tif$",
                   full.names = T)

      rList <- vector("list", length = length(raster_files))


      for (i in seq_along(raster_files)) {
        rList[[i]] <- raster::raster(raster_files[i])
      }
    } #else {
    #stop("The input have to be a folder of '.tif' forder or a list of 'Raster Layer'")
    #}
    #testing if the raster are similar in nrow, ncol and crs
    extent_test <-
      all(mapply(
        function(x, y)
          raster::compareRaster(
            x,
            y,
            extent = TRUE,
            rowcol = TRUE,
            crs = TRUE,
            res = FALSE,
            orig = FALSE,
            rotation = TRUE,
            values = FALSE,
            stopiffalse = FALSE,
            showwarning = FALSE
          ),
        rList[1:(length(rList) - 1)],
        rList[2:length(rList)]
      ))

    Year_from <- Year_to <- strings01 <- strings02 <-
      yearTo <- yearFrom <-
      QtPixel <- Period <- From <- To <- km2 <- interval <- NULL

    if (!extent_test) {
      stop("The rasters have differents nrow, ncol and/or src, please edit the files and retry!")
    } else {
      #how to compute the cross table of two layers, then setting the columns name???
      lulc <- list("oneStep", "multiStep")
      table_cross <- function(x, y) {
        contengency <-
          raster::crosstab(x, y, long = TRUE, progress = "text")
        contengency %>% dplyr::mutate(Year_from = colnames(contengency)[1],
                               Year_to = colnames(contengency)[2]) %>%
          dplyr::rename(
            From = colnames(contengency)[1],
            To = colnames(contengency)[2],
            QtPixel = colnames(contengency)[3]
          )
      }
      if (length(rList) > 2) {
        lulc[[1]] <- table_cross(rList[[1]], rList[[length(rList)]])
      }
      #compute a serie of contengency table iteratively over the whole list of raster
      lulc[[2]] <-
        Reduce(rbind,
               mapply(function(x, y)
                 table_cross(x, y), rList[1:(length(rList) - 1)], rList[2:length(rList)], SIMPLIFY = FALSE))
    }
    lulctable <-
      lapply(lulc, function(x)
        x %>% dplyr::arrange(Year_from) %>%
          tidyr::separate(Year_from, c("strings01", "yearFrom"), sep = "_") %>%
          tidyr::separate(Year_to, c("strings02", "yearTo"), sep = "_") %>%
          dplyr::select(-strings01, -strings02) %>%
          dplyr::mutate(interval = as.numeric(yearTo) - as.numeric(yearFrom)) %>%
          dplyr::mutate(km2 = QtPixel * (pixelresolution ^ 2) / 1000000) %>%
          tidyr::unite("Period", c("yearFrom", "yearTo"), sep = "-", remove = FALSE) %>%
          dplyr::select(Period, From, To, km2, interval, QtPixel, yearFrom, yearTo))

    #calculating the total interval and the pixelValue
    allinterval <-
      as.numeric(dplyr::last(lulctable[[2]]$yearTo)) - as.numeric(dplyr::first(lulctable[[2]]$yearFrom))

    tb_legend <-
      lulctable[[2]] %>% dplyr::distinct(From) %>% dplyr::rename(classValue = From)

    genclass <- function() {paste(sample(LETTERS, size = 3, replace = FALSE), collapse = "")}

    tb_legend$className <- as.factor(vapply(seq_len(nrow(tb_legend)), function(x) genclass(), character(1)))
    tb_legend$color <- hcl.colors(nrow(tb_legend), palette = "Blue-Red 3",
                                  alpha = NULL, rev = FALSE, fixup = TRUE)

    areaTotal <-
      lulctable[[2]] %>% dplyr::group_by(Period) %>% dplyr::summarise(area_km2 = sum(km2), QtPixel = sum(QtPixel))

    contengenceTable <-
      list(
        lulc_Mulstistep = tibble::as_tibble(lulctable[[2]]),
        tb_legend = tibble::as_tibble(tb_legend),
        totalArea = areaTotal[[1, 2]],
        lulc_Onstep = tibble::as_tibble(lulctable[[1]]),
        totalInterval = allinterval
      )
    return(contengenceTable)
  }
