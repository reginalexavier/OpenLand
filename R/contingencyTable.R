#' @include demolandscape.R
NULL

#' Contingence Table
#'
#' @param input_raster list (of filenames or Raster* objects),
#' RasterStack(\code{\link[raster]{brick}}) or RasterStack(\code{\link[raster]{stack}})
#' @param pixelresolution numeric. The pixel resolution in meter.
#'
#'
#'
#'
#' @import dplyr
#'
#' @return A list that contains 5 objects.
#' \itemize{
#'   \item \code{lulc_Mulstistep}: \code{<tibble>}  The table of contingency between all step of time analysed, contains 8 columns:
#'   \enumerate{
#'   \item Period: \code{<chr>} The period \emph{[Yt, Yt+1]}.
#'   \item From: \code{<dbl>} a category \emph{i}.
#'   \item To: \code{<dbl>} a category \emph{j}.
#'   \item km2: \code{<dbl>} The quantity in kilometer that transit from the classes category \emph{i}
#'    to category \emph{j} in the period \emph{[Yt, Yt+1]}.
#'   \item Interval: \code{<dbl>} The number of time point between the first and
#'    the last year of the period \emph{[Yt, Yt+1]}.
#'   \item QtPixel: \code{<int>} The quantity of pixel that transit from the classes category \emph{i}
#'    to category \emph{j} in the period \emph{[Yt, Yt+1]}.
#'   \item yearFrom: \code{<chr>} The year that the change come from \emph{[Yt]}
#'   \item yearTo: \code{<chr>} The year that the change go for \emph{[Yt+1]}
#'   }
#'   \item \code{lulc_Onestep}:\code{<tibble>} The table of contingency between the
#'   first and the last year analysed \emph{[Yt, Yt+1]}, contains
#'   8 columns like \code{lulc_Mulstistep}.
#'   \item \code{tb_legend}: \code{<tibble>} A table of the pixel value his name and color, contains 3 columns:
#'   \enumerate{
#'   \item classValue: \code{<dbl>} the pixel value of the classes of land use
#'   \item className: \code{<factor>} The name or legend associate with a given pixel value, here as factor with a specific level for blablabla
#'   \item color: \code{<chr>} The color associate with the given pixel value.
#'   Befor others step of analysis, one would want to change the \code{className} and \code{color} values. Some details.
#'     \itemize{
#'         \item the class name have to be  in the same order of the \code{classValue}
#'         column, the \code{levels} the order that the graphs legend will have to be printed. Like:
#'         \preformatted{
#'
#'         myobject$tb_legend$className <- factor(c("name1", "name2", "name3", "name4"),
#'                                                levels = c("name3", "name2", "name1", "name4"))}
#'         \item the color by the same order of the \code{classValue} comuln, it can be a
#'        colour name (eg. "black") or an HEX value (eg. #FFFFFF). Like:
#'        \preformatted{
#'
#'        myobject$tb_legend$color <- c("#CDB79E", "red", "#66CD00", "yellow")}}}
#'   \item \code{totalArea}: \code{<tibble>}  A table of two columns with the total area of study, contains 2 columns:
#'   \enumerate{
#'   \item area_km2: \code{<numeric>} The total area of study in square meter.
#'   \item QtPixel: \code{<numeric>} The total area of study in quantity of pixel
#'   }
#'   \item \code{totalInterval}: \code{<numeric>} The value represinting the whole time points
#'   interval of time analysed \emph{[Yt, Yt+1]}, in years.
#'   }
#'
#'
#'
#' @export
#'
#' @examples
#'
#'contingenceTable(demo_landscape(year = 2000:2005, res = 1), pixelresolution = 1)

contingenceTable <-
  function(input_raster, pixelresolution = 30) {
    #importing the rasters
    if (c(class(input_raster)) %in% c("RasterStack", "RasterBrick")) {

      rList  <- raster::unstack(input_raster)

    } else if ((c(class(input_raster[[1]]))) == "RasterLayer") {

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
    } else {
    stop("The input can only be a `RasterStack`, `RasterBrick`, a list of `RasterLayer` or
         a path directory of rasters `.tif` ")
    }
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
      QtPixel <- Period <- From <- To <- km2 <- Interval <- NULL

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
          ) %>% dplyr::mutate(From = as.integer(From), To = as.integer(To))
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
          dplyr::mutate(yearFrom = as.integer(yearFrom), yearTo = as.integer(yearTo),
                        Interval = yearTo - yearFrom) %>%
          dplyr::mutate(km2 = QtPixel * (pixelresolution ^ 2) / 1000000) %>%
          tidyr::unite("Period", c("yearFrom", "yearTo"), sep = "-", remove = FALSE) %>%
          dplyr::select(Period, From, To, km2, QtPixel, Interval, yearFrom, yearTo))

    #calculating the total interval and the pixelValue
    allinterval <-
      as.numeric(dplyr::last(lulctable[[2]]$yearTo)) - as.numeric(dplyr::first(lulctable[[2]]$yearFrom))

    tb_legend <-
      lulctable[[2]] %>% dplyr::distinct(From) %>% dplyr::rename(classValue = From)

    genclass <- function() {paste(sample(LETTERS, size = 3, replace = FALSE), collapse = "")}

    tb_legend$className <- as.factor(vapply(seq_len(nrow(tb_legend)), function(x) genclass(), character(1)))

    if (version$major <= 3 & version$minor < 6.0) {

      tb_legend$color <- base::sample(grDevices::colors(), nrow(tb_legend), replace = F)

    } else {

      tb_legend$color <- grDevices::hcl.colors(nrow(tb_legend), palette = "Blue-Red 3", alpha = NULL, rev = FALSE, fixup = TRUE)

    }

    areaTotal <-
      lulctable[[2]] %>% dplyr::group_by(Period) %>% dplyr::summarise(area_km2 = sum(km2), QtPixel = sum(QtPixel))

    contingenceTable <-
      list(
        lulc_Multistep = tibble::as_tibble(lulctable[[2]]),
        lulc_Onestep = tibble::as_tibble(lulctable[[1]]),
        tb_legend = tibble::as_tibble(tb_legend),
        totalArea = areaTotal[1, c(2,3)],
        totalInterval = allinterval
      )
    return(contingenceTable)
  }
