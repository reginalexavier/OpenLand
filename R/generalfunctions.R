
#' Summary of multiple parameters in a raster directory
#'
#' Comparring the extent of rasters
#'
#' @param path The path for a raster directory/list to be analysed
#'
#' @return Table with the parameters in columns
#' @export
#'
#' @examples
#'
#' summary_dir(demo_landscape(2000:2005, res = 1, prob = c(0.05, 0.3, 0.05, 0.4, 0.2)))
#'
#'
summary_dir <- function(path) {

  if ((class(path) == "list") &
      (c(class(path[[1]])) == "RasterLayer")) {
    layer_list <- path
  } else if (class(path) == "character") {

    raster_files <-
      list.files(path,
                 pattern = ".tif$",
                 full.names = T)

    layer_list <- vector("list", length = length(raster_files))


    for (i in seq_along(raster_files)) {
      layer_list[[i]] <- raster::raster(raster_files[i])
    }
  }

  Reduce(rbind,
         lapply(layer_list, function(x) {
           layermap <- x
           tibble(
             file_name = base::names(x),
             xmin = raster::xmin(layermap),
             xmax = raster::xmax(layermap),
             ymin = raster::ymin(layermap),
             ymax = raster::ymax(layermap),
             res_x = raster::res(layermap)[1],
             res_y = raster::res(layermap)[2],
             nrow = raster::nrow(layermap),
             ncol = raster::ncol(layermap),
             min_val = raster::minValue(layermap),
             max_val = raster::maxValue(layermap),
             crs = as.character(raster::crs(layermap))
           )
         }))
}


#' Quick summary of categorical raster raster
#'
#' @param path The path for the raster to be analysed
#'
#' @return A table containing in columns each pixel value and its quantity
#'
#' @export
#'
#' @examples
#'
#' summary_map(demo_landscape(2000, res = 1, prob = c(0.05, 0.3, 0.05, 0.4, 0.2))[[1]])
summary_map <- function(path) {
  rastermap <-
    if (class(path) != "character") {
      path
    } else {
      raster::raster(path)
    }
  value_map <- table(raster::values(rastermap))

  tbfinal <- dplyr::tibble(pixvalue = numeric(), Qt = numeric())

  for (i in seq_along(value_map)) {
    tbfinal[i, c(1:2)] <-
      c(as.numeric(names(value_map)[i]), value_map[[i]])
  }
  return(tbfinal)
}



#' Accumulate changes in a LULC time serie raster
#'
#' This fucntion return two maps, the first containing the sequence of change as
#' pixel value (answering: in what point of time a pixel has changes?) and second
#' the quantity time of change as pixel value(answering: how many times a pixel
#' has change). These maps can be plotted  with the plot function method of the
#' raster package or with the `tmap` package.
#'
#' @param path list (of filenames or Raster* objects), RasterStack(\code{\link[raster]{brick}}) or
#' RasterStack(\code{\link[raster]{stack}})
#'
#' @return Two maps, the first containing the sequence of change as pixel value and second
#'     the quantity time of change as pixel value.
#' @export
#'
#' @examples
#' test <- demo_landscape(2000:2005, res = 1, prob = c(0.05, 0.3, 0.05, 0.4, 0.2))
#' acc_changes(test)
#' acc_changes(raster::stack(test))
#' acc_changes(raster::brick(test))
#'
acc_changes <- function(path) {

  #importing the rasters
  if (c(class(path)) %in% c("RasterStack", "RasterBrick")) {

    rList  <- raster::unstack(path)

  } else if ((c(class(path[[1]]))) == "RasterLayer") {

    rList <- path

  } else if (class(path) == "character") {
    raster_files <- list.files(path, pattern = ".tif$", full.names = T)

    rList <- vector("list", length = length(raster_files))

    for (i in seq_along(raster_files)) {
      rList[[i]] <- raster::raster(raster_files[i])
    }
  } else {
    stop("The input can only be a `RasterStack`, `RasterBrick`, a list of `RasterLayer` or
         a path directory of rasters `.tif` ")
  }

  difflist <- mapply(
    function(x, y, z)
      raster::overlay(
        x,
        y,
        fun = function(x1, x2)
          ifelse((x1 != x2), z, 0)
      ),
    x = rList[1:(length(rList) - 1)],
    y = rList[2:length(rList)],
    z = 1:(length(rList) - 1),
    SIMPLIFY = FALSE
  )
  # capturing the sequence of the changes by pixel, reducing all maps in difflist
  seq_change <- Reduce(function(a, b)
    raster::overlay(
      a,
      b,
      fun = function(y1, y2) {
        value <-
          as.numeric(ifelse((y2 != 0), paste0(y1, y2), paste0(y1))) * 1
      }
    ), difflist)
  # how many times a pixel change in the interval analysed???
  qt_change <- raster::overlay(
    seq_change,
    fun = function(x)
      value <- nchar(x)
  )

  return(list(seq_change, qt_change))
}
