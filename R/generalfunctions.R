
#' Summary of multiple parameters in a raster directory
#'
#' Listing major charateristics of raster inputs
#'
#' @param path The path for a raster directory/list to be analysed
#'
#' @return Table with the raster parameters in columns
#' @export
#'
#' @examples
#'
#' summary_dir(raster::unstack(SaoLourencoBasin))
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


#' Summary of major characteristics of a unique categorical raster
#'
#' @param path The path for the raster to be analysed
#'
#' @return A table containing in columns the pixel counts for each pixel value
#'
#' @export
#'
#' @examples
#' \donttest{summary_map(SaoLourencoBasin[[1]])}
#'
summary_map <- function(path) {
  rastermap <-
    if (class(path) != "character") {
      if (class(path) == "RasterLayer") {
        path
      } else {
        path[[1]]
      }
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



#' Accumulates changes in a LULC raster time series
#'
#' This function calculates the number of times a pixel has changed during the
#' analysed period. It returns a raster with the number of changes as pixel value
#' and a table containing the areal percentage of every pixel value (number of changes).
#'
#'
#' @param path list. List of filenames, list of Raster* objects, RasterBrick(\code{\link[raster]{brick}}) or
#' RasterStack(\code{\link[raster]{stack}})
#'
#' @return Two objects, a raster layer and a table.
#' @export
#'
#'
#'
#' @examples
#' \donttest{acc_changes(SaoLourencoBasin)}
#'

acc_changes <- function(path) {
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

  n_raster <- length(rList)

  if (n_raster < 2) {
    stop('acc_changes needs at least 2 rasters')
  }

  difflist <- mapply(
    function(x, y)
      raster::overlay(
        x,
        y,
        fun = function(x1, x2)
          ifelse((x1 != x2), 1, 0)
      ),
    x = rList[1:(length(rList) - 1)],
    y = rList[2:length(rList)],
    SIMPLIFY = FALSE
  )

  sumraster <- sum(raster::stack(difflist))

  Freq <- Var1 <- NULL

  df01_values <- table(matrix(sumraster))

  df_values <- dplyr::mutate(data.frame(df01_values),
                             Var1 = as.character(Var1),
                             Var1 = as.integer(Var1),
                             Percent = Freq/sum(Freq)*100)

  df_values <- dplyr::as_tibble(df_values)

  names(df_values) <- c("PxValue", "Qt", "Percent")

  list(sumraster, df_values)

}


#' Gain and persistence area in a LULC raster time series
#'
#' This function calculates the gain of every category during the analysed period.
#' For a series containing n rasters it returns a list of (n - 1) rasters, i.e, a
#' raster for every interval.
#'
#' @param path list. List of filenames, list of Raster* objects, RasterBrick(\code{\link[raster]{brick}}) or
#' RasterStack(\code{\link[raster]{stack}})
#' @param persistence int. value to be assigned to a persistence pixel
#'
#' @return A RasterBrick
#' @export
#'
#' @examples
#' \donttest{gain_map(SaoLourencoBasin)}
lulc_gain <- function(path, persistence = 99) {
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
    stop(
      "The input can only be a `RasterStack`, `RasterBrick`, a list of `RasterLayer` or
         a path directory of rasters `.tif` "
    )
  }

  n_raster <- length(rList)

  if (n_raster < 2) {
    stop('gain_map needs at least 2 rasters')
  }

  gain_map <- raster::brick(mapply(
    function(x, y)
      raster::overlay(
        x,
        y,
        fun = function(x1, x2)
          ifelse((x1 != x2), x2, persistence)
      ),
    x = rList[1:(length(rList) - 1)],
    y = rList[2:length(rList)],
    SIMPLIFY = FALSE
  ))

}

#' Loss and persistence area in a LULC raster time series
#'
#' This function calculates the loss of every category during the analysed period.
#' For a series containing n rasters it returns a list of (n - 1) rasters, i.e, a
#' raster for every interval.
#'
#' @param path list. List of filenames, list of Raster* objects, RasterBrick(\code{\link[raster]{brick}}) or
#' RasterStack(\code{\link[raster]{stack}})
#' @param persistence int. value to be assigned to a persistence pixel
#'
#' @return A RasterBrick
#' @export
#'
#' @examples
#' \donttest{loss_map(SaoLourencoBasin)}
lulc_loss <- function(path, persistence = 99) {
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
    stop(
      "The input can only be a `RasterStack`, `RasterBrick`, a list of `RasterLayer` or
         a path directory of rasters `.tif` "
    )
  }

  n_raster <- length(rList)

  if (n_raster < 2) {
    stop('loss_map needs at least 2 rasters')
  }

  loss_map <- raster::brick(mapply(
    function(x, y)
      raster::overlay(
        x,
        y,
        fun = function(x1, x2)
          ifelse((x1 != x2), x1, persistence)
      ),
    x = rList[1:(length(rList) - 1)],
    y = rList[2:length(rList)],
    SIMPLIFY = FALSE
  ))


}
