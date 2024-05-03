#' Create Raster with Random pixel Value
#'
#' This function creates a raster series with some setup like the layer name and
#' the sample value for the lulc
#'
#' @param year numeric. A vector of year, first and last included.
#' @param nrows numeric. nrows of the raster to be created.
#' @param ncols numeric. ncols of the raster to be created.
#' @param res numeric. the resolution of the raster to be created.
#' @param xmn numeric. x minimum extent.
#' @param xmx numeric. x maximum extent.
#' @param ymn numeric. y minimum extent.
#' @param ymx numeric. y maximum extent.
#' @param crs character. the coordinate referencing system.
#' @param category A numeric vector of the raster categories.
#' @param prob A numeric vector of the probability of occurrence for the
#' category list.
#'
#'
#' @seealso \code{\link[raster]{raster}}
#'
#' @import dplyr
#' @return list
#' @export
#' @keywords internal
#' @examples
#' .demo_landscape(year =  2000:2005,
#'                 res = 1,
#'                 crs = "+proj=utm +zone=21 +south +ellps=GRS80 +units=m +no_defs")
#'

.demo_landscape <- function(year,
                            nrows = 100,
                            ncols = 100,
                            res = 1,
                            xmn = 0,
                            xmx = 100,
                            ymn = 0,
                            ymx = 100,
                            crs = NA,
                            category = 1:5,
                            prob = NULL) {
  # a sample raster
  landscape <- raster::raster(
    nrows = nrows,
    ncols = ncols,
    xmn = xmn,
    xmx = xmx,
    ymn = ymn,
    ymx = ymx,
    resolution = res,
    crs = crs
  )

  mapdemo <- function(year01, pixvalue) {  # an atribuitor of values
    raster::values(landscape) <- pixvalue
    names(landscape) <- paste0("landscape_", year01)
    landscape
  }

  samplerow <- nrow(landscape)
  samplecol <- ncol(landscape)

  pixsample <-
    lapply(year, function(x)
      base::sample(
        category,
        samplerow * samplecol,
        replace = T,
        prob = prob
      ))


  # create a list of n of them
  raster_list <-
    mapply(function(x, y)
      mapdemo(year01 = x, pixvalue = y), year, pixsample, USE.NAMES = TRUE)

  names(raster_list) <- vapply(raster_list, function(x)
    names(x), FUN.VALUE = character(1L))

  return(raster_list)

}


