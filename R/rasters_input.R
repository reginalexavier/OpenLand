#' input_rasters
#'
#' A methods for loading the raster into OpenLand
#'
#' @param x path (character), Raster* object or list of Raster* objects.
#' @param \dots additional arguments to \code{raster::\link[raster]{raster}}.
#'
#' @return A RasterStack
#'
#' @seealso \code{raster::\link[raster]{raster}},
#' \code{raster::\link[raster]{stack}}
#'
#' @keywords internal
#' @name dot-input_rasters
#' @exportMethod .input_rasters
#'

if (!isGeneric(".input_rasters")) {
  setGeneric(".input_rasters", function(x, ...)
    standardGeneric(".input_rasters"))
}


# the methods character
#' @rdname dot-input_rasters
#' @aliases character
setMethod(".input_rasters", signature(x = "character"),
          function(x, ...) {
            files <- list.files(path = x,
                                pattern = ".tif$",
                                full.names = FALSE)
            if (length(files) > 0) {
              sorted_files <- sort(files)
              paths <- file.path(x, sorted_files)
              maps <- raster::stack(paths)
              maps
            } else {
              stop("maps not found")
            }
          })

#' @rdname dot-input_rasters
#' @aliases list
setMethod(".input_rasters", signature(x = "list"),
          function(x, ...) {
            list.names <- names(x)
            if (is.null(list.names))
              stop("list elements must be named")
            files <- list.names
            sorted_files <- sort(files)
            maps <- raster::stack(x[sorted_files])
            maps
          })

#' @rdname dot-input_rasters
#' @aliases RasterLayer
setMethod(".input_rasters", signature(x = "RasterLayer"),
          function(x, ...) {
            maps <- raster::stack(x)
            maps
          })

#' @rdname dot-input_rasters
#' @aliases RasterBrick
setMethod(".input_rasters", signature(x = "RasterBrick"),
          function(x, ...) {
            maps <- x
            maps
          })

#' @rdname dot-input_rasters
#' @aliases RasterStack
setMethod(".input_rasters", signature(x = "RasterStack"),
          function(x, ...) {
            maps <- x
            maps
          })


