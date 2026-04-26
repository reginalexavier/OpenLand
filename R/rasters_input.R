#' input_rasters
#'
#' A methods for loading the raster into OpenLand with support for both raster and terra packages
#'
#' @param x path (character), Raster*/SpatRaster object or list of Raster*/SpatRaster objects.
#' @param use_terra logical. If TRUE, prefer terra package for better performance. Default is TRUE.
#' @param \dots additional arguments to \code{raster::\link[raster]{raster}} or \code{terra::\link[terra]{rast}}.
#'
#' @return A RasterStack or SpatRaster depending on use_terra parameter
#'
#' @seealso \code{raster::\link[raster]{raster}},
#' \code{raster::\link[raster]{stack}},
#' \code{terra::\link[terra]{rast}}
#'
#' @keywords internal
#' @rdname dot-input_rasters
#' @exportMethod .input_rasters
#'


setGeneric(".input_rasters", function(x, use_terra = TRUE, ...)
    standardGeneric(".input_rasters"))



# the methods character
#' @rdname dot-input_rasters
#' @aliases character
setMethod(".input_rasters", signature(x = "character"),
          function(x, use_terra = TRUE, ...) {
            # Robust path handling
            normalized_path <- normalizePath(path.expand(x), winslash = "/", mustWork = FALSE)
            
            # Check if it's a single file or directory
            if (file.exists(normalized_path) && !dir.exists(normalized_path)) {
              # Single file - try to load directly
              if (use_terra) {
                tryCatch({
                  maps <- terra::rast(normalized_path)
                  return(maps)
                }, error = function(e) {
                  warning("Terra loading failed, falling back to raster package: ", e$message)
                  maps <- raster::stack(normalized_path)
                  return(maps)
                })
              } else {
                maps <- raster::stack(normalized_path)
                return(maps)
              }
            } else if (dir.exists(normalized_path)) {
              # Directory - list files
              files <- list.files(path = normalized_path,
                                  pattern = ".tif$",
                                  full.names = FALSE)
              if (length(files) > 0) {
                sorted_files <- sort(files)
                paths <- normalizePath(file.path(normalized_path, sorted_files), winslash = "/")
                
                if (use_terra) {
                  # Use terra for better performance
                  tryCatch({
                    maps <- terra::rast(paths)
                    return(maps)
                  }, error = function(e) {
                    warning("Terra loading failed, falling back to raster package: ", e$message)
                    maps <- raster::stack(paths)
                    return(maps)
                  })
                } else {
                  # Use raster package
                  maps <- raster::stack(paths)
                  return(maps)
                }
              } else {
                stop("No .tif files found in directory: ", normalized_path)
              }
            } else {
              stop("Path does not exist: ", normalized_path)
            }
          })

#' @rdname dot-input_rasters
#' @aliases list
setMethod(".input_rasters", signature(x = "list"),
          function(x, use_terra = TRUE, ...) {
            list.names <- names(x)
            if (is.null(list.names))
              stop("list elements must be named")
            files <- list.names
            sorted_files <- sort(files)
            
            if (use_terra && inherits(x[[1]], "SpatRaster")) {
              # Handle terra objects
              maps <- terra::rast(x[sorted_files])
            } else {
              # Handle raster objects or fallback
              maps <- raster::stack(x[sorted_files])
            }
            maps
          })

#' @rdname dot-input_rasters
#' @aliases RasterLayer
setMethod(".input_rasters", signature(x = "RasterLayer"),
          function(x, use_terra = TRUE, ...) {
            if (use_terra) {
              # Convert to terra if requested
              tryCatch({
                maps <- terra::rast(x)
                return(maps)
              }, error = function(e) {
                warning("Terra conversion failed, using raster: ", e$message)
                maps <- raster::stack(x)
                return(maps)
              })
            } else {
              maps <- raster::stack(x)
              return(maps)
            }
          })

#' @rdname dot-input_rasters
#' @aliases RasterBrick
setMethod(".input_rasters", signature(x = "RasterBrick"),
          function(x, use_terra = TRUE, ...) {
            if (use_terra) {
              # Convert to terra if requested
              tryCatch({
                maps <- terra::rast(x)
                return(maps)
              }, error = function(e) {
                warning("Terra conversion failed, using raster: ", e$message)
                return(x)
              })
            } else {
              return(x)
            }
          })

#' @rdname dot-input_rasters
#' @aliases RasterStack
setMethod(".input_rasters", signature(x = "RasterStack"),
          function(x, use_terra = TRUE, ...) {
            if (use_terra) {
              # Convert to terra if requested
              tryCatch({
                maps <- terra::rast(x)
                return(maps)
              }, error = function(e) {
                warning("Terra conversion failed, using raster: ", e$message)
                return(x)
              })
            } else {
              return(x)
            }
          })

# Add methods for terra objects
#' @rdname dot-input_rasters
#' @aliases SpatRaster  
setMethod(".input_rasters", signature(x = "SpatRaster"),
          function(x, use_terra = TRUE, ...) {
            if (!use_terra) {
              # Convert to raster if specifically requested
              tryCatch({
                maps <- raster::stack(x)
                return(maps)
              }, error = function(e) {
                warning("Raster conversion failed, using terra: ", e$message)
                return(x)
              })
            } else {
              return(x)
            }
          })


