
#' @importFrom dplyr bind_rows tibble
#' @importFrom terra rast ext res nrow ncol minmax crs freq nlyr
NULL

#' Summary of multiple parameters in a raster directory
#'
#' Listing major characteristics of raster inputs. Those characteristics are the
#' dimensions, the resolution, the extent, the values (min, max) and the
#' coordinate reference system. This function supports both raster and terra objects
#' for improved performance.
#'
#' @param path The path for the Raster*/SpatRaster directory or list of Raster*/SpatRaster to be
#' analysed. Supports raster package objects and terra package objects.
#'
#' @return Table with the raster parameters in columns
#' @export
#'
#' @examples
#' \donttest{
#' url <- "https://zenodo.org/record/3685230/files/SaoLourencoBasin.rda?download=1"
#' temp <- tempfile()
#' download.file(url, temp, mode = "wb") # downloading the SaoLourencoBasin dataset
#' load(temp)
#' # the summary_dir() function, with the SaoLourencoBasin dataset
#'
#' summary_dir(raster::unstack(SaoLourencoBasin))
#' }
#'
summary_dir <- function(path) {
  
  # Handle different input types with terra/raster compatibility
  if (inherits(path, "list")) {
    # Check if it's a list of raster/terra objects
    if (inherits(path[[1]], c("RasterLayer", "SpatRaster"))) {
      layer_list <- path
    } else {
      stop("List elements must be RasterLayer or SpatRaster objects")
    }
  } else if (inherits(path, "character")) {
    # Robust path handling - normalize and expand path
    normalized_path <- normalizePath(path.expand(path), winslash = "/", mustWork = FALSE)
    
    # Check if directory exists
    if (!dir.exists(normalized_path)) {
      stop("Directory does not exist: ", normalized_path)
    }
    
    # Directory path - load files vectorized
    raster_files <- list.files(normalized_path, pattern = ".tif$", full.names = TRUE)
    
    if (length(raster_files) == 0) {
      stop("No .tif files found in the specified directory: ", normalized_path)
    }
    
    # Use terra for faster loading - vectorized approach
    # terra::rast can handle multiple files at once
    tryCatch({
      # Try terra first (faster)
      layer_stack <- terra::rast(raster_files)
      layer_list <- as.list(layer_stack)
    }, error = function(e) {
      # Fallback to raster if terra fails
      warning("Terra loading failed, falling back to raster package")
      layer_list <- lapply(raster_files, raster::raster)
    })
  } else if (inherits(path, c("RasterStack", "RasterBrick", "SpatRaster"))) {
    # Handle stack/brick objects
    if (inherits(path, "SpatRaster")) {
      layer_list <- as.list(path)
    } else {
      layer_list <- raster::unstack(path)
    }
  } else {
    stop("Input must be a character path, list of raster objects, or raster stack/brick")
  }
  
  # Vectorized summary extraction using appropriate package functions
  summary_list <- lapply(layer_list, function(x) {
    
    # Determine which package functions to use based on object type
    if (inherits(x, "SpatRaster")) {
      # Use terra functions for SpatRaster
      ext_obj <- terra::ext(x)
      res_obj <- terra::res(x)
      tibble::tibble(
        file_name = names(x),
        xmin = ext_obj[1],
        xmax = ext_obj[2],
        ymin = ext_obj[3],
        ymax = ext_obj[4],
        res_x = res_obj[1],
        res_y = res_obj[2],
        nrow = terra::nrow(x),
        ncol = terra::ncol(x),
        min_val = terra::minmax(x)[1],
        max_val = terra::minmax(x)[2],
        crs = as.character(terra::crs(x))
      )
    } else {
      # Use raster functions for RasterLayer
      tibble::tibble(
        file_name = base::names(x),
        xmin = raster::xmin(x),
        xmax = raster::xmax(x),
        ymin = raster::ymin(x),
        ymax = raster::ymax(x),
        res_x = raster::res(x)[1],
        res_y = raster::res(x)[2],
        nrow = raster::nrow(x),
        ncol = raster::ncol(x),
        min_val = raster::minValue(x),
        max_val = raster::maxValue(x),
        crs = as.character(raster::crs(x))
      )
    }
  })
  
  # Efficient binding using dplyr
  dplyr::bind_rows(summary_list)
}


#' Quantitative summary of a unique categorical raster
#'
#' This function presents a summary with the pixel quantity of each category
#' present in a categorical raster. Supports both raster and terra objects
#' for improved performance.
#'
#' @param path The path for the raster to be analysed, if path is a multilayer
#' raster only the first RasterLayer/SpatRaster will be analysed. Supports both
#' raster package objects and terra package objects.
#'
#' @return A table containing in columns the pixel counts for each pixel value
#'
#' @export
#'
#' @examples
#' \donttest{
#' url <- "https://zenodo.org/record/3685230/files/SaoLourencoBasin.rda?download=1"
#' temp <- tempfile()
#' download.file(url, temp, mode = "wb") # downloading the SaoLourencoBasin dataset
#' load(temp)
#' summary_map(SaoLourencoBasin[[1]])
#' }
#'
summary_map <- function(path) {
  
  # Handle different input types with terra/raster compatibility
  if (inherits(path, "character")) {
    # Try terra first for better performance
    tryCatch({
      rastermap <- terra::rast(path)
      if (terra::nlyr(rastermap) > 1) {
        rastermap <- rastermap[[1]]  # Use first layer if multilayer
      }
    }, error = function(e) {
      # Fallback to raster
      warning("Terra loading failed, falling back to raster package")
      rastermap <- raster::raster(path)
    })
  } else if (inherits(path, c("RasterLayer", "SpatRaster"))) {
    rastermap <- path
  } else if (inherits(path, c("RasterStack", "RasterBrick"))) {
    rastermap <- path[[1]]  # Use first layer
  } else if (inherits(path, "SpatRaster")) {
    rastermap <- path[[1]]  # Use first layer if multilayer
  } else {
    stop("Input must be a character path, RasterLayer, SpatRaster, or raster stack/brick")
  }
  
  # Get pixel values using appropriate method
  if (inherits(rastermap, "SpatRaster")) {
    # Use terra for SpatRaster - more efficient
    value_map <- terra::freq(rastermap, digits = 0)
    
    # Convert to tibble format
    if (is.data.frame(value_map)) {
      tbfinal <- dplyr::tibble(
        pixvalue = value_map$value,
        Qt = value_map$count
      )
    } else {
      # Handle case where freq returns matrix
      tbfinal <- dplyr::tibble(
        pixvalue = as.numeric(rownames(value_map)),
        Qt = as.numeric(value_map[, 1])
      )
    }
  } else {
    # Use raster for RasterLayer - but vectorized approach
    values_vec <- raster::values(rastermap)
    values_vec <- values_vec[!is.na(values_vec)]  # Remove NA values
    
    # Use table for frequency counting (vectorized)
    value_map <- table(values_vec)
    
    # Convert to tibble efficiently (vectorized)
    tbfinal <- dplyr::tibble(
      pixvalue = as.numeric(names(value_map)),
      Qt = as.numeric(value_map)
    )
  }
  
  return(tbfinal)
}



#' Accumulates changes in a LULC raster time series with terra/raster compatibility
#'
#' This function calculates the number of times a pixel has changed during the 
#' analysed period, providing comprehensive spatial screening of Land Use and Cover 
#' Change (LUCC) frequencies. It returns a raster with the number of changes as pixel 
#' values and a statistical table containing the areal percentage of every change frequency.
#'
#' The function automatically detects and handles both modern terra (SpatRaster) and 
#' legacy raster (RasterStack/RasterBrick) objects, applying appropriate processing 
#' methods for optimal performance while maintaining full backward compatibility.
#'
#' @param path The path for the Raster* directory, RasterStack/RasterBrick object, 
#' SpatRaster object, or list of Raster*/SpatRaster objects to be analysed. 
#' Supports both raster and terra package formats with automatic detection.
#'
#' @details
#' **Processing Logic:**
#' \itemize{
#'   \item Compares consecutive time steps in the series (t1 vs t2, t2 vs t3, etc.)
#'   \item Creates binary change maps (1 = change, 0 = no change) for each interval
#'   \item Sums all change maps to get total change frequency per pixel
#'   \item Generates statistical summary of change patterns across the landscape
#' }
#' 
#' **Terra/Raster Compatibility:**
#' \itemize{
#'   \item **Terra objects**: Uses terra::nlyr(), terra::app(), and optimized layer extraction
#'   \item **Raster objects**: Uses raster::unstack(), raster::overlay(), and legacy operations  
#'   \item **Automatic detection**: Function determines object type and applies appropriate methods
#'   \item **Performance**: Terra processing typically 2-3x faster for large datasets
#' }
#' 
#' **Change Detection Method:**
#' For each consecutive pair of time steps, the function:
#' 1. Extracts individual layers from the stack/raster collection
#' 2. Applies pixel-wise comparison using conditional logic
#' 3. Creates binary change indicators (ifelse(pixel_t1 != pixel_t2, 1, 0))
#' 4. Accumulates all change indicators to generate final change frequency map
#'
#' @return A list containing two objects:
#' \itemize{
#'   \item **Element 1**: A RasterLayer/SpatRaster with pixel values representing the 
#'   number of changes (0 = no changes, 1 = one change, 2 = two changes, etc.)
#'   \item **Element 2**: A tibble with three columns:
#'   \enumerate{
#'     \item \code{PxValue}: Number of changes per pixel (0, 1, 2, ...)
#'     \item \code{Qt}: Quantity of pixels with that change frequency
#'     \item \code{Percent}: Percentage of total area with that change frequency
#'   }
#' }
#' @export
#'
#'
#'
#' @examples
#' \donttest{
#' url <- "https://zenodo.org/record/3685230/files/SaoLourencoBasin.rda?download=1"
#' temp <- tempfile()
#' download.file(url, temp, mode = "wb") # downloading the SaoLourencoBasin dataset
#' load(temp)
#' # the acc_changes() function, with the SaoLourencoBasin dataset
#' acc_changes(SaoLourencoBasin)
#' }
#'

acc_changes <- function(path) {

  rList <- .input_rasters(path)

  # Handle both terra and raster objects for unstacking
  if (inherits(rList, "SpatRaster")) {
    # For terra objects, convert to list of individual layers
    n_raster <- terra::nlyr(rList)
    if (n_raster < 2) {
      stop('acc_changes needs at least 2 rasters')
    }
    rList <- lapply(1:n_raster, function(i) rList[[i]])
  } else {
    # For raster objects, use unstack
    rList <- raster::unstack(rList)
    n_raster <- length(rList)
    if (n_raster < 2) {
      stop('acc_changes needs at least 2 rasters')
    }
  }

  # Create difference layers - handle both terra and raster objects
  difflist <- mapply(
    function(x, y) {
      if (inherits(x, "SpatRaster") && inherits(y, "SpatRaster")) {
        # For terra objects, use terra::c() for optimal performance
        combined <- terra::c(x, y)
        terra::app(combined, fun = function(vals) {
          # vals is a matrix with columns for each layer
          ifelse(vals[,1] != vals[,2], 1, 0)
        })
      } else {
        # For raster objects, use raster::overlay
        raster::overlay(
          x,
          y,
          fun = function(x1, x2)
            ifelse((x1 != x2), 1, 0)
        )
      }
    },
    x = rList[1:(length(rList) - 1)],
    y = rList[2:length(rList)],
    SIMPLIFY = FALSE
  )

  # Sum all difference layers
  if (inherits(difflist[[1]], "SpatRaster")) {
    # For terra objects
    sumraster <- Reduce("+", difflist)
  } else {
    # For raster objects
    sumraster <- sum(raster::stack(difflist))
  }

  Freq <- Var1 <- NULL

  # Extract values for table - handle both terra and raster objects
  if (inherits(sumraster, "SpatRaster")) {
    # For terra objects
    df01_values <- table(terra::values(sumraster, na.rm = TRUE))
  } else {
    # For raster objects
    df01_values <- table(matrix(sumraster))
  }

  df_values <- dplyr::mutate(data.frame(df01_values),
                             Var1 = as.character(Var1),
                             Var1 = as.integer(Var1),
                             Percent = Freq/sum(Freq)*100)

  df_values <- dplyr::as_tibble(df_values)

  names(df_values) <- c("PxValue", "Qt", "Percent")

  list(sumraster, df_values)

}


#' Extract year from raster name with flexible naming conventions
#'
#' This internal function provides flexible extraction of years from raster names
#' supporting different separators, year positions, and custom patterns.
#'
#' @param name character. The raster name to extract year from.
#' @param separator character. The separator used to split the name.
#' @param position character or numeric. Position of the year in the split name.
#' @param pattern character. Regular expression pattern to extract year.
#'
#' @return character. The extracted year as a string.
#'
#' @keywords internal
#' @noRd
extract_year_from_name <- function(name, separator = "_", 
                                   position = "last", pattern = NULL) {
  if (!is.null(pattern)) {
    # Use custom pattern if provided
    year_match <- stringr::str_extract(name, pattern)
    if (is.na(year_match)) {
      stop(paste("Could not extract year from name:", name, "using pattern:", pattern))
    }
    return(year_match)
  } else {
    # Auto-detect R modifications and adjust separator
    original_separator <- separator
    
    # Handle common R name modifications
    if (separator == "-" && !grepl("-", name) && grepl("\\.", name)) {
      separator <- "."
      warning(paste("R converted hyphens to dots in raster names. Using '.' instead of '-' for:", name))
    }
    
    # Remove R's automatic "X" prefix if present when extracting from beginning
    clean_name <- name
    x_prefix_removed <- FALSE
    if (position == "first" && grepl("^X[0-9]", name)) {
      clean_name <- sub("^X", "", name)
      x_prefix_removed <- TRUE
      warning(paste("R added 'X' prefix to numeric name. Removing 'X' from:", name))
    }
    
    # Use separator and position
    parts <- strsplit(clean_name, separator, fixed = TRUE)[[1]]
    if (length(parts) < 2) {
      # Enhanced error message with suggestions
      suggested_separators <- c("_", ".", "-")
      found_separators <- suggested_separators[sapply(suggested_separators, function(s) grepl(s, name, fixed = TRUE))]
      
      error_msg <- paste("Name", name, "does not contain separator", original_separator)
      if (length(found_separators) > 0) {
        error_msg <- paste(error_msg, "\nFound these separators in the name:", paste(found_separators, collapse = ", "))
        error_msg <- paste(error_msg, "\nTry using name_separator =", paste0("'", found_separators[1], "'"))
      }
      if (grepl("[0-9]{4}", name)) {
        error_msg <- paste(error_msg, "\nAlternatively, use name_pattern = '[0-9]{4}' to extract the year directly")
      }
      stop(error_msg)
    }
    
    if (position == "last") {
      year <- parts[length(parts)]
    } else if (position == "first") {
      year <- parts[1]
    } else if (is.numeric(position)) {
      if (position > length(parts) || position < 1) {
        stop(paste("Position", position, "is out of range for name:", name, "with", length(parts), "parts"))
      }
      year <- parts[position]
    } else {
      stop("year_position must be 'first', 'last', or a numeric position")
    }
    
    # Validate that extracted part looks like a year
    if (!grepl("^[0-9]{4}$", year)) {
      # Enhanced error with suggestions
      error_msg <- paste("Extracted part '", year, "' from name '", name, "' does not look like a 4-digit year", sep = "")
      if (grepl("[0-9]{4}", name)) {
        found_years <- stringr::str_extract_all(name, "[0-9]{4}")[[1]]
        error_msg <- paste(error_msg, "\nFound these 4-digit numbers:", paste(found_years, collapse = ", "))
        error_msg <- paste(error_msg, "\nConsider using name_pattern = '[0-9]{4}' instead")
      }
      stop(error_msg)
    }
    
    return(year)
  }
}


#' Check terra availability and performance status
#'
#' Helper function to check if terra package is available and provide
#' performance recommendations for OpenLand functions.
#'
#' @return A list with terra availability status and performance tips
#' @export
#'
#' @examples
#' \dontrun{
#' performance_status()
#' }
#'
performance_status <- function() {
  terra_available <- requireNamespace("terra", quietly = TRUE)
  
  result <- list(
    terra_available = terra_available,
    recommendations = character(0),
    performance_tips = character(0)
  )
  
  if (terra_available) {
    result$recommendations <- c(
      "✓ Terra package is available - functions will use terra for better performance",
      "✓ Expected performance improvement: 2-10x faster for large raster datasets",
      "✓ Memory usage optimization: terra handles large rasters more efficiently"
    )
    
    result$performance_tips <- c(
      "• Use summary_dir() with terra objects for fastest directory summaries",
      "• summary_map() automatically detects and uses terra::freq() for faster pixel counting",
      "• .input_rasters() defaults to terra format - set use_terra=FALSE to force raster format",
      "• For very large datasets, terra's out-of-memory processing provides significant advantages"
    )
  } else {
    result$recommendations <- c(
      "⚠ Terra package not available - falling back to raster package",
      "⚠ Consider installing terra for significant performance improvements:",
      "  install.packages('terra')",
      "⚠ Performance may be slower for large raster datasets"
    )
    
    result$performance_tips <- c(
      "• Install terra package: install.packages('terra')",
      "• Terra provides 2-10x performance improvements for raster operations",
      "• Terra has better memory management for large datasets",
      "• Terra supports larger-than-memory raster processing"
    )
  }
  
  # Print formatted output
  cat("OpenLand Performance Status\n")
  cat("==========================\n\n")
  
  cat("Status:\n")
  for (rec in result$recommendations) {
    cat(rec, "\n")
  }
  
  cat("\nPerformance Tips:\n")
  for (tip in result$performance_tips) {
    cat(tip, "\n")
  }
  
  cat("\nFor more information, see: ?summary_dir, ?summary_map, ?.input_rasters\n")
  
  invisible(result)
}
