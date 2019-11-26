#'
#' Class Intensity Level 1
#'
#' An S4 class for the intensity level 1 analysis result
#'
#' @slot level character
#' @slot intervalData a tibble
#'
#' @export
#' @exportClass IntensityL01
#' @rdname IntensityL01-class
#' @import methods
#'


setClass(
  Class = "IntensityL01", slots = c(level = "character",
                                    intervalData = "tbl_df"),
  prototype = prototype(level = "Interval",
                        intervalData = tibble::tibble()),
  validity = function(object) {
    if (!(tibble::is_tibble(object@intervalData) & (ncol(object@intervalData) == 4)))
    {
      stop("The data have to be a `tibble` and have 4 columns")
    }
    return(TRUE)
  }
)

#' Class Intensity Level 2
#'
#' A class for the intensity level 2 analysis result
#'
#' @slot level character
#' @slot lookupcolor character
#' @slot categoryData a tibble
#' @slot categoryStationarity a tibble
#'
#' @export
#' @exportClass IntensityL02
#' @rdname IntensityL02-class
#'


setClass(
  Class = "IntensityL02",
  slots = c(
    level = "character",
    lookupcolor = "character",
    categoryData = "tbl_df",
    categoryStationarity = "tbl_df"
  ),
  prototype = prototype(
    level = "Category",
    categoryData = tibble::tibble(),
    categoryStationarity = tibble::tibble()
  ),
  validity = function(object) {
    if (!(tibble::is_tibble(object@categoryData) &
          (ncol(object@categoryData) == 6))) {
      stop("The `categoryData` data have to be a `tibble` and have 5 columns")

    } else if (!(tibble::is_tibble(object@categoryStationarity) &
                 (ncol(object@categoryStationarity) == 5)))

    {
      stop("The `categoryStationarity` data have to be a `tibble` and have 5 columns")

    } else if (!is.character(object@lookupcolor) &
               length(names(object@lookupcolor) == 0)) {
      stop("The `lookupcolor` have to be an object of type `character` with names attribuites")

    }

    return(TRUE)
  }
)

#' Class Intensity Level 3
#'
#' An S4 class for the intensity level 3 analysis  result
#'
#' @slot level character
#' @slot lookupcolor character
#' @slot transitionData a tibble
#' @slot transitionStationarity a tibble
#'
#' @export
#' @exportClass IntensityL03
#' @rdname IntensityL03-class
#'




setClass(
  Class = "IntensityL03",
  slots = c(
    level = "character",
    lookupcolor = "character",
    transitionData = "tbl_df",
    transitionStationarity = "tbl_df"
  ),
  prototype = methods::prototype(level = "Transition"),
  validity = function(object) {
    if (!(tibble::is_tibble(object@transitionData) &
          (ncol(object@transitionData) == 7))) {
      stop("The `transitionData` data have to be a `tibble` and have 7 columns")
    } else if (!(tibble::is_tibble(object@transitionStationarity) &
                 (ncol(object@transitionStationarity) == 5)))
    {
      stop("The `categoryStationarity` data have to be a `tibble` and have 5 columns")

    } else if (!is.character(object@lookupcolor) &
               length(names(object@lookupcolor) == 0)) {
      stop("The `lookupcolor` have to be an object of type `character` with names attribuites")

    }
    return(TRUE)
  }
)

