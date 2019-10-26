
#' A S4 super class for the objects from the intensity analysis
#'
#' @slot tabela tbl_df.
#'
#' @return virtual class
#' @export
#'
#' @examples

methods::setClass(
  Class = "Intensity", slots = c(tabela = "tbl_df"),
  prototype = methods::prototype(tabela = tibble::tibble()),
  validity = function(object) {
    if (!tibble::is_tibble(object@tabela))
    {
      stop("The data have to be a 'tibble'")
    }
    return(TRUE)
  },
  contains = c("VIRTUAL")
  #contains = c("tbl_df", "VIRTUAL")
)


# setMethod("initialize",
#              "Intensity",
#              function(.Object,
#                         tabela = tibble()) {
#                .Object <- callNextMethod()
#                .Object@tabela <- tibble()
#                .Object
#                })


#' An S4 class for the intensity level 1 analysis
#'
#' @slot level character.
#'
#' @return an intensity level 1 object
#' @export
#'
#' @examples
methods::setClass(
  Class = "IntensityL01", slots = c(level = "character"),
  prototype = methods::prototype(level = "Interval"),
  validity = function(object) {
    if (!(ncol(object@tabela) == 5))
    {
      stop("The data have to be a `tibble` and have 5 columns")
    }
    return(TRUE)
  },
  contains = c("Intensity")
)

#' A class for the intensity level 2 analysis
#'
#' @slot level character.
#' @slot color character.
#'
#' @return an intensity level 2 object
#' @export
#'
#' @examples
methods::setClass(
  Class = "IntensityL02", slots = c(level = "character", color = "character"),
  prototype = methods::prototype(level = "Category"),
  validity = function(object) {
    if (!(ncol(object@tabela) == 8))
    {
      stop("The data have to be a `tibble` and have 8 columns")
    }
    return(TRUE)
  },
  contains = c("Intensity")
)

#' An S4 class for the intensity level 2 analysis
#'
#' @slot level character.
#' @slot color character.
#'
#' @return an intensity level 3 object
#' @export
#'
#' @examples
methods::setClass(
  Class = "IntensityL03", slots = c(level = "character", color = "character"),
  prototype = methods::prototype(level = "Transition"),
  validity = function(object) {
    if (!(ncol(object@tabela) == 10))
    {
      stop("The data have to be a `tibble` and have 10 columns")
    }
    return(TRUE)
  },
  contains = c("Intensity")
)


# setting a method for the show function
# setMethod("show", signature = "Intensity",
#              function(object){
#                cat("*** Class Intensity, method Show *** \n")
#                nrowShow <- min(10, nrow(object@tabela))
#                #ncolShow <- min(10, nrow(object@tabela))
#                cat("* Intensity table (limited to a `tibble` 10 rows) = \n")
#                ifelse((length(object@tabela) != 0),
#                print(object@tabela[1:nrowShow, ]), print("tible not defined!\n"))
#                #cat("* Level =", object@level, "\n")
#                cat("******* End Show (Intensity table) ******* \n")
#                }
#              )


#'An replacement method for the intensity super class
#'
#' @param Intensity
#'
#' @return
#' @export
#'
#' @examples
methods::setMethod("$", signature = "Intensity",
          function(x, name) {
            if (name %in% methods::slotNames(x)) {
              methods::slot(x, name)
            }})


# helper or constructor for the 3 classes
#' This is a helper funtion for create the intensities sub classes
#'
#' @param data tibble
#'
#' @return an object with the correspondate class
#' @export
#'
#' @examples
intensity <- function(data) {
  if (ncol(data) == 5) {
    methods::new("IntensityL01", tabela = data)
  } else if (ncol(data) == 8) {
    methods::new("IntensityL02", tabela = data, color = lookupcolor)
  } else if (ncol(data) == 10) {
    methods::new("IntensityL03", tabela = data, color = lookupcolor)
  } else {
    cat("The intensity table as to have 5, 8 or 10 columns for level 1, 2 or 3 respectively")
  }
}

