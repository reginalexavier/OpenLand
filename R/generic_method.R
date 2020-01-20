#' @include intensityClass.R
NULL
#' Accessor method for objects from Intensity Analysis
#' @param Interval the class
#' @param x the object
#' @param name the name of the object
#'
#' @keywords internal
#' @rdname acessor
#'
#'


setMethod("$", signature = "Interval",
          function(x, name) {
            if (name %in% slotNames(x)) {
              slot(x, name)
            }})



#' @rdname acessor
#'
#' @param Category the class
setMethod("$", signature = "Category",
          function(x, name) {
            if (name %in% slotNames(x)) {
              slot(x, name)
            }})


#' @rdname acessor
#'
#' @param Transition the class
setMethod("$", signature = "Transition",
          function(x, name) {
            if (name %in% slotNames(x)) {
              slot(x, name)
            }})
