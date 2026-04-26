#' @include intensityClass.R
NULL

#' Accessor methods for Intensity Analysis objects
#' 
#' These methods provide convenient access to slots in S4 objects from intensity analysis.
#' They enable dollar sign notation (e.g., object$slot) for accessing object components.
#' 
#' @param x An object of class Interval, Category, or Transition
#' @param name Character string specifying the slot name to access
#' 
#' @return The contents of the specified slot if it exists
#' 
#' @keywords internal
#' @rdname acessor
#' @name acessor-methods
#' 
#' @examples
#' \dontrun{
#' # After running intensityAnalysis()
#' my_result <- intensityAnalysis(dataset = SL_2002_2014, 
#'                               category_n = "Ap", category_m = "SG")
#' 
#' # Access interval level data  
#' interval_data <- my_result$interval_lvl$intervalData
#' 
#' # Access category level data
#' category_data <- my_result$category_lvlGain$categoryData
#' }

# Generic accessor method for all intensity analysis classes
.accessor_method <- function(x, name) {
  if (name %in% slotNames(x)) {
    slot(x, name)
  } else {
    warning("Slot '", name, "' not found in object of class '", class(x), "'")
    NULL
  }
}

# Interval class accessor
setMethod("$", signature = "Interval", .accessor_method)

# Category class accessor  
setMethod("$", signature = "Category", .accessor_method)

# Transition class accessor
setMethod("$", signature = "Transition", .accessor_method)
