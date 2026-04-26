#'
#' Class Interval
#'
#' A S4 class for the Interval level result of an Intensity analysis.  Can be
#' plotted with the plot method \code{\link{plot}}.
#'
#' @slot intervalData tibble. A table with the results of an Intensity analysis
#' at the Interval level (\emph{St} and \emph{U} values).
#'
#' @details The slot \code{intervalData} receives a table containing 4 columns
#' in the following format:
#'    \enumerate{
#'    \item Period: \code{<fct>}. The period of interest \emph{[Yt, Yt+1]}.
#'    \item PercentChange: \code{<dbl>}. Changed area on the Interval level (\%).
#'    \item St: \code{<dbl>}. Annual intensity of change for a time
#'    period [Yt, Yt+1].
#'    \item U: \code{<dbl>}. Uniform intensity for a LUC category change in a time
#'    period of interest.
#'    }
#' @export
#' @aliases Interval
#' @exportClass Interval
#' @rdname Interval-class
#' @import methods
#'
setClass(
  Class = "Interval",
  slots = c(intervalData = "tbl_df"),
  validity = function(object) {
    if (!("tbl_df" %in% class(object@intervalData) &
          (ncol(object@intervalData) == 4)))
    {
      stop("The data has to be a `tibble` and must have 4 columns")
    }
    return(TRUE)
  }
)








#' Class Category
#'
#' A S4 class for the Category level result of an Intensity analysis.  Can be
#' plotted with the plot method \code{\link{plot}}.
#'
#' @slot lookupcolor The colors (character vector) associated with the LUC legend items.
#' @slot categoryData tibble. A table of Category level's results (gain (\emph{Gtj})
#' or loss (\emph{Lti}) values).
#' @slot categoryStationarity tibble. A table containing results of a stationarity
#' test. A change is considered stationary only if the intensities for all time
#' intervals reside on one side of the uniform intensity, i.e are smaller or bigger
#' than the uniform rate over the whole period.
#'
#' @details The slots \code{categoryData} and \code{categoryStationarity} can receive
#'    tables for "Gain" or "Loss" in the following format:
#' \enumerate{
#'
#' \item Gain
#'
#'  \itemize{
#'
#'  \item \code{categoryData}: \code{<tibble>}. A table containing 6 columns:
#'  \enumerate{
#'    \item Period: \code{<fct>}. The period \emph{[Yt, Yt+1]}.
#'    \item To: \code{<fct>}. A LUC category \emph{j}.
#'    \item Interval: \code{<int>}. Duration of the period \emph{[Yt, Yt+1]} in years.
#'    \item GG_km2/GG_pixel: \code{<dbl>/<int>}. Area of gross gain of category
#'    \emph{j} during \emph{[Yt, Yt+1]}.
#'    \item Gtj: \code{<dbl>}. Annual intensity of gross gain of category \emph{j}
#'    for time interval \emph{[Yt, Yt+1]}.
#'    \item St: \code{<dbl>}. Annual intensity of change for time interval \emph{[Yt, Yt+1]}.
#'    }
#'  \item categoryStationarity:  \code{<tibble>}. A table with the results of a
#'  stationarity test of the gain of the categories on the Category level, containing 5 columns:
#'  \enumerate{
#'    \item To: \code{<fct>}. A category of interest \emph{j}.
#'    \item gain: \code{<int>}. Number of times a category had gains during all
#'    time intervals \emph{[Y1, YT]}.
#'    \item N: \code{<int>}. Total number of evaluated time points (T).
#'    \item Stationarity: \code{<chr>}. \emph{Active Gain} or \emph{Dormant Gain}.
#'    \item Test: \code{<chr>}. \emph{Y} if stationarity was detected and \emph{N} if not.
#'    }
#'    }
#'
#' \item Loss
#'
#'  \itemize{
#'
#'  \item \code{categoryData}: \code{<tibble>}. A table containing 6 columns:
#'  \enumerate{
#'    \item Period: \code{<fct>}. The period \emph{[Yt, Yt+1]}.
#'    \item From: \code{<fct>}. A LUC category \emph{i}.
#'    \item Interval: \code{<int>}. Duration of the period \emph{[Yt, Yt+1]} in years.
#'    \item GG_km2/GG_pixel: \code{<dbl>/<int>}. Area of gross loss of category
#'    \emph{i} during \emph{[Yt, Yt+1]}.
#'    \item Lti: \code{<dbl>}. Annual intensity of gross loss of category \emph{i}
#'    for time interval \emph{[Yt, Yt+1]}.
#'    \item STt: \code{<dbl>}. Annual intensity of change for time interval \emph{[Yt, Yt+1]}.
#'    }
#'   \item categoryStationarity: \code{<tibble>}. A table of stationarity test
#'   over the loss of the categories in the Category level, containing 5 columns:
#'  \enumerate{
#'    \item From: \code{<fct>}. A category of interest \emph{i}.
#'    \item loss: \code{<int>}. Number of times a category had losses during all
#'    time intervals \emph{[Y1, YT]}.
#'    \item N: \code{<int>}. Total number of evaluated time points (T).
#'    \item Stationarity: \code{<chr>}. \emph{Active Loss} or \emph{Dormant Loss}.
#'    \item Test: \code{<chr>}. \emph{Y} if stationarity was detected and \emph{N} if not.
#'    }
#'    }
#'
#'    }
#'
#'
#' @aliases Category
#' @export
#' @exportClass Category
#' @rdname Category-class
#'
setClass(
  Class = "Category",
  slots = c(
    lookupcolor = "character",
    categoryData = "tbl_df",
    categoryStationarity = "tbl_df"
  ),

  validity = function(object) {
    if (!("tbl_df" %in% class(object@categoryData) &
          (ncol(object@categoryData) == 6))) {
      stop("The `categoryData` data has to be a `tibble` and must have 5 columns")

    } else if (!("tbl_df" %in% class(object@categoryStationarity) &
                 (ncol(object@categoryStationarity) == 5)))

    {
      stop("The `categoryStationarity` data has to be a `tibble` and must have 5 columns")

    } else if (!is.character(object@lookupcolor) |
               length(names(object@lookupcolor)) == 0) {
      stop("The `lookupcolor` has to be an object of type `character` with name attributes")

    }

    return(TRUE)
  }
)





#' Class Transition
#'
#' A S4 class for the Transition level result of an Intensity analysis. Can be
#' plotted with the plot method \code{\link{plot}}.
#'
#' @slot lookupcolor The colors (character vector) associated with the LUC legend items.
#' @slot transitionData tibble. A table of Transition level's results (gain n (\emph{Rtin} & \emph{Wtn})
#'    or loss m (\emph{Qtmj} & \emph{Vtm}) values).
#' @slot transitionStationarity tibble. A table containing results of a stationarity
#'  test. A change is considered stationary only if the intensities for all time
#'  intervals reside on one side of the uniform intensity, i.e are smaller or bigger
#'  than the uniform rate over the whole period.
#'
#'
#'
#' @details The slots \code{transitionData} and \code{transitionStationarity} can
#' receive tables for "Gain of category n" or "Loss of category m" in the following
#' format:
#' \enumerate{
#'
#' \item Gain of category n:
#'
#'  \itemize{
#'
#'   \item \code{transitionData}: \code{<tibble>}. A table with 7 columns:
#'   \enumerate{
#'    \item Period: \code{<fct>}. The period \emph{[Yt, Yt+1]}.
#'    \item From: \code{<fct>}. A category i.
#'    \item To: \code{<fct>}. The gaining category in the transition of interest \emph{(n)}.
#'    \item Interval: \code{<int>}. Duration of the period \emph{[Yt, Yt+1]}.
#'    \item T_i2n_km2/T_i2n_pixel: \code{<dbl>}. Area with transition from category
#'    i to category n during time interval \emph{[Yt, Yt+1]} where \emph{i}
#'    \code{is not equal to} \emph{n}.
#'    \item Rtin: \code{<dbl>}. Annual intensity of transition from category i to
#'    category n
#'    during time interval \emph{[Yt, Yt+1]} where \emph{i}  \code{is not equal to} \emph{n}.
#'    \item Wtn: \code{<dbl>}. Value of the uniform intensity of the transition
#'    to category n from all non-n categories at time Yt during time interval \emph{[Yt, Yt+1]}.
#'    }
#'  \item transitionStationarity: \code{<tibble>}. A table containing results of
#'  a stationarity test over the gain on \emph{category n} containing 5 columns:
#'  \enumerate{
#'    \item From: \code{<fct>}. The losing category in the transition of interest
#'    to the category n.
#'    \item loss: \code{<int>}. Number of times the category had losses to the
#'    category n.
#'    \item N: \code{<int>}. Total number of transitions to be considered as stationary (T).
#'    \item Stationarity: \code{<chr>}. \emph{targeted by} or \emph{avoided by}
#'    the category \code{n}.
#'    \item Test: \code{<chr>}. \emph{Y} for stationarity detected and \emph{N}
#'    when not.
#'    }
#'    }
#'
#' \item Loss of category m:
#'
#'  \itemize{
#'
#'   \item \code{transitionData}: \code{<tibble>}. A table with 7 columns:
#'   \enumerate{
#'    \item Period: \code{<fct>}. The period \emph{[Yt, Yt+1]}.
#'    \item To: \code{<fct>}. A category \emph{j}.
#'    \item From: \code{<fct>}. The losing category in the transition of interest (m).
#'    \item Interval: \code{<dbl>}. Duration of the period \emph{[Yt, Yt+1]}.
#'    \item T_m2j_km2/T_m2j_pixel: \code{<dbl>}. Area with transition from category
#'     \emph{m} to category \emph{j} during time interval \emph{[Yt, Yt+1]}
#'    where \emph{j}  \code{is not equal to} \emph{m}.
#'    \item Qtmj: \code{<dbl>}. Annual intensity of transition from category \emph{m} to
#'    category \emph{j} during time interval \emph{[Yt, Yt+1]}
#'    where \emph{j}  \code{is not equal to} \emph{m}.
#'    \item Vtm: \code{<dbl>}. Value of the uniform intensity of the transition
#'    from category \emph{m} to all \emph{non-m} categories at time \out{Y<sub>t+1</sub>}
#'    during time interval \emph{[Yt, Yt+1]}.
#'    }
#'  \item transitionStationarity: \code{<tibble>}. A table containing results of
#'  a stationarity test
#'  over the loss of \emph{category m} containing 5 columns:
#'  \enumerate{
#'    \item To: \code{<fct>}. The gaining category in the transition of interest
#'    from the category m.
#'    \item gain: \code{<int>}. Number of times the category had gains from the
#'    category m.
#'    \item N: \code{<int>}. Total number of transitions to be considered as stationary (T).
#'    \item Stationarity: \code{<chr>}. \emph{targeted} or \emph{avoided} the category \code{m}.
#'    \item Test: \code{<chr>}. \emph{Y} for stationarity detected and \emph{N} when not.
#'    }
#'    }
#'
#' }
#' @aliases Transition
#' @export
#' @exportClass Transition
#' @rdname Transition-class
#'
setClass(
  Class = "Transition",
  slots = c(
    lookupcolor = "character",
    transitionData = "tbl_df",
    transitionStationarity = "tbl_df"
  ),
  validity = function(object) {
    if (!("tbl_df" %in% class(object@transitionData) &
          (ncol(object@transitionData) == 7))) {
      stop("The `transitionData` data has to be a `tibble` and must have 7 columns")
    } else if (!("tbl_df" %in% class(object@transitionStationarity) &
                 (ncol(object@transitionStationarity) == 5)))
    {
      stop("The `categoryStationarity` data has to be a `tibble` and must have 5 columns")

    } else if (!is.character(object@lookupcolor) |
               length(names(object@lookupcolor)) == 0) {
      stop("The `lookupcolor` has to be an object of type `character` with name attributes")

    }
    return(TRUE)
  }
)

