#' Tables of land use and cover (LUC) in the São Lourenço River Basin (2002 - 2014)
#'
#' A list containing five objects created by the \code{\link{contingencyTable}}
#' function with \code{SaoLourencoBasin} as input
#' (\code{SL_2002_2014 <- contingenceTable(input_raster = SaoLourencoBasin, pixelresolution = 30)}).
#'
#'
#' @docType data
#'
#' @usage data(SL_2002_2014)
#'
#' @format A data list with 5 objects:
#' \describe{
#'   \item{lulc_Multistep}{\code{<tibble>} Contingency table for all analysed
#'   time steps, containing 8 columns:
#'   \enumerate{
#'   \item Period: \code{<chr>} The period \emph{[Yt, Yt+1]}.
#'   \item From: \code{<int>} numerical code of a LUC category \emph{i}.
#'   \item To: \code{<int>} numerical code of a LUC category \emph{j}.
#'   \item km2: \code{<dbl>} Area in square kilometers that transited from the
#'   category \emph{i}
#'    to category \emph{j} in the period from \emph{Yt} to \emph{Yt+1}.
#'   \item QtPixel: \code{<int>} Pixel count that transited from the categories
#'    \emph{i} to category \emph{j} in the period from \emph{Yt} to \emph{Yt+1}.
#'   \item Interval: \code{<int>} Interval of years between the first and
#'    the last year of the period \emph{[Yt, Yt+1]}.
#'   \item yearFrom: \code{<int>} The year that the change comes from \emph{[Yt]}
#'   \item yearTo: \code{<int>} The year that the change goes for \emph{[Yt+1]}
#'   }}
#'   \item{lulc_Onstep}{\code{<tibble>} Contingency table for the entire analysed
#'    period \emph{[Yt1, YT]}, containing
#'   8 columns identical with \code{lulc_Mulstistep}}.
#'   \item{tb_legend}{\code{<tibble>} A table of the pixel value, his name and
#'   color containing 3 columns:
#'   \enumerate{
#'   \item categoryValue: \code{<int>} the pixel value of the LUC category.
#'   \item categoryName: \code{<fct>} randomly created string associated with a
#'   given pixel value of a LUC category.
#'   \item color: \code{<chr>} random color associated with the given pixel value
#'    of a LUC category.
#'   }}
#'   \item{totalArea}{\code{<tibble>} A table with the total area of the study
#'   area containing 2 columns:
#'   \enumerate{
#'   \item area_km2: \code{<dbl>} The total area in square kilometers.
#'   \item QtPixel: \code{<int>} The total area in pixel counts}}.
#'   \item{totalInterval}{\code{<int>} Total interval of the analysed time
#'   series in years}.
#'   }
#'
#' @keywords datasets
#'
#'
#' @source \url{https://www.embrapa.br/pantanal/bacia-do-alto-paraguai}
"SL_2002_2014"
