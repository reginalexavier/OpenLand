#' Land use and cover (LUC) rasters of the São Lourenço River Basin (2002 - 2014)
#'
#' A multi-layer raster (RasterStack) with five (5) RasterLayer objects
#' representing LUC in the years of 2002, 2008, 2010, 2012 and 2014 respectively.
#' Each RasterLayer has 11 LUC categories:
#' \tabular{cclll}{
#' \strong{Pixel Value} \tab \strong{Legend} \tab \strong{Class}
#' \tab \strong{Use} \tab \strong{Category}  \cr
#' 2        \tab Ap     \tab Anthropic \tab Anthropic Use \tab Pasture    \cr
#' 3        \tab FF     \tab Natural   \tab NA            \tab Forest \cr
#' 4        \tab SA     \tab Natural   \tab NA            \tab Park Savannah   \cr
#' 5        \tab SG     \tab Natural   \tab NA            \tab Gramineous Savannah \cr
#' 7        \tab aa     \tab Anthropic \tab NA            \tab Anthropized Vegetation  \cr
#' 8        \tab SF     \tab Natural   \tab NA            \tab Woooded Savannah          \cr
#' 9        \tab Agua   \tab Natural   \tab NA            \tab Water body     \cr
#' 10       \tab Iu     \tab Anthropic \tab Anthropic Use \tab Urban                      \cr
#' 11       \tab Ac     \tab Anthropic \tab Anthropic Use \tab Crop farming                \cr
#' 12       \tab R      \tab Anthropic \tab Anthropic Use \tab Reforestation           \cr
#' 13       \tab Im     \tab Anthropic \tab Anthropic Use \tab Mining
#' }
#'
#' @docType data
#'
#' @usage data(SaoLourencoBasin)
#'
#' @format An object of class \code{"RasterStack"} with 5 layers:
#' \itemize{
#'   \item \strong{landscape_2002:} LUC in 2002.
#'   \item \strong{landscape_2008:} LUC in 2008.
#'   \item \strong{landscape_2010:} LUC in 2010.
#'   \item \strong{landscape_2012:} LUC in 2012.
#'   \item \strong{landscape_2014:} LUC in 2014.
#'   }
#'
#' @keywords datasets
#'
#' @references Instituto SOS Pantanal and WWF-Brasil (2015)
#' (\href{https://d3nehc6yl9qzo4.cloudfront.net/downloads/publicacao_bap_relatorio_2012_2014_web.pdf}{Monitoramento
#' das alterações da cobertura vegetal e uso do Solo na Bacia do
#' Alto Paraguai – Porção Brasileira – Período de Análise: 2012 a 2014. 4th edn. Brasilia.})
#'
#'
#' @source \url{https://www.embrapa.br/pantanal/bacia-do-alto-paraguai}
"SaoLourencoBasin"



#' Tables of land use and cover (LUC) in the São Lourenço River Basin (2002 - 2014)
#'
#' A list containing five objects created by the \code{\link{contingencyTable}} funtion
#' with \code{\link{SaoLourencoBasin}} as input
#' (\code{SL_2002_2014 <- contingenceTable(input_raster = SaoLourencoBasin, pixelresolution = 30)}).
#'
#'
#' @docType data
#'
#' @usage data(SL_2002_2014)
#'
#' @format A data list with 5 objects:
#' \describe{
#'   \item{lulc_Multistep}{\code{<tibble>} Contingency table for all analysed time steps, containing 8 columns:
#'   \enumerate{
#'   \item Period: \code{<chr>} The period \emph{[Yt, Yt+1]}.
#'   \item From: \code{<int>} numerical code of a LUC category \emph{i}.
#'   \item To: \code{<int>} numerical code of a LUC category \emph{j}.
#'   \item km2: \code{<dbl>} Area in square kilometers that transited from the category \emph{i}
#'    to category \emph{j} in the period from \emph{Yt} to \emph{Yt+1}.
#'   \item QtPixel: \code{<int>} Pixel count that transited from the categories \emph{i}
#'    to category \emph{j} in the period from \emph{Yt} to \emph{Yt+1}.
#'   \item Interval: \code{<int>} Interval of years between the first and
#'    the last year of the period \emph{[Yt, Yt+1]}.
#'   \item yearFrom: \code{<int>} The year that the change comes from \emph{[Yt]}
#'   \item yearTo: \code{<int>} The year that the change goes for \emph{[Yt+1]}
#'   }}
#'   \item{lulc_Onstep}{\code{<tibble>} Contingency table for the entire analysed period \emph{[Yt1, YT]}, containing
#'   8 columns identical with \code{lulc_Mulstistep}}.
#'   \item{tb_legend}{\code{<tibble>} A table of the pixel value, his name and color containing 3 columns:
#'   \enumerate{
#'   \item categoryValue: \code{<int>} the pixel value of the LUC category.
#'   \item categoryName: \code{<fct>} randomly created string associated with a given pixel value of a LUC category.
#'   \item color: \code{<chr>} random color associated with the given pixel value of a LUC category.
#'   }}
#'   \item{totalArea}{\code{<tibble>} A table with the total area of the study area containing 2 columns:
#'   \enumerate{
#'   \item area_km2: \code{<dbl>} The total area in square kilometers.
#'   \item QtPixel: \code{<int>} The total area in pixel counts}}.
#'   \item{totalInterval}{\code{<int>} Total interval of the analysed time series in years}.
#'   }
#'
#' @keywords datasets
#'
#'
#' @source \url{https://www.embrapa.br/pantanal/bacia-do-alto-paraguai}
"SL_2002_2014"
