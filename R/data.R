#' Rasters of Land Use and Land Cover of the São Lourenço River Basin (2002 - 2014)
#'
#' A multi-layer raster (RasterBrick) with five (5) RasterLayer objects
#' represinting the years 2002, 2008, 2010, 2012 and 2014 respectively.
#' These Raster* have everyone 11 classes of use, these classes are:
#' \tabular{cclll}{
#' \strong{Pixel Value} \tab \strong{Legend} \tab \strong{Class}
#' \tab \strong{Use} \tab \strong{Details}  \cr
#' 2        \tab Ap     \tab Anthropic \tab Anthropic Use \tab Pastagem    \cr
#' 3        \tab FF     \tab Natural   \tab NA            \tab Formações Florestais \cr
#' 4        \tab SA     \tab Natural   \tab NA            \tab Savana Arborizada (SA - Cerrado)  \cr
#' 5        \tab SG     \tab Natural   \tab NA            \tab Savana Gramínea (SG - Campo) \cr
#' 7        \tab aa     \tab Anthropic \tab NA            \tab Alteração Antrópica \cr
#' 8        \tab SF     \tab Natural   \tab NA            \tab Savana Florestada (SF - Cerradão) \cr
#' 9        \tab Agua   \tab Natural   \tab NA            \tab Rios, córregos, corixos, vazantes, baías e salinas   \cr
#' 10       \tab Iu     \tab Anthropic \tab Anthropic Use \tab Influencia Urbana   \cr
#' 11       \tab Ac     \tab Anthropic \tab Anthropic Use \tab Agricultura           \cr
#' 12       \tab R      \tab Anthropic \tab Anthropic Use \tab Reflorestamento   \cr
#' 13       \tab Im     \tab Anthropic \tab Anthropic Use \tab Degradação por Mineração
#' }
#'
#' @docType data
#'
#' @usage data(SaoLourencoBasin)
#'
#' @format An object of class \code{"RasterBrick"} with 5 layers:
#' \itemize{
#'   \item \strong{landscape_2002:} Land scape of the land use and land cover for the yeear 2002
#'   \item \strong{landscape_2008:} Land scape of the land use and land cover for the yeear 2008
#'   \item \strong{landscape_2010:} Land scape of the land use and land cover for the yeear 2010
#'   \item \strong{landscape_2012:} Land scape of the land use and land cover for the yeear 2012
#'   \item \strong{landscape_2014:} Land scape of the land use and land cover for the yeear 2014
#'   }
#'
#' @keywords datasets
#'
#' @references Instituto SOS Pantanal and WWF-Brasil (2015)
#' (\href{https://d3nehc6yl9qzo4.cloudfront.net/downloads/publicacao_bap_relatorio_2012_2014_web.pdf}{Monitoramento
#' das alterações da cobertura vegetal e uso do Solo na Bacia do
#' Alto Paraguai – Porção Brasileira – Período de Análise: 2012 a 2014. 4th edn. Brasilia.})
#'
#' @examples
#' data(SaoLourencoBasin)
#' \donttest{raster::plot(SaoLourencoBasin[[1]])}
#'
#' @source \url{https://www.embrapa.br/pantanal/bacia-do-alto-paraguai}
"SaoLourencoBasin"



#' Tables of Land Use and Land Cover of the São Lourenço River Basin (2002 - 2014)
#'
#' A list containing five objects from \code{\link{contingenceTable}} funtion
#' with the \code{\link{SaoLourencoBasin}} as input
#' (\code{SL_2002_2014 <- contingenceTable(input_raster = SaoLourencoBasin, pixelresolution = 30)}).
#'
#'
#' @docType data
#'
#' @usage data(SL_2002_2014)
#'
#' @format A data list with 5 objects:
#' \describe{
#'   \item{lulc_Mulstistep}{A \code{tibble} of contingency between all step of time analysed, contains 8 columns:
#'   \enumerate{
#'   \item Period: period between the years analyzed
#'   \item From: The pixel value from
#'   \item To: The pixel value to
#'   \item km2: The area of change in squared kilometer
#'   \item interval: The interval in years between the period analyzed
#'   \item QtPixel: The change in quatity of pixel
#'   \item yearFrom: The year that the change come from
#'   \item yearTo: The year that the change go for
#'   }}
#'   \item{tb_legend}{A \code{tibble} of the pixel value his name and color, contains 3 columns:
#'   \enumerate{
#'   \item classValue: \code{numeric} the pixel value of the classes of land use
#'   \item className: \code{factor} The name or legend associate with a given pixel value, here as factor with a specific level for blablabla
#'   \item color: The color associate with the given pixel value
#'   }}
#'   \item{totalArea}{A \code{tibble} of two columns with the total area of study, contains 2 columns:
#'   \enumerate{
#'   \item area_km2: \code{numeric} The total area of study in km2
#'   \item QtPixel: \code{numeric} The total area of study in quantity of pixel
#'   }}
#'   \item{lulc_Onstep}{A \code{tibble} of contingensy of the first and last year analysed with the same columns
#'   like \code{lulc_Mulstistep} \code{tibble}}
#'   \item{totalInterval}{A \code{numeric} represinting the whole interval of time analysed}
#'   }
#' @keywords datasets
#'
#'
#' @source \url{https://www.embrapa.br/pantanal/bacia-do-alto-paraguai}
"SL_2002_2014"
