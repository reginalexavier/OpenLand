#' Rasters of Land Use and Land Cover of the São Lourenço River Basin (2002 - 2014)
#'
#' A multi-layer raster (RasterStack) with five (5) RasterLayer objects
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
#'   \item{lulc_Multistep}{\code{<tibble>} A table of contingency between all step of time analysed, contains 8 columns:
#'   \enumerate{
#'   \item Period: \code{<chr>} period between the years analyzed
#'   \item From: \code{<int>} The pixel value from
#'   \item To: \code{<int>} The pixel value to
#'   \item km2: \code{<dbl>} The area of change in squared kilometer
#'   \item QtPixel: \code{<int>} The change in quatity of pixel
#'   \item Interval: \code{<int>} The interval in years between the period analyzed
#'   \item yearFrom: \code{<int>} The year that the change come from
#'   \item yearTo: \code{<int>} The year that the change go for
#'   }}
#'   \item{lulc_Onstep}{\code{<tibble>} A table of contingensy of the first and last year analysed with the same columns
#'   as \code{lulc_Mulstistep} \code{tibble}}
#'   \item{tb_legend}{\code{<tibble>} A table of the pixel value, their names and colors, contains 3 columns:
#'   \enumerate{
#'   \item classValue: \code{<int>} the pixel value of the classes of land use
#'   \item className: \code{<fct>} The name or legend associated with a given pixel value
#'   \item color: \code{<chr>} The color associate with the given pixel value
#'   }}
#'   \item{totalArea}{\code{<tibble>} A table of two columns with the total area of study, contains 2 columns:
#'   \enumerate{
#'   \item area_km2: \code{<dbl>} The total area of study in km2
#'   \item QtPixel: \code{<int>} The total area of study in quantity of pixel
#'   }}
#'   \item{totalInterval}{\code{<int>} A number represinting the whole interval of time analysed}
#'   }
#'
#' @keywords datasets
#'
#'
#' @source \url{https://www.embrapa.br/pantanal/bacia-do-alto-paraguai}
"SL_2002_2014"
