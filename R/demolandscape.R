#' Create Raster with Random pixel Value
#'
#' This function create a raster serie with some setup like the layer name and
#' the sample value for the lulc
#'
#' @param year A list of year, first and last
#' @param nrows nrows of the raster
#' @param ncols ncols of the raster
#' @param res the resolution of the raster to be created
#' @param xmn x minimun extent
#' @param xmx x maximum extent
#' @param ymn y minimum extent
#' @param ymx y maximum extent
#' @param crs the coordinate referencing system
#' @param class the raster classes
#' @param prob the probability of occorence for the class list
#' @param sim.autocorrelate logical This argument define if the simulation will
#'     be a autocorrelate in space or pixel of independent values.
#' @param bin.interval logical This argument define if the categorisation of the
#'     value will be via igual interval or igual number see `ggplot2`
#' @param psill sill see variogram
#' @param model a model see variogram
#' @param range a range of the variability
#' @param variance character; variance function to transform to non-stationary
#'     covariances; "identity" does not transform, other options are "mu"
#'     (Poisson) and "mu(1-mu)" (binomial)
#'
#' @import dplyr
#' @importFrom grDevices hcl.colors
#' @return list
#' @export
#'
#' @examples
#' demo_landscape(2000:2005, res = 1)




demo_landscape <- function(year,
                           nrows = 100,
                           ncols = 100,
                           res = 1,
                           xmn = 0,
                           xmx = 100,
                           ymn = 0,
                           ymx = 100,
                           crs = NA,
                           class = 1:5,
                           prob = NULL,
                           sim.autocorrelate = TRUE,
                           bin.interval = TRUE,
                           psill = 0.025,
                           model = "Exp",
                           range = 1000,
                           variance = c("identity", "mu" ,  "mu(1-mu)")[1]) {
  # a sample raster
  landscape <- raster::raster(
    nrows = nrows,
    ncols = ncols,
    xmn = xmn,
    xmx = xmx,
    ymn = ymn,
    ymx = ymx,
    resolution = res,
    crs = crs
  )

  mapdemo <-
    function(year01, pixvalue) {
      # an atribuitor of values

      raster::values(landscape) <- pixvalue

      names(landscape) <- paste0("landscape_", year01)
      landscape
    }

  samplerow <- nrow(landscape)
  samplecol <- ncol(landscape)

  if (isFALSE(sim.autocorrelate)) {
    pixsample <-
      lapply(year, function(x)
        base::sample(
          class,
          samplerow * samplecol,
          replace = T,
          prob = prob
        ))
  } else {
    # setting the dimention
    # create structure
    xy <- expand.grid(1:samplerow, 1:samplecol)
    names(xy) <- c("x", "y")

    # define the gstat object (spatial model)
    g.dummy <-
      gstat::gstat(
        formula = z ~ 1,
        locations =  ~ x + y,
        dummy = TRUE,
        beta = 1,
        model = gstat::vgm(
          psill = psill,
          model = model,
          range = range,
          variance = variance
        ),
        nmax = 20
      )

    # make four simulations based on the stat object
    simulations <- stats::predict(g.dummy, newdata = xy, nsim = length(year))

    simulations <-  as_tibble(simulations)[, -(1:2)]

    pixsample <- lapply(seq_len(ncol(simulations)),
                        function(x)
                          if (isTRUE(bin.interval)) {
                            ggplot2::cut_interval(simulations[[x]], n = length(class), labels = FALSE)
                          } else {
                            ggplot2::cut_number(simulations[[x]], n = length(class), labels = FALSE)
                          })

  }

  # create a list of n of them
  raster_list <-
    mapply(function(x, y)
      mapdemo(year01 = x, pixvalue = y), year, pixsample)

  return(raster_list)
}
