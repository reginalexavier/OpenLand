utils::globalVariables(c("From", "To", "target", "km2", "Year",
                         "QtPixel", "yearFrom", "yearTo",
                         "colorFrom", "colorTo", "lulc", "area"))
#' @include plotMethods.R
NULL

#' A barplot for the area of every point in time of the analysed period
#'
#' @param dataset A table of the multi step transition
#' @param legendtable A table contains the legend of the land use classes and his respective color
#' @param title The title of the plot
#' @param color A color list for the three types of chages
#' @param fill Legend
#' @param xlab character Label for the x axe
#' @param ylab character Label for the y axe
#' @param area_km2 boolean TRUE for km2 unit, FALSE for pixel unit
#' @param \dots themes parameters \code{\link[ggplot2]{theme}}
#'
#'
#' @seealso \code{ggplot2::\link[ggplot2]{theme}}
#'
#' @return a barplot
#' @export
#'
#' @importFrom graphics par
#'
#'
barplotLand <-
  function(dataset,
           legendtable,
           title = "General Area in the interval (2002 - 2014)",
           color = c(GC = "gray70", NG = "#006400", NL = "#EE2C2C"),
           fill = "LULC Classes",
           xlab = "Year",
           ylab = expression(paste("Area ", Km ^ {2})),
           area_km2 = TRUE, ...) {

#    From <- To <- yearTo <- km2 <- yearFrom <- Year <- lulc <- area <- NULL


    datachange <- dataset %>%
      left_join(legendtable, by = c("From" = "classValue")) %>%
      left_join(legendtable, by = c("To" = "classValue")) %>%
      dplyr::select(-c(From, To)) %>%
      rename(
        "From" = "className.x",
        "To" = "className.y",
        "colorFrom" = "color.x",
        "colorTo" = "color.y"
      )

    # datanual %>% group_by(Year) %>% summarise(sum(area))

    areaif <- ifelse(isTRUE(area_km2), "km2", "QtPixel")

    datanual <-
      datachange %>% group_by(yearTo, To) %>%
      summarise(area = sum(!!as.name(areaif))) %>%
      rename("Year" = "yearTo", "lulc" = "To") %>% rbind(
        datachange[datachange$yearFrom == first(datachange$yearFrom),] %>%
          group_by(yearFrom, From) %>% # capturing the first year change
          summarise(area = sum(!!as.name(areaif))) %>%
          rename("Year" = "yearFrom", "lulc" = "From"))

    ggplot(data = datanual, aes(as.character(Year), area)) +
      geom_bar(aes(fill = lulc), stat = "identity", position = "dodge") +
      scale_fill_manual(values = legendtable$color[order(legendtable$className)]) +
      labs(fill = fill) +
      xlab(xlab) +
      ylab(ylab) +
      ggtitle(title) +
      theme(plot.title = element_text(hjust = .5),
            ...)

  }




#' A circlize plot for the transition between two times point, one step.
#'
#' @param dataset A table of the one step transition
#' @param legendtable A table contains the legend of the land use classes and his respective color
#' @param legposition A list of `x` and `y` parameter position for the legend
#' @param legtitle The title of the legen
#' @param sectorcol The color of the extern sector containing the years
#' @param area_km2 boolean TRUE for km2 unit, FALSE for pixel unit
#'
#' @return A Chord Diagram
#' @export
#'
#'
#'
chordDiagramLand <-
  function(dataset,
           legendtable,
           legposition = c(x = 1.8, y = 1.5),
           legtitle = "Classes",
           sectorcol = "gray80",
           area_km2 = TRUE) {

    # From <-
    #   To <-
    #   target <- km2 <- yearFrom <- yearTo <- colorFrom <- colorTo <- QtPixel <- NULL

    circle_data <- dataset %>%
      left_join(legendtable, by = c("From" = "classValue")) %>%
      left_join(legendtable, by = c("To" = "classValue")) %>%
      dplyr::select(-c(From, To)) %>%
      rename(
        "From" = "className.x",
        "To" = "className.y",
        "colorFrom" = "color.x",
        "colorTo" = "color.y"
      ) %>% tidyr::unite("source",
                  c("From", "yearFrom"),
                  sep = "-",
                  remove = FALSE) %>%
      tidyr::unite("target",
            c("To", "yearTo"),
            sep = "-",
            remove = FALSE) %>%
      dplyr::select(source,
                    target,
                    From,
                    To,
                    km2,
                    QtPixel,
                    yearFrom,
                    yearTo,
                    colorFrom,
                    colorTo)

    # input for the circlize function
    onestepcircle <-
      circle_data[order(circle_data$From), ][, c("source", "target", "km2", "QtPixel")]

    # seting the grid.color parameter automatic
    grid_a <- unique(circle_data$colorFrom[order(circle_data$From)])
    names(grid_a) <-
      unique(circle_data$source[order(circle_data$From)])

    grid_b <- unique(circle_data$colorTo[order(circle_data$To)])
    names(grid_b) <- unique(circle_data$target[order(circle_data$To)])

    grid.col <- c(grid_a, grid_b)


    # tablelegend$color[order(tablelegend$legend)]

    ano01 <- unique(onestepcircle$source)
    ano02 <- unique(onestepcircle$target)

    #
    if (isTRUE(area_km2)) {
    onestepcircle <- onestepcircle[c(1,2,3)]} else {
      onestepcircle <- onestepcircle[c(1,2,4)]
    }

    # the legend
    legenda <- ComplexHeatmap::Legend(
      at = levels(circle_data$From),
      type = "grid",
      grid_height = unit(6, "mm"),
      grid_width = unit(3, "mm"),
      gap = unit(1, "mm"),
      labels_gp = grid::gpar(fontsize = 12),
      #labels_rot = 0,
      legend_gp = grid::gpar(fill = legendtable$color[order(legendtable$className)]),
      direction = c("vertical", "horizontal")[2],
      background = "#EEEEEE",
      title_position = c(
        "topleft",
        "topcenter",
        "leftcenter",
        "lefttop",
        "leftcenter-rot",
        "lefttop-rot"
      )[1],
      title = legtitle #,
      #title_gap = unit(2, "mm")
    )

    # parameters
    #circos.clear()
    circlize::circos.par(
      start.degree = 0,
      gap.degree = 1,
      track.margin = c(-0.01, 0.015),
      points.overflow.warning = T
    )
    par(mar = rep(0, 4))
    # the base plot
    circlize::chordDiagram(
      x = onestepcircle,
      grid.col = grid.col,
      transparency = 0.25,
      directional = 1,
      direction.type = c("arrows", "diffHeight"),
      diffHeight  = -0.02,
      annotationTrack = c("name", "grid")[2],
      annotationTrackHeight = c(0.05, 0.1),
      link.arr.type = "big.arrow",
      link.sort = TRUE,
      link.decreasing = F,
      link.largest.ontop = TRUE,
      preAllocateTracks = list(
        track.height = circlize::uh(5, "mm"),
        track.margin = c(circlize::uh(4, "mm"), 0)
      )
    )

    #the km2 label
    for (si in circlize::get.all.sector.index()) {
      circlize::circos.axis(
        h = "top",
        labels.cex = .6,
        sector.index = si,
        track.index = 2
      )
    }
    #adding the externs arcs 2002 & 2014

    circlize::highlight.sector(
      ano01,
      track.index = 1,
      col = sectorcol,
      text = circle_data$yearFrom[1],
      cex = 0.9,
      text.col = "black",
      niceFacing = TRUE
    )
    circlize::highlight.sector(
      ano02,
      track.index = 1,
      col = sectorcol,
      text = circle_data$yearTo[1],
      cex = 0.9,
      text.col = "black",
      niceFacing = TRUE
    )
    #the legend
    ComplexHeatmap::draw(
      legenda,
      x = unit(legposition[[1]], "cm"),
      y = unit(legposition[[2]], "cm"),
      just = c("right", "bottom")
    )

    circlize::circos.clear()
  }




#' A barplot for the Net Gain/Loss and Gross Changes between land use classes for the period analysed
#'
#' @param dataset A table of multi step transition
#' @param legendtable A table contains the legend of the land use classes and his respective color
#' @param title character. The title of the plot (optional), use \code{NULL} for no title.
#' @param xlab character. Label for the x axe
#' @param ylab character. Label for the y axe
#' @param changesLabel character. Labels for the three types of chages, the default is c(GC = "Gross chages", NG = "Net Gain", NL = "Net Loss")
#' @param color character. Colors for the three types of chages
#' @param area_km2 boolean. TRUE for km2 unit, FALSE for pixel unit
#'
#'
#'
#' @return A bar plot
#' @export
#'
#' @examples
#' test1 <- demo_landscape(2000:2005)
#' test2 <- contingenceTable(input_raster = test1, pixelresolution = 1)
#' netgrossplot(dataset = test2$lulc_Multistep, legendtable = test2$tb_legend, area_km2 = FALSE)
#'
#'
netgrossplot <-
  function(dataset,
           legendtable,
           title = "General Changes (First year - Last year)",
           xlab = "Land Use Classes",
           ylab = "Area (Kilometer)",
           changesLabel = c(GC = "Gross changes", NG = "Net Gain", NL = "Net Loss"),
           color = c(GC = "gray70", NG = "#006400", NL = "#EE2C2C"),
           area_km2 = TRUE) {
#    From <- To <- km2 <- QtPixel <- area <- NULL
    datachange <- (dataset %>%
      left_join(legendtable, by = c("From" = "classValue")) %>%
      left_join(legendtable, by = c("To" = "classValue")) %>%
      dplyr::select(-c(From, To)) %>%
      rename(
        "From" = "className.x",
        "To" = "className.y"))[c(1, 2, 3, 7, 9)] #Period, km2, QtPixel, From, To

    areaif <- ifelse(isTRUE(area_km2), "km2", "QtPixel")


    lulc_gain <- datachange %>% dplyr::filter(From != To)

    lulc_loss <- lulc_gain %>% rename("To" = "From", "From" = "To") %>%
      mutate(km2 = -1 * km2, QtPixel = -1 * QtPixel)


    lulc_gainloss_gross <- rbind(lulc_gain, lulc_loss) %>%
      mutate(changes = ifelse(QtPixel > 0, "Gain", "Loss"))


    lulc_gainLoss_net <-
      lulc_gainloss_gross %>% group_by(To) %>% summarise(area = sum(!!as.name(areaif))) %>%
      mutate(changes = ifelse(area > 0, changesLabel[[2]], changesLabel[[3]]))

    if (isTRUE(area_km2)) {
      lulc_gainloss_gross <- lulc_gainloss_gross[c(1, 2, 4, 5, 6)]
    } else {
      lulc_gainloss_gross <- lulc_gainloss_gross[c(1, 3, 4, 5, 6)]
    }


    ggplot(data = lulc_gainloss_gross, aes(To, lulc_gainloss_gross[[2]])) +
      geom_bar(stat = "identity", width = 0.5, aes(fill = changesLabel[[1]])) +
      geom_bar(
        aes(x = To, y = area, fill = changes),
        data = lulc_gainLoss_net,
        stat = "identity",
        width = 0.4,
        inherit.aes = F
      ) +
      geom_segment(data = lulc_gainLoss_net,
                   aes(
                     x = as.numeric(To) - 0.3,
                     y = area,
                     xend = as.numeric(To) + 0.3,
                     yend = area
                   )) +
      scale_fill_manual(values = c(color[[1]], color[[2]], color[[3]])) +
      labs(fill = "Changes") +
      geom_hline(yintercept = 0, size = .3) +
      xlab(xlab) +
      ylab(ylab) +
      ggtitle(title) +
      theme(plot.title = element_text(hjust = .5))
  }



#' A sandkey plot for the land use land cover transition for the period analysed
#'
#' @param dataset Table of one step transition for a onestep sankey plot or a table multi step
#' transition for a multistep sankey plot.
#' @param legendtable A table contains the legend of the land use classes and his respective color
#' @param iterations numeric. Number of iterations in the diagramm layout for computation
#' of the depth (y-position) of each node.
#'
#' @seealso \code{\link[networkD3]{sankeyNetwork}}
#'
#'
#' @return A land use land cover transition sankey plot for the period analysed
#' @export
#'
#'
sankeyLand <- function(dataset, legendtable, iterations = 0) {
  From <- To <- target <- km2 <- yearFrom <- yearTo <- name <- NULL
  linkMultistep <- dataset %>%
    left_join(legendtable, by = c("From" = "classValue")) %>%
    left_join(legendtable, by = c("To" = "classValue")) %>%
    dplyr::select(-c(From, To)) %>%
    rename(
      "From" = "className.x",
      "To" = "className.y",
      "colorFrom" = "color.x",
      "colorTo" = "color.y"
    ) %>% tidyr::unite("source",
                c("From", "yearFrom"),
                sep = "-",
                remove = FALSE) %>%
    tidyr::unite("target",
          c("To", "yearTo"),
          sep = "-",
          remove = FALSE) %>%
    dplyr::select(source, target, From, To, km2, yearFrom, yearTo)
  # defining the color scale
  domain <- paste(paste0("'",
                         as.character(levels(legendtable$className)), "'"),
                  collapse = ", ")

  range <- paste(paste0("'",
                        as.character(legendtable$color[order(legendtable$className)]), "'"),
                 collapse = ", ")

  colorScale <-
    paste0(
      "d3.scaleOrdinal().domain([",
      domain,
      "]).range([",
      range,
      "]).unknown(['grey']);"
    )


  nodeMultistep <-
    data.frame(name = c(
      as.character(linkMultistep[order(linkMultistep$From), ]$source),
      as.character(linkMultistep[order(linkMultistep$To), ]$target)
    ) %>% unique()) %>%
    tidyr::separate(name, c("name02", "year"), sep = "-", remove = F)

  linkMultistep$IDsource <-
    match(linkMultistep$source, nodeMultistep$name) - 1
  linkMultistep$IDtarget <-
    match(linkMultistep$target, nodeMultistep$name) - 1

  # Plot
  networkD3::sankeyNetwork(
    Links = as.data.frame(linkMultistep),
    Nodes = nodeMultistep,
    Source = "IDsource",
    Target = "IDtarget",
    colourScale = colorScale,
    Value = "km2",
    NodeID = "name02",
    fontSize = 13,
    nodeWidth = 20,
    fontFamily = "sans-serif",
    iterations = iterations,
    nodePadding = 20,
    sinksRight = FALSE
  )
}
