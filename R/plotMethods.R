#' @include intensityAnalysis.R
NULL

#' Plot method for objects from Intensity Analysis
#'
#' Plot \code{Intensity} objects based on Intensity Analysis tables
#'
#' @param Interval the class
#' @param x tibble. An object from the intensity analysis data
#' @param y not used
#' @param labels character. left right and title
#' @param labs character
#' @param marginplot character
#' @param leg_curv character
#' @param color_bar character
#' @param \dots additional arguments themes parameters \code{\link[ggplot2]{theme}}
#'
#' @return An intensity graph
#'
#'
#'
#' @importFrom ggplot2 ggplot aes geom_bar geom_hline scale_y_continuous scale_x_continuous
#'    ylab xlab labs scale_color_manual coord_flip guides guide_legend scale_fill_manual geom_curve
#'    theme facet_wrap scale_y_reverse scale_x_discrete expand_scale arrow unit geom_text geom_bar
#'    geom_segment ggtitle element_text element_blank margin
#'    theme_gray theme_bw theme_linedraw theme_light theme_dark theme_minimal
#'    theme_classic theme_void
#' @importFrom forcats fct_rev
#' @importFrom graphics plot
#'
#'
#'
#' @name plot
#' @rdname plot
#' @aliases plot,Interval,ANY-method
#'

setMethod(
  f = "plot",
  signature = "Interval",
  definition = function(x,
                        y,
                        labels = c(leftlabel = "Interval Change Area (percent of map)",
                                   rightlabel = "Annual Change Area (percent of map)",
                                   title = "Level 01 Mudancas todo o Periodo"),
                        labs = c(type = "Changes", ur = "Uniform Rate"),
                        marginplot = c(lh = -10, rh = 0),
                        leg_curv = c(x = 1 / 10, y = 1 / 10),
                        color_bar = c(faster = "#B22222",
                                      slower = "#006400",
                                      area = "gray40"),
                        ...) {
    Type <- St <-  U <-  NULL

    dataset <-
      x@intervalData %>% dplyr::mutate(Type = ifelse(St > U, "Fast", "Slow"))


    GL01_taxa <-
      dataset %>% ggplot(aes(fct_rev(dataset[[1]]), dataset[[3]])) +
      geom_bar(
        aes(fill = Type),
        stat = "identity",
        position = "dodge",
        width = .5
      ) +
      geom_hline(aes(yintercept = dataset[[4]], color = "U"),
                 linetype = 5,
                 size = .5) +
      geom_hline(aes(yintercept = 0), size = .01) +
      scale_fill_manual(values = c(color_bar[[1]], color_bar[[2]])) +
      ylab(NULL) +
      scale_color_manual(values = "black") +
      labs(fill = labs[[1]], color = labs[[2]]) +
      scale_y_continuous(expand = expand_scale(mult = c(0, .01))) +
      scale_x_discrete(expand = expand_scale(mult = c(0.06, 0.06))) +
      guides(fill = guide_legend(order = 1),
             color = guide_legend(order = 2)) +
      coord_flip() +
      geom_curve(
        aes(
          x = length(unique(dataset[[1]])) / 2,
          y = dataset[[4]],
          xend = (length(unique(dataset[[1]])) / 2) + leg_curv[[1]],
          yend = dataset[[4]] + leg_curv[[2]]
        ),
        size = .6,
        curvature = .1,
        arrow = arrow(length = unit(2, "mm"), ends = "first")
      ) +
      geom_text(
        aes(x = (length(unique(
          dataset[[1]]
        )) / 2) + leg_curv[[1]],
        y = dataset[[4]] + leg_curv[[2]]),
        label = paste(round(dataset[[4]], 2), "%"),
        colour = "black",
        fontface = "plain",
        nudge_y = 1 / 100,
        hjust = "left"
      ) +
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_text(margin = margin(r = 8)),
        axis.ticks.length.y = unit(2, "pt"),
        legend.position = "right",
        legend.direction = "vertical",
        plot.margin = unit(c(
          t = 0,
          r = 0,
          b = 0,
          l = -(marginplot[[2]])
        ), "pt"), ...
      )

    #Area---------------
    GL01_area <-
      dataset %>% ggplot(aes(fct_rev(dataset[[1]]), dataset[[2]])) +
      geom_bar(
        stat = "identity",
        position = "dodge",
        width = .5,
        fill = color_bar[[3]]
      ) +
      coord_flip() +
      xlab(expression(paste("Periodo de tempo [ ", Y[t], ",", Y[t + 1], "]"))) +
      ylab(NULL) +
      geom_hline(aes(yintercept = 0), size = .01) +
      scale_y_reverse(expand = expand_scale(mult = c(0.01, 0))) +
      scale_x_discrete(position = "top", expand = expand_scale(mult = c(0.06, 0.06))) +
      theme(
        axis.title.y = element_blank(),
        axis.ticks.length.y = unit(2, "pt"),
        axis.text.y = element_blank(),
        plot.margin = unit(c(
          t = 0,
          r = -(marginplot[[1]]),
          b = 0,
          l = 0
        ), "pt"), ...
      )


    format_lab <- function(x, font = 1, size = 11) {
      grid::textGrob(x, gp = grid::gpar(fontface = font, fontsize = size))
    }

    if (!is.na(labels[[3]])) {
      title_lab <- format_lab(labels[[3]])
      left_lab <- format_lab(labels[[1]])
      right_lab <- format_lab(labels[[2]])

      my_layout <-
        matrix(c(rep(1, 6), NA, rep(rep(2:3, c(
          3, 4
        )), 20), rep(4:5, c(3, 3)), NA),
        ncol = 7,
        byrow = TRUE)

      gridExtra::grid.arrange(title_lab,
                              GL01_area,
                              GL01_taxa,
                              left_lab,
                              right_lab,
                              layout_matrix = my_layout)
    } else {
      left_lab <- format_lab(labels[[1]])
      right_lab <- format_lab(labels[[2]])

      my_layout <-
        matrix(c(rep(rep(2:3, c(
          3, 4
        )), 20), rep(4:5, c(3, 3)), NA),
        ncol = 7, byrow = TRUE)

      gridExtra::grid.arrange(GL01_area, GL01_taxa,
                              left_lab, right_lab,
                              layout_matrix = my_layout)

    }

  }
)



#' @rdname plot
#'
#' @param Category the class
setMethod(
  f = "plot",
  signature = "Category",
  definition = function(x,
                        y,
                        labels = c(
                          leftlabel = bquote("Gain/Loss area (" ~ Km ^ 2 ~ ")"),
                          rightlabel = "Intensity Gain/Loss (%)",
                          title = "Level 02 Ganho/Perda Anual"
                        ),
                        labs = c(type = "Classes", ur = "Uniform Rate"),
                        marginplot = c(lh = 0.5, rh = 0.5),
                        leg_curv = c(x = 1 / 10, y = 1 / 10),
                        ...) {
    dataset <- x@categoryData
    lookupcolor <- x@lookupcolor

    GL02_ganho_taxa <-
      dataset %>% ggplot(aes(fct_rev(dataset[[2]]), dataset[[5]])) +
      geom_bar(aes(fill = dataset[[2]]), stat = "identity", position = "dodge") +
      facet_wrap(~ dataset[[1]], ncol = 1) +
      scale_fill_manual(values =  unname(lookupcolor[as.character(unique(dataset[[2]]))
                                                     [order(match(as.character(unique(dataset[[2]])),
                                                                  levels(dataset[[2]])))]])) +
      xlab(NULL) +
      ylab(NULL) +
      geom_hline(aes(yintercept = 0), size = .3) +
      geom_hline(aes(yintercept = dataset[[6]], color = names(dataset)[[6]]),
                 linetype = 5,
                 size = .3) +
      scale_color_manual(values = "black") +
      coord_flip() +
      labs(fill = labs[[1]], colour = labs[[2]]) +
      scale_y_continuous(expand = expand_scale(mult = c(0, .01))) +
      guides(fill = guide_legend(order = 1),
             color = guide_legend(order = 2)) +

      geom_curve(
        aes(
          x = length(unique(dataset[[2]])) / 2,
          y = dataset[[6]],
          xend = (length(unique(dataset[[2]])) / 2) + leg_curv[[1]],
          yend = dataset[[6]] + leg_curv[[2]]
        ),
        curvature = .1,
        arrow = arrow(length = unit(2, "mm"), ends = "first")
      ) +

      geom_text(
        aes(x = (length(unique(
          dataset[[2]]
        )) / 2) + leg_curv[[1]],
        y = dataset[[6]] + leg_curv[[2]]),
        label = paste(round(dataset[[6]], 2), "%"),
        colour = "black",
        fontface = "bold",
        nudge_y = 1 / 100,
        hjust = "left"
      ) +
      theme(
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 13, face = "plain"),
        axis.ticks.length.y = unit(0, "pt"),
        legend.position = "right",
        legend.direction = "vertical",
        plot.margin = unit(c(
          t = 0,
          r = 0,
          b = 0,
          l = -(marginplot[[2]])
        ), "pt"), ...
      )

    #Area ----

    GL02_ganho_area <-
      dataset %>% ggplot(aes(fct_rev(dataset[[2]]), dataset[[4]])) +
      geom_bar(aes(fill = dataset[[2]]), stat = "identity", position = "dodge") +
      facet_wrap(~ dataset[[1]], ncol = 1) +
      scale_fill_manual(values = unname(lookupcolor[as.character(unique(dataset[[2]]))
                                                    [order(match(as.character(unique(dataset[[2]])),
                                                                 levels(dataset[[2]])))]])) +
      xlab(NULL) +
      ylab(NULL) +
      geom_hline(aes(yintercept = 0), size = .3) +
      coord_flip() +
      labs(fill = "Classes") +
      scale_y_reverse(expand = expand_scale(mult = c(0.01, 0))) +
      scale_x_discrete(position = "top") +
      theme(
        axis.ticks.length.y = unit(0, "pt"),
        axis.title.x = element_text(size = 12, face = "plain"),
        axis.text.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(
          t = 0,
          r = -(marginplot[[1]]),
          b = 0,
          l = 0
        ), "pt"), ...

      )

    format_lab <- function(x, font = 1, size = 11) {
      grid::textGrob(x, gp = grid::gpar(fontface = font, fontsize = size))
    }

    if (!is.na(labels[[3]])) {
      title_lab <- format_lab(labels[[3]])
      left_lab <- format_lab(labels[[1]])
      right_lab <- format_lab(labels[[2]])

      my_layout <-
        matrix(c(rep(1, 6), NA, rep(rep(2:3, c(
          3, 4
        )), 20), rep(4:5, c(3, 3)), NA),
        ncol = 7,
        byrow = TRUE)

      gridExtra::grid.arrange(
        title_lab,
        GL02_ganho_area,
        GL02_ganho_taxa,
        left_lab,
        right_lab,
        layout_matrix = my_layout
      )
    } else {
      left_lab <- format_lab(labels[[1]])
      right_lab <- format_lab(labels[[2]])

      my_layout <-
        matrix(c(rep(rep(2:3, c(
          3, 4
        )), 20), rep(4:5, c(3, 3)), NA),
        ncol = 7, byrow = TRUE)

      gridExtra::grid.arrange(GL02_ganho_area,
                              GL02_ganho_taxa,
                              left_lab,
                              right_lab,
                              layout_matrix = my_layout)

    }

  }
)





#' @rdname plot
#'
#' @param Transition the class
setMethod(
  f = "plot",
  signature = "Transition",
  definition = function(x,
                        y,
                        labels = c(
                          rightlabel = bquote("Intensity Gain/Loss of" ~ .(as.character(dataset[[3]][[1]])) ~ "(%)"),
                          leftlabel = bquote("Gain/Loss of " ~ .(as.character(dataset[[3]][[1]])) ~ "(" ~ Km ^ 2 ~ ")"),
                          title = "Level 03 Ganho da classe m/n"
                        ),
                        labs = c(type = "Classes", ur = "Uniform Rate"),
                        marginplot = c(lh = 0.5, rh = 0.5),
                        leg_curv = c(x = 1 / 10, y = 1 / 10),
                        ...) {
    dataset <- x@transitionData
    lookupcolor <- x@lookupcolor

    GL03_ganho_taxa <-
      dataset %>% ggplot(aes(fct_rev(dataset[[2]]), dataset[[6]])) +
      geom_bar(aes(fill = dataset[[2]]), stat = "identity", position = "dodge") +
      facet_wrap(~ dataset[[1]], ncol = 1) +
      scale_fill_manual(values = unname(lookupcolor[as.character(unique(dataset[[2]]))
                                                    [order(match(as.character(unique(dataset[[2]])),
                                                                 levels(dataset[[2]])))]])) +
      xlab(NULL) +
      ylab(NULL) +
      geom_hline(aes(yintercept = 0), size = .01) +
      geom_hline(aes(yintercept = dataset[[7]], color = names(dataset)[[7]]),
                 linetype = 5,
                 size = .3) +
      scale_color_manual(values = "black") +
      coord_flip() +
      labs(fill = labs[[1]], colour = labs[[2]]) +
      scale_y_continuous(expand = expand_scale(mult = c(0, .01))) +
      guides(fill = guide_legend(order = 1),
             color = guide_legend(order = 2)) +
      geom_curve(
        aes(
          x = length(unique(dataset[[2]])) / 2,
          y = dataset[[7]],
          xend = (length(unique(dataset[[2]])) / 2) + leg_curv[[1]],
          yend = dataset[[7]] + leg_curv[[2]]
        ),
        curvature = .1,
        arrow = arrow(length = unit(2, "mm"), ends = "first")
      ) +

      geom_text(
        aes(x = (length(unique(
          dataset[[2]]
        )) / 2) + leg_curv[[1]],
        y = dataset[[7]] + leg_curv[[2]]),
        label = paste(round(dataset[[7]], 2), "%"),
        colour = "black",
        fontface = "bold",
        nudge_y = 1 / 100,
        hjust = "left"
      ) +
      theme(
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 13, face = "plain"),
        axis.ticks.length.y = unit(0, "pt"),
        legend.position = "right",
        legend.direction = "vertical",
        plot.margin = unit(c(
          t = 0,
          r = 0,
          b = 0,
          l = -(marginplot[[2]])
        ), "pt"), ...
      )

    #Area ----
    #ganho da classe n from as classes i
    GL03_ganho_area <- dataset %>%
      ggplot(aes(fct_rev(dataset[[2]]), dataset[[5]])) +
      geom_bar(aes(fill = dataset[[2]]), stat = "identity", position = "dodge") +
      facet_wrap(~ dataset[[1]], ncol = 1) +
      scale_fill_manual(values = unname(lookupcolor[as.character(unique(dataset[[2]]))
                                                    [order(match(as.character(unique(dataset[[2]])),
                                                                 levels(dataset[[2]])))]])) +
      xlab(NULL) +
      ylab(NULL) +
      geom_hline(aes(yintercept = 0), size = .01) +
      coord_flip() +
      labs(fill = "Classes") +
      scale_y_reverse(expand = expand_scale(mult = c(0.01, 0))) +
      scale_x_discrete(position = "top") +
      theme(
        axis.ticks.length.y = unit(0, "pt"),
        axis.title.x = element_text(size = 12, face = "plain"),
        axis.text.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(
          t = 0,
          r = -(marginplot[[1]]),
          b = 0,
          l = 0
        ), "pt"), ...
      )


    format_lab <- function(x, font = 1, size = 11) {
      grid::textGrob(x, gp = grid::gpar(fontface = font, fontsize = size))
    }

    if (!is.na(labels[[3]])) {
      title_lab <- format_lab(labels[[3]])
      left_lab <- format_lab(labels[[2]])
      right_lab <- format_lab(labels[[1]])

      my_layout <-
        matrix(c(rep(1, 6), NA, rep(rep(2:3, c(
          3, 4
        )), 20), rep(4:5, c(3, 3)), NA),
        ncol = 7,
        byrow = TRUE)

      gridExtra::grid.arrange(
        title_lab,
        GL03_ganho_area,
        GL03_ganho_taxa,
        left_lab,
        right_lab,
        layout_matrix = my_layout
      )
    } else {
      left_lab <- format_lab(labels[[2]])
      right_lab <- format_lab(labels[[1]])

      my_layout <-
        matrix(c(rep(rep(2:3, c(
          3, 4
        )), 20), rep(4:5, c(3, 3)), NA),
        ncol = 7, byrow = TRUE)

      gridExtra::grid.arrange(GL03_ganho_area,
                              GL03_ganho_taxa,
                              left_lab,
                              right_lab,
                              layout_matrix = my_layout)

    }
  }
)
