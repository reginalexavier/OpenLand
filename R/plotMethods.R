#' @include intensity.R
NULL

#' Title: setting a method in the generic plot for the openland class IntensityL01
#'
#' @param x IntensityL01.
#' @param y missing.
#'
#' @return
#' @export
#'
#' @examples
setMethod(
  f = "plot",
  signature = c(x = "IntensityL01", y = "missing"),
  definition = function(x,
                        y = "missing",
                        labels = c(
                          leftlabel = bquote("Mudancas de area" ~ "(" ~ Km ^ 2 ~ ")"),
                          rightlabel = "Taxa de mudança (%)",
                          title = "Level 01 Mudanças todo o Periodo"
                        ),
                        labs = c(type = "Mudanças", ur = "U"),
                        marginplot = c(lh = -10, rh = 0),
                        leg_curv = c(x = 1 / 10, y = 1 / 10),
                        color_bar = c(faster = "#B22222",
                                      slower = "#006400",
                                      area = "gray40"),
                        ...) {
    dataset <- x$tabela

    GL01_taxa <-
      dataset %>% ggplot(aes(fct_rev(dataset[[1]]), dataset[[3]])) +
      geom_bar(
        aes(fill = tipo),
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
        ), "pt")
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
        ), "pt")
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



#' Title: setting a method in the generic plot for the openland class IntensityL02
#'
#' @param x IntensityL02.
#' @param y missing.
#'
#' @return
#' @export
#'
#' @examples
setMethod(
  "plot",
  signature(x = "IntensityL02", y = "missing"),
  definition = function(x,
                        y = "missing",
                        labels = c(
                          leftlabel =  bquote("Ganho de area (" ~ Km ^ 2 ~ ")"),
                          rightlabel = "Intensidade de ganho (%)",
                          title = "Level 02 Ganho Anual"
                        ),
                        labs = c(type = "Classes", ur = "STt"),
                        marginplot = c(lh = 0.5, rh = 0.5),
                        leg_curv = c(x = 1 / 10, y = 1 / 10),
                        ...) {
    dataset <- x$tabela
    lookupcolor <- x$color

    GL02_ganho_taxa <-
      dataset %>% ggplot(aes(fct_rev(dataset[[2]]), dataset[[6]])) +
      geom_bar(aes(fill = dataset[[2]]), stat = "identity", position = "dodge") +
      facet_wrap( ~ dataset[[1]], ncol = 1) +
      scale_fill_manual(values =  unname(lookupcolor[as.character(unique(dataset[[2]]))
                                                     [order(match(as.character(unique(dataset[[2]])),
                                                                  levels(dataset[[2]])))]])) +
      xlab(NULL) +
      ylab(NULL) +
      geom_hline(aes(yintercept = 0), size = .3) +
      geom_hline(aes(yintercept = dataset[[8]], color = names(dataset)[[8]]),
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
          y = dataset[[8]],
          xend = (length(unique(dataset[[2]])) / 2) + leg_curv[[1]],
          yend = dataset[[8]] + leg_curv[[2]]
        ),
        curvature = .1,
        arrow = arrow(length = unit(2, "mm"), ends = "first")
      ) +

      geom_text(
        aes(x = (length(unique(
          dataset[[2]]
        )) / 2) + leg_curv[[1]],
        y = dataset[[8]] + leg_curv[[2]]),
        label = paste(round(dataset[[8]], 2), "%"),
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
        ), "pt")
      )

    #Area ----

    GL02_ganho_area <-
      dataset %>% ggplot(aes(fct_rev(dataset[[2]]), dataset[[4]])) +
      geom_bar(aes(fill = dataset[[2]]), stat = "identity", position = "dodge") +
      facet_wrap( ~ dataset[[1]], ncol = 1) +
      scale_fill_manual(values = unname(lookupcolor[as.character(unique(dataset[[2]]))
                                                    [order(match(as.character(unique(dataset[[2]])),
                                                                 levels(dataset[[2]])))]])) +
      xlab(NULL) +
      ylab(NULL) +
      geom_hline(aes(yintercept = 1), size = .3) +
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
        ), "pt")

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



#' Title: setting a method in the generic plot for the openland class IntensityL03
#'
#' @param x IntensityL03.
#' @param y missing.
#'
#' @return
#' @export
#'
#' @examples
setMethod(
  f = "plot",
  signature = c(x = "IntensityL03", y = "missing"),
  definition = function(x,
                        y,
                        labels = c(
                          rightlabel = bquote("Intensidade de ganho da classe" ~ .(as.character(dataset[[7]][[1]])) ~ "(%)"),
                          leftlabel = bquote("Ganho da classe" ~ .(as.character(dataset[[7]][[1]])) ~ "(" ~ Km ^ 2 ~ ")"),
                          title = "Level 03 Ganho da Ap"
                        ),
                        labs = c(type = "Classes", ur = "Wtn/Vtm"),
                        marginplot = c(lh = 0.5, rh = 0.5),
                        leg_curv = c(x = 1 / 10, y = 1 / 10),
                        ...) {
    dataset <- x$tabela
    lookupcolor <- x$color

    GL03_ganho_taxa <-
      dataset %>% ggplot(aes(fct_rev(dataset[[2]]), dataset[[6]])) +
      geom_bar(aes(fill = dataset[[2]]), stat = "identity", position = "dodge") +
      facet_wrap( ~ dataset[[1]], ncol = 1) +
      scale_fill_manual(values = unname(lookupcolor[as.character(unique(dataset[[2]]))
                                                    [order(match(as.character(unique(dataset[[2]])),
                                                                 levels(dataset[[2]])))]])) +
      xlab(NULL) +
      ylab(NULL) +
      geom_hline(aes(yintercept = 0), size = .01) +
      geom_hline(aes(yintercept = dataset[[10]], color = names(dataset)[[10]]),
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
          y = dataset[[10]],
          xend = (length(unique(dataset[[2]])) / 2) + leg_curv[[1]],
          yend = dataset[[10]] + leg_curv[[2]]
        ),
        curvature = .1,
        arrow = arrow(length = unit(2, "mm"), ends = "first")
      ) +

      geom_text(
        aes(x = (length(unique(
          dataset[[2]]
        )) / 2) + leg_curv[[1]],
        y = dataset[[10]] + leg_curv[[2]]),
        label = paste(round(dataset[[10]], 2), "%"),
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
        ), "pt")
      )

    #Area ----
    #ganho da classe n from as classes i
    #plot03 em termo de area em km2 de mudança
    GL03_ganho_area <- dataset %>%
      ggplot(aes(fct_rev(dataset[[2]]), dataset[[4]])) +
      geom_bar(aes(fill = dataset[[2]]), stat = "identity", position = "dodge") +
      facet_wrap( ~ dataset[[1]], ncol = 1) +
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
        ), "pt")
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
