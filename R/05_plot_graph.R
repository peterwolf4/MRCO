#' Plot Graph Structure MRCO
#' @description uses MRCO internal arguments to plot tree like layouts
#' @param metadata_column_name character or tidy-selection style unquoted name of a metadata column to plot piechart nodes from
#' @param graph_layout tibble, used graph_layout to plot and to be summarized
#' @param graph_arc tibble, used graph_arc to plot piecharts
#' @param edge_num_size_filter numeric, give the number of cells that must be surpassed to draw edge, has to be set for each data set as its highly dependent on nr of cells within data
#' @param edge_prop_size_filter numeric, give the proportion of cells that must move between nodes to draw edge
#' @param highlight_selection logical, TRUE to highlight selected nodes
#' @param plot_col_gradient NULL for auto detect, or logical, TRUE to use continuous fill, FALSE for discrete fill; only applies when continuous metadata column is selected
#' @param no_labels logical, TRUE to turn off cluster labels, FALSE default for labeled clusters
#' @import ggplot2 ggnewscale dplyr tidygraph ggraph
#' @importFrom rlang quo_is_null


plot_graph_MRCO <- function(metadata_column_name,
                            graph_layout,
                            graph_arc,
                            edge_num_size_filter,
                            edge_prop_size_filter,
                            highlight_selection = TRUE,
                            plot_col_gradient,
                            no_labels) {
  # check which plot to create
  if (quo_is_null(metadata_column_name)) {
    plot_MRCO <- createplot_simplenode(
      graph_layout = graph_layout,
      graph_arc = graph_arc,
      edge_num_size_filter = edge_num_size_filter,
      edge_prop_size_filter = edge_prop_size_filter,
      highlight_selection = highlight_selection,
      no_labels = no_labels
    )
  } else {
    plot_MRCO <- createplot_piechart(
      metadata_column_name = metadata_column_name,
      graph_layout = graph_layout,
      graph_arc = graph_arc,
      edge_num_size_filter = edge_num_size_filter,
      edge_prop_size_filter = edge_prop_size_filter,
      highlight_selection = highlight_selection,
      plot_col_gradient = plot_col_gradient,
      no_labels = no_labels
    )
  }

  # use MRCO plot theme
  plot_MRCO <- plot_MRCO +
    theme(
      plot.background = element_rect(fill = "white"),
      axis.line.y = element_line(
        size = 0.2,
        colour = alpha("black", 0),
        linetype = 1
      ),
      axis.text.y = element_text(size = 8),
      axis.ticks.y = element_line(size = .1, linetype = 1),
      axis.ticks.length.y = unit(1, "mm"),
      panel.grid.major.y = element_line(color = alpha("lightblue", 0.35)),
      panel.background = element_blank()
    )

  plot(plot_MRCO)
  return(plot_MRCO)
}




#' create simple node plot MRCO
#' @description create MRCO plot with simple nodes (single colour per node).
#' @param graph_layout tibble, used graph_layout to plot and to be summarized
#' @param graph_arc tibble, used graph_arc to plot piecharts
#' @param edge_num_size_filter numeric, give the number of cells that must be surpassed to draw edge, has to be set for each data set as its highly dependent on nr of cells within data
#' @param edge_prop_size_filter numeric, give the proportion of cells that must move between nodes to draw edge
#' @param highlight_selection logical, TRUE to highlight selected nodes
#' @param no_labels logical, TRUE to turn off cluster labels, FALSE default for labeled clusters
#' @import ggplot2 ggnewscale dplyr tidygraph ggraph

createplot_simplenode <- function(graph_layout,
                                  graph_arc,
                                  edge_num_size_filter,
                                  edge_prop_size_filter,
                                  highlight_selection = TRUE,
                                  no_labels) {
  plot_simplenode <- ggraph(graph_layout) +
    geom_edge_diagonal(
      aes(
        edge_alpha = .data$e_prop_size,
        filter = .data$e_prop_size >= edge_prop_size_filter &
          .data$e_size >= edge_num_size_filter
      ),
      show.legend = c("alpha" = TRUE, "colour" = FALSE)
    ) +
    scale_edge_alpha(name = "Edge Size prop")

  if (highlight_selection &
    any(graph_layout$is_selected == TRUE)) {
    plot_simplenode <- plot_simplenode +
      geom_node_point(aes(
        colour = .data$n_psize,
        size = .data$is_selected,
        alpha = .data$is_selected
      )) +
      scale_size_manual(
        name = "Is Selected",
        values = c("TRUE" = 8, "FALSE" = 6),
        guide = "none"
      ) +
      scale_alpha_manual(
        name = "Is Selected",
        values = c("TRUE" = 1, "FALSE" = .25),
        guide = "none"
      )
  } else {
    plot_simplenode <- plot_simplenode +
      geom_node_point(aes(colour = .data$n_psize),
        size = 8
      )
  }

  plot_simplenode <- plot_simplenode +
    scale_color_viridis_c(name = "Node Size proportional", begin = 0.5) +
    coord_fixed() +
    scale_y_continuous(
      breaks = unique(graph_layout$y),
      labels = unique(as.character(graph_layout$resolution))
    )+
    ggnewscale::new_scale_colour()

  if (!no_labels){
    plot_simplenode <- createplot_labels(plot_simplenode, highlight_selection)
  }
  return(plot_simplenode)
}




#' create pie chart node plot MRCO
#' @description create MRCO plot with pie chart nodes (multiple colours per node).
#' @param metadata_column_name character or tidy-selection style unquoted name of a metadata column to plot piechart nodes from
#' @param graph_layout tibble, used graph_layout to plot and to be summarized
#' @param graph_arc tibble, used graph_arc to plot piecharts
#' @param edge_num_size_filter numeric, give the number of cells that must be surpassed to draw edge, has to be set for each data set as its highly dependent on nr of cells within data
#' @param edge_prop_size_filter numeric, give the proportion of cells that must move between nodes to draw edge
#' @param highlight_selection logical, TRUE to highlight selected nodes
#' @param plot_col_gradient NULL for auto detect, or logical, TRUE to use continuous fill, FALSE for discrete fill; only applies when continuous metadata column is selected
#' @param no_labels logical, TRUE to turn off cluster labels, FALSE default for labeled clusters
#' @import ggplot2 ggnewscale dplyr tidygraph ggraph rlang

createplot_piechart <- function(metadata_column_name,
                                graph_layout,
                                graph_arc,
                                edge_num_size_filter,
                                edge_prop_size_filter,
                                highlight_selection = TRUE,
                                plot_col_gradient,
                                no_labels) {
  plot_piechart <- ggraph(graph_layout) +
    geom_edge_diagonal(
      aes(
        edge_alpha = .data$e_prop_size,
        filter =
          .data$e_prop_size >= edge_prop_size_filter &
            .data$e_size >= edge_num_size_filter
      )
    ) +
    scale_edge_alpha(name = "Edge Size proportion")

  if (plot_col_gradient) {
    # use metadata variable as is (continuous or character)
    plot_piechart <- plot_piechart +
      geom_node_arc_bar(
        aes(
          x0 = .data$x,
          y0 = .data$y,
          r0 = 0,
          r = if_else(.data$is_selected,
                      if_else(highlight_selection, .5, .4), .4),
          start = .data$start * 2 * pi,
          end = ((.data$end) * 2) * pi,
          fill = .data[[!!metadata_column_name]],
          alpha = .data$is_selected
        ),
        data = graph_arc
      )
  } else {
    # use as.character for metadata to transform colour mapping
    plot_piechart <- plot_piechart +
      geom_node_arc_bar(
        aes(
          x0 = .data$x,
          y0 = .data$y,
          r0 = 0,
          r = if_else(.data$is_selected,
                      if_else(highlight_selection, .5, .4), .4),
          start = .data$start * 2 * pi,
          end = ((.data$end) * 2) * pi,
          fill = as.character(.data[[!!metadata_column_name]]),
          alpha = .data$is_selected
        ),
        data = graph_arc
      )
  }

  if (plot_col_gradient) {
    plot_piechart <- plot_piechart +
      scale_fill_viridis(
        name = rlang::as_name(metadata_column_name),
        labels = graph_arc$metadata_labels %>%
          base::as.character() %>%
          unique(),
        breaks = dplyr::pull(graph_arc, !!metadata_column_name) %>%
          unique(),
        discrete = FALSE
      )
  } else {
    # incase metadata is not numeric, scale colour discrete
    plot_piechart <- plot_piechart +
      scale_fill_viridis(
        name = rlang::as_name(metadata_column_name),
        labels = graph_arc$metadata_labels %>%
          base::as.character() %>%
          unique(),
        breaks = dplyr::pull(graph_arc, !!metadata_column_name) %>%
          unique(),
        discrete = TRUE
      )
  }

  if (highlight_selection &
    any(graph_layout$is_selected == TRUE)) {
    # highlight selected nodes
    plot_piechart <- plot_piechart +
      scale_alpha_manual(
        name = "Is Selected",
        values = c("TRUE" = 1, "FALSE" = .25),
        guide = "none"
      )
  } else {
    # don't highlight selected nodes
    plot_piechart <- plot_piechart +
      scale_alpha_manual(
        name = "Is Selected",
        values = c("TRUE" = 1, "FALSE" = 1),
        guide = "none"
      )
  }

  plot_piechart <- plot_piechart +
    scale_y_continuous(
      breaks = unique(graph_layout$y),
      labels = unique(as.character(graph_layout$resolution))
    ) +
    coord_fixed()

  if (!no_labels){
    plot_piechart <- createplot_labels(plot_piechart, highlight_selection)
  }

  return(plot_piechart)
}






#' create plot labels MRCO
#' @description create MRCO plot labels
#' @param plot_MRCO ggplot, MRCO plot without labels
#' @param highlight_selection logical, TRUE to highlight selected nodes
#' @import ggplot2 ggnewscale dplyr tidygraph ggraph rlang

createplot_labels <- function(plot_MRCO = NULL,
                              highlight_selection = NULL) {
  # selected_font <- "sans"
    # plot with labels
    plot_MRCO +
      geom_node_text(
        aes(
          label = .data$cluster,
          fontface = "bold",
          colour = if_else(.data$is_selected,
                           if_else(highlight_selection, "TRUE", "TRUE"),
                           if_else(highlight_selection, "FALSE", "TRUE")
          )
        ),
        position = "identity",
        size = 13 / .pt,
        # family = selected_font
        fontface = "bold"

      ) +
      scale_color_manual(
        name = "Is Selected",
        values = c(
          "TRUE" = alpha("black", .95),
          "FALSE" = alpha("black", .5)
        ),
        guide = "none"
      )+
    ggnewscale::new_scale_colour()+
      geom_node_text(
        aes(
          label = .data$cluster,
          colour = if_else(.data$is_selected,
                           if_else(highlight_selection, "TRUE", "TRUE"),
                           if_else(highlight_selection, "FALSE", "TRUE")
          )
        ),
        # family = selected_font,
        fontface = "plain",
        position = "identity"
      ) +
      scale_color_manual(
        name = "Is Selected",
        values = c(
          "TRUE" = alpha("white", 1),
          "FALSE" = alpha("white", .5)
        ),
        guide = "none"
      )

}
