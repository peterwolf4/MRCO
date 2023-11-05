#' Plot Graph Structure MRCO
#' @description uses MRCO internal arguments to plot tree like layouts
#' @param metadata_column_name character or tidy-selection style unquoted name of a metadata column to plot piechart nodes from
#' @param graph_layout tibble, used graph_layout to plot and to be summarized
#' @param graph_arc tibble, used graph_arc to plot piecharts
#' @param edge_num_size_filter numeric, give the number of cells that must be surpassed to draw edge, has to be set for each data set as its highly dependent on nr of cells within data
#' @param edge_prop_size_filter numeric, give the proportion of cells that must move between nodes to draw edge
#' @param highlight_selection logical, TRUE to highlight selected nodes
#' @param no_labels logical, TRUE to turn off cluster labels, FALSE default for labeled clusters
#' @returns ggplot MRCO final plot
#' @import ggplot2 ggnewscale dplyr tidygraph ggraph
#' @importFrom rlang quo_is_null


plot_graph_MRCO <- function(metadata_column_name,
                            graph_layout,
                            graph_arc,
                            edge_num_size_filter,
                            edge_prop_size_filter,
                            highlight_selection = TRUE,
                            no_labels) {
  # create plot base
  plot_MRCO <- ggraph(graph_layout) +
    geom_edge_diagonal(
      aes(
        edge_alpha = .data$e_prop_size,
        filter =
          .data$e_prop_size >= edge_prop_size_filter &
            .data$e_size >= edge_num_size_filter
      )
    ) +
    scale_edge_alpha(name = "Edge Size prop")

  # check which plot to create
  if (quo_is_null(metadata_column_name)) {
    plot_MRCO <- createplot_simplenode(
      plot_MRCO = plot_MRCO,
      graph_layout = graph_layout,
      highlight_selection = highlight_selection,
      no_labels = no_labels
    )
  } else {
    plot_MRCO <- createplot_piechart(
      plot_MRCO = plot_MRCO,
      metadata_column_name = metadata_column_name,
      graph_layout = graph_layout,
      graph_arc = graph_arc,
      highlight_selection = highlight_selection,
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
#' @param plot_MRCO ggplot object, used to gradually build up final MRCO plot
#' @param graph_layout tibble, used graph_layout to plot and to be summarized
#' @param highlight_selection logical, TRUE to highlight selected nodes
#' @param no_labels logical, TRUE to turn off cluster labels, FALSE default for labeled clusters
#' @returns ggplot simplenode plot
#' @import ggplot2 ggnewscale dplyr tidygraph ggraph

createplot_simplenode <- function(plot_MRCO,
                                  graph_layout,
                                  highlight_selection = TRUE,
                                  no_labels) {
  # Add nodes
  plot_simplenode <- createplot_point_nodes(
    plot_MRCO = plot_MRCO,
    graph_layout = graph_layout,
    highlight_selection = highlight_selection
  )

  # Add colour
  plot_simplenode <- plot_simplenode +
    scale_color_viridis_c(name = "Node Size prop", begin = 0.5) +
    coord_fixed() +
    scale_y_continuous(
      breaks = unique(graph_layout$y),
      labels = unique(as.character(graph_layout$resolution))
    ) +
    ggnewscale::new_scale_colour()

  if (!no_labels) {
    plot_simplenode <- createplot_labels(plot_simplenode, highlight_selection)
  }
  return(plot_simplenode)
}




#' create pie chart node plot MRCO
#' @description create MRCO plot with pie chart nodes (multiple colours per node).
#' @param plot_MRCO ggplot, MRCO plot with only edges
#' @param metadata_column_name character or tidy-selection style unquoted name of a metadata column to plot piechart nodes from
#' @param graph_layout tibble, used graph_layout to plot and to be summarized
#' @param graph_arc tibble, used graph_arc to plot piecharts
#' @param highlight_selection logical, TRUE to highlight selected nodes
#' @param no_labels logical, TRUE to turn off cluster labels, FALSE default for labeled clusters
#' @returns ggplot MRCO piechart plot
#' @import ggplot2 ggnewscale dplyr tidygraph ggraph rlang

createplot_piechart <- function(plot_MRCO,
                                metadata_column_name,
                                graph_layout,
                                graph_arc,
                                highlight_selection,
                                no_labels) {
  # Add nodes
  plot_piechart <- createplot_piechart_nodes(
    plot_MRCO = plot_MRCO,
    metadata_column_name = metadata_column_name,
    graph_layout = graph_layout,
    graph_arc = graph_arc,
    highlight_selection = highlight_selection
  )

  # Manage Highlighting
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

  # Add Labels
  if (!no_labels) {
    plot_piechart <- createplot_labels(plot_piechart, highlight_selection)
  }

  return(plot_piechart)
}






#' create plot labels MRCO
#' @description create MRCO plot labels
#' @param plot_MRCO ggplot, MRCO plot without labels
#' @param highlight_selection logical, TRUE to highlight selected nodes
#' @returns ggplot MRCO plot with labels
#' @import ggplot2 ggnewscale dplyr tidygraph ggraph rlang

createplot_labels <- function(plot_MRCO = NULL,
                              highlight_selection = NULL) {
  # plot with labels
  plot_MRCO +
    geom_node_text(
      aes(
        label = .data$cluster,
        colour = if_else(.data$is_selected,
          if_else(highlight_selection, "TRUE", "TRUE"),
          if_else(highlight_selection, "FALSE", "TRUE")
        )
      ),
      position = "identity",
      size = 13 / .pt,
      fontface = "bold"
    ) +
    scale_color_manual(
      name = "Is Selected",
      values = c(
        "TRUE" = alpha("black", .95),
        "FALSE" = alpha("black", .5)
      ),
      guide = "none"
    ) +
    ggnewscale::new_scale_colour() +
    geom_node_text(
      aes(
        label = .data$cluster,
        colour = if_else(.data$is_selected,
          if_else(highlight_selection, "TRUE", "TRUE"),
          if_else(highlight_selection, "FALSE", "TRUE")
        )
      ),
      fontface = "plain"
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



#' create simple nodes MRCO
#' @description add nodes to existing edge graph for simplenode plot
#' @param plot_MRCO ggplot, MRCO plot with only edges
#' @param graph_layout tibble, used graph_layout to plot and to be summarized
#' @param highlight_selection logical, TRUE to highlight selected nodes
#' @returns ggplot MRCO plot with point nodes
#' @import ggplot2 ggnewscale dplyr tidygraph ggraph rlang

createplot_point_nodes <- function(plot_MRCO,
                                   graph_layout,
                                   highlight_selection) {
  if (highlight_selection &
    any(graph_layout$is_selected == TRUE)) {
    plot_MRCO +
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
    plot_MRCO +
      geom_node_point(aes(colour = .data$n_psize),
        size = 8
      )
  }
}



#' create piechart nodes MRCO
#' @description add nodes to existing edge graph for piechart plot
#' @param plot_MRCO ggplot, MRCO plot without nodes
#' @param metadata_column_name character or tidy-selection style unquoted name of a metadata column to plot piechart nodes from
#' @param graph_layout tibble, used graph_layout to plot and to be summarized
#' @param graph_arc tibble, used graph_arc to plot piecharts
#' @param highlight_selection logical, TRUE to highlight selected nodes
#' @returns ggplot MRCO plot with piechart nodes
#' @import ggplot2 ggnewscale dplyr tidygraph ggraph rlang

createplot_piechart_nodes <- function(plot_MRCO,
                                      metadata_column_name,
                                      graph_arc,
                                      graph_layout,
                                      highlight_selection) {
  plot_MRCO +
    geom_node_arc_bar(
      aes(
        x0 = .data$x,
        y0 = .data$y,
        r0 = 0,
        r = if_else(.data$is_selected,
          if_else(highlight_selection, .5, .4), .4
        ),
        start = .data$start * 2 * pi,
        end = ((.data$end) * 2) * pi,
        fill = .data[[!!metadata_column_name]],
        alpha = .data$is_selected
      ),
      data = graph_arc
    ) +
    scale_y_continuous(
      breaks = unique(graph_layout$y),
      labels = unique(as.character(graph_layout$resolution))
    ) +
    coord_fixed() +
    scale_fill_viridis(
      name = rlang::as_name(metadata_column_name),
      labels = graph_arc$metadata_labels %>%
        base::as.character() %>%
        unique(),
      breaks = dplyr::pull(graph_arc, !!metadata_column_name) %>%
        unique(),
      discrete = ifelse(graph_arc %>%
        pull(!!metadata_column_name) %>%
        is.numeric(),
      FALSE,
      TRUE
      )
    )
}
