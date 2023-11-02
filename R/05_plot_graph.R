#' Plot Graph Structure NGCS
#' @description uses NGCS internal arguments to plot tree like layouts
#' @param metadata_column_name character or tidy-selection style unquoted name of a metadata column to plot piechart nodes from
#' @param graph_layout tibble, used graph_layout to plot and to be summarized
#' @param graph_arc tibble, used graph_arc to plot piecharts
#' @param edge_num_size_filter numeric, give the number of cells that must be surpassed to draw edge, has to be set for each data set as its highly dependent on nr of cells within data
#' @param edge_prop_size_filter numeric, give the proportion of cells that must move between nodes to draw edge
#' @param highlight_selection logical, TRUE to highlight selected nodes
#' @param plot_col_gradient NULL for auto detect, or logical, TRUE to use continuous fill, FALSE for discrete fill; only applies when continuous metadata column is selected
#' @param no_labels logical, TRUE to turn off cluster labels, FALSE default for labeled clusters
#' @import ggplot2 ggnewscale dplyr tidygraph ggraph


plot_graph_NGCS <- function(
    metadata_column_name,
    graph_layout,
    graph_arc,
    edge_num_size_filter,
    edge_prop_size_filter,
    highlight_selection = TRUE,
    plot_col_gradient,
    no_labels

  ){
  return_list <- list()


  #create aesthetics for plotting
  aes_list <- list()
  aes_list$theme <- theme(plot.background = element_rect(fill = "white"),
                          axis.line.y = element_line(size = 0.2,
                                                     colour = alpha("black",0),
                                                     linetype = 1),
                          axis.text.y = element_text(size = 8),
                          axis.ticks.y = element_line(size = .1,linetype = 1),
                          axis.ticks.length.y = unit(1, "mm"),
                          panel.grid.major.y = element_line(color = alpha("lightblue",0.35)),
                          panel.background = element_blank())




  if (is.null(metadata_column_name)) {
    #plotting simple graph--
    plot_simplenode <-
      ggraph(graph_layout)+
      geom_edge_diagonal(aes(edge_alpha = .data$e_prop_size,
                             filter = .data$e_prop_size >= edge_prop_size_filter &
                               .data$e_size >= edge_num_size_filter)
                         , show.legend = c("alpha" = TRUE, "colour" = FALSE))+
      scale_edge_alpha(name = "Edge Size prop")

    if (highlight_selection & any(graph_layout$is_selected == TRUE) ) {
      plot_simplenode <- plot_simplenode +
      geom_node_point(aes(colour =  .data$n_psize,
                          size = .data$is_selected,
                      alpha = .data$is_selected))+
        scale_size_manual(name = "Is Selected",
                          values = c("TRUE" = 6, "FALSE" = 4),
                          guide = "none")+
        scale_alpha_manual(name = "Is Selected",
                           values = c("TRUE" = 1,"FALSE" = .25),
                           guide = "none")

    } else {
      plot_simplenode <- plot_simplenode +
        geom_node_point(aes(colour =  .data$n_psize)
                        , size = 5
                        )
    }

    if (no_labels) {
      plot_simplenode <- plot_simplenode +
        scale_color_viridis_c(name = "Node Size proportional", begin = 0.5)+
        # ggnewscale::new_scale_colour()+
        # geom_node_text(aes(label = .data$cluster,
        #                    fontface = "bold"),
        #                colour = alpha("black",.75),
        #                position = "identity",
        #                size = 13/.pt)+
        # geom_node_text(aes(label = .data$cluster,
        #                    fontface = "bold",
        #                    colour = is_selected),
        #                position = "identity")+
        # scale_color_manual(name = "Is Selected",
        #                    values = c("TRUE" = "white",
        #                               "FALSE" = alpha("white",.75)),
        #                    guide = "none")+
        coord_fixed()+
        scale_y_continuous(breaks = unique(graph_layout$y),
                           labels = unique(as.character(graph_layout$resolution)))+
        aes_list$theme
      # scale_size_identity() #avoid custom scaling set by ggplot

    } else {
      plot_simplenode <- plot_simplenode +
        scale_color_viridis_c(name = "Node Size proportional", begin = 0.5)+
        ggnewscale::new_scale_colour()+
        geom_node_text(aes(label = .data$cluster,
                           fontface = "bold"),
                       colour = alpha("black",.75),
                       position = "identity",
                       size = 13/.pt)+
        geom_node_text(aes(label = .data$cluster,
                           fontface = "bold",
                           colour = .data$is_selected),
                       position = "identity")+
        scale_color_manual(name = "Is Selected",
                           values = c("TRUE" = "white",
                                      "FALSE" = alpha("white",.75)),
                           guide = "none")+
        coord_fixed()+
        scale_y_continuous(breaks = unique(graph_layout$y),
                           labels = unique(as.character(graph_layout$resolution)))+
        aes_list$theme
      # scale_size_identity() #avoid custom scaling set by ggplot

    }


      plot(plot_simplenode)
      return_list$plot_node_tree <- plot_simplenode



  } else {
    #plotting piechart node graph--
    plot_piechart <-
      ggraph(graph_layout)+
      geom_edge_diagonal(aes(edge_alpha = .data$e_prop_size,
                             filter =
                               .data$e_prop_size >= edge_prop_size_filter &
                               .data$e_size >= edge_num_size_filter))+
      scale_edge_alpha(name = "Edge Size proportion")



    if (highlight_selection & any(graph_layout$is_selected == TRUE)){
      if (plot_col_gradient){
        #plot value of metadata column as is & highlight selected nodes
        plot_piechart <- plot_piechart +
          geom_node_arc_bar(aes(x0 = .data$x , y0 = .data$y,
                                r0 = 0,
                                r = if_else(.data$is_selected,.5,.3),
                                start = .data$start*2*pi,
                                end = ((.data$end)*2)*pi,
                                fill = {{metadata_column_name}},
                                alpha = .data$is_selected),
                            data = graph_arc)+
          scale_alpha_manual(name = "Is Selected",
                             values = c("TRUE" = 1,"FALSE" = .25),
                             guide = "none")
      } else {
        #plot value of metadata column as character for discrete fill & highlight selected nodes
        plot_piechart <- plot_piechart +
          geom_node_arc_bar(aes(x0 = .data$x , y0 = .data$y,
                                r0 = 0,
                                r = if_else(.data$is_selected,.5,.3),
                                start = .data$start*2*pi,
                                end = ((.data$end)*2)*pi,
                                fill = as.character({{metadata_column_name}}),
                                alpha = .data$is_selected),
                            data = graph_arc)+
          scale_alpha_manual(name = "Is Selected",
                             values = c("TRUE" = 1,"FALSE" = .25),
                             guide = "none")
      }
    } else {
      if (plot_col_gradient){
        #plot value of metadata column as is & don't highlight selected nodes
        plot_piechart <- plot_piechart +
          geom_node_arc_bar(aes(x0 = .data$x , y0 = .data$y,
                                r0 = 0, r = 0.4,
                                start = .data$start*2*pi,
                                end = ((.data$end)*2)*pi,
                                fill = {{metadata_column_name}}),
                            size = 0, data = graph_arc)


      } else {
        #plot value of metadata column as character for discrete fill & don't highlight selected nodes
        plot_piechart <- plot_piechart +
          geom_node_arc_bar(aes(x0 = .data$x , y0 = .data$y,
                                r0 = 0, r = 0.4,
                                start = .data$start*2*pi,
                                end = ((.data$end)*2)*pi,
                                fill = as.character({{metadata_column_name}})),
                            size = 0, data = graph_arc)
      }

    }

    #incase numerical values are used, scale fill continuouse
    if (plot_col_gradient){
      plot_piechart <- plot_piechart+
      scale_fill_viridis(name = rlang::as_string({{metadata_column_name}}),
                         labels = dplyr::pull(graph_arc, .data$metadata_labels) %>% base::as.character(),
                         breaks = dplyr::pull(graph_arc, !!metadata_column_name),
                         discrete = FALSE)
    } else {
      plot_piechart <- plot_piechart+
      scale_fill_viridis(name = rlang::as_string({{metadata_column_name}}),
                         labels = dplyr::pull(graph_arc, .data$metadata_labels) %>% base::as.character(),
                         breaks = dplyr::pull(graph_arc, !!metadata_column_name),
                         discrete = TRUE)
    }

    if (no_labels){
      #plot without cluster labels
      plot_piechart <- plot_piechart +
        scale_y_continuous(breaks = unique(graph_layout$y),
                           labels = unique(as.character(graph_layout$resolution)))+
        # ggnewscale::new_scale_colour()+
        # geom_node_text(aes(label = .data$cluster,
        #                    fontface = "bold"),
        #                colour = alpha("black",.75),
        #                position = "identity",
        #                size = 13/.pt)+
        # geom_node_text(aes(label = .data$cluster,
        #                    fontface = "bold",
        #                    colour = is_selected),
        #                position = "identity")+
        # scale_color_manual(name = "Is Selected",
        #                    values = c("TRUE" = "white",
        #                               "FALSE" = alpha("white",.75)),
        #                    guide = "none")+
        coord_fixed()+
        aes_list$theme

    } else {
      #plot with labels
      plot_piechart <- plot_piechart +
        scale_y_continuous(breaks = unique(graph_layout$y),
                           labels = unique(as.character(graph_layout$resolution)))+
        # ggnewscale::new_scale_colour()+
        geom_node_text(aes(label = .data$cluster,
                           fontface = "bold"),
                       colour = alpha("black",.75),
                       position = "identity",
                       size = 13/.pt)+
        geom_node_text(aes(label = .data$cluster,
                           fontface = "bold",
                           colour = .data$is_selected),
                       position = "identity")+
        scale_color_manual(name = "Is Selected",
                           values = c("TRUE" = "white",
                                      "FALSE" = alpha("white",.75)),
                           guide = "none")+
        coord_fixed()+
        aes_list$theme
    }


    plot(plot_piechart)
    return_list$plot_node_tree <- plot_piechart
  }

  return(return_list)

}
