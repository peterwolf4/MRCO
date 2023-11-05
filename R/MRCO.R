#' Multi Resolution Cluster Optimization
#' @description
#' MRCO visualizes the results of a clustering algorithm across different
#'  resolutions (sensitivities) in a treelike graph.
#'  This graph can display differences in sample compositions and give an
#'  intuitive solution to choosing the best amount of clusters for your
#'  experiment. \cr
#' Simply run your clustering algorithm of choice multiple
#'  times across a sensitivity range of interest. MRCO takes these clustering
#'  results as input, compares them, finds stable clusters that represent
#'  the data across your range of resolution, and returns the new clustering.\cr
#' MRCO can also plot clusters as pie chart nodes to visualize the composition.
#'  This can help distinguish which metadata variable correlates with
#'  cluster separation across increasing sensitivity.
#'
#' @param metadata data.frame or tibble: with row names (cell ids) and
#'  column names (cell metadata variables)
#' @param metadata_column_name character or unquoted name: name the metadata
#'  column to plot piechart nodes from
#' @param clustering_columns character prefix or tidy-select: to distinguish
#' clustering resolution columns from metadata columns.
#' @param nbins numeric, set the number of bins to create from given metadata
#'  column if it is numeric data
#' @param plot_col_gradient logical, TRUE to use continuous fill, FALSE for
#'  discrete fill; only applies when numeric metadata column is selected
#' @param suggest_cut logical, TRUE to suggest stable nodes based on graph
#'  structure, FALSE to skip automated selection
#' @param edge_num_size_filter numeric, give the minimum number of samples
#'  required to draw an edge
#' @param edge_prop_size_filter numeric, give the minimum edge ratio, which is
#'  displaying the proportion of samples that move between nodes to draw an edge

#' @param nodes_selection character vector or list, two ways to manually select
#'  a node by its global id from the graph
#' @param merge_downwards logical, cells which are in multiple selected clusters
#'  can either be assorted into the first or last cluster available for them.
#' FALSE means that clusters are merged from the bottom towards the top of
#'  the graph, hence cells which are part of a higher resolution cluster will
#'   remain in it.
#' TRUE means that clusters are merged from top towards the bottom, hence cells
#'  will end up in the lowest resolution cluster that they may end in.
#' @param highlight_selection logical, TRUE to highlight selected nodes
#' @param no_labels logical, TRUE to turn off cluster labels

#' @param silent logical, TRUE to suppress messages
#' @param warnings logical, FALSE to suppress warnings
#' @param plot logical, FALSE to skip plotting


#' @param igraph_layout_type character giving the igraph layout type for graph creation; either "tree" or "sugiyama"
#' @param edge_ratio_weigth numeric between 0 and 1, when stable edges are determined their edge ratio must be larger than the branch paths maximum edge ratio timed edge_ratio_weight.
#' Therefore, a value closer to 1 is less permissive towards noise, whereas a value closer to 0 may handle noisier graphs better.
#' @returns list, contains the graph layout. May contain selected nodes, the resulting clustering and the ggplot object if respective functions are used.
#' @example man/examples/MRCO_example.R
#' @import dplyr tidyr tibble stringr tidygraph ggraph ggplot2 rlang ggnewscale methods
#' @importFrom utils data head
#' @export

MRCO <- function(metadata = NULL,
                 metadata_column_name = NULL,
                 clustering_columns = NULL,
                 nbins = 4,
                 plot_col_gradient = FALSE,
                 suggest_cut = TRUE,
                 edge_num_size_filter = 0,
                 edge_prop_size_filter = 0.05,
                 nodes_selection = NULL,
                 merge_downwards = FALSE,
                 plot = TRUE,
                 no_labels = FALSE,
                 highlight_selection = TRUE,
                 silent = FALSE,
                 warnings = TRUE,
                 # Advanced user options/tuning
                 igraph_layout_type = "tree", # tree or sugiyama ~soft deprecated
                 edge_ratio_weigth = .9) {
  ### Check Function Input-----

  # initiate MRCO return value list
  return_list <- list()
  # capture user given columns
  metadata_column_name <- enquo(metadata_column_name)
  clustering_columns <- enquo(clustering_columns)

  input_list <- input_checker_MRCO(
    metadata = metadata,
    metadata_column_name = metadata_column_name,
    clustering_columns = clustering_columns,
    nbins = nbins,
    plot_col_gradient = plot_col_gradient,
    suggest_cut = suggest_cut,
    edge_num_size_filter = edge_num_size_filter,
    edge_prop_size_filter = edge_prop_size_filter,
    nodes_selection = nodes_selection,
    merge_downwards = merge_downwards,
    #
    plot = plot,
    no_labels = no_labels,
    silent = silent,
    warnings = warnings,
    highlight_selection = highlight_selection
  )

  metadata <- input_list$metadata
  cm <- input_list$cm
  nodes_selection <- input_list$nodes_selection
  plot_col_gradient <- input_list$plot_col_gradient
  nbins <- input_list$nbins
  rm(input_list)



  ### Build Graph-----

  graph_list <- build_graph_MRCO(
    cm = cm,
    cells_selected = NULL,
    metadata = metadata,
    metadata_column_name = metadata_column_name,
    nbins = nbins,
    silent = silent,
    warnings = warnings,
    igraph_layout_type = igraph_layout_type
  )

  graph_layout <- graph_list$graph_layout
  edges <- graph_list$edges
  graph_arc <- graph_list$graph_arc
  nodes_cell <- graph_list$nodes_cell
  rm(graph_list)


  ### Suggest Stable Clusters -----

  if (suggest_cut) {
    # Create Branchlist
    branch_df <- create_node_branch_list_MRCO(
      graph_layout,
      edges
    )
    # Suggest stable clusters
    cluster_stability <- cluster_stability_estimator_MRCO(
      graph_layout = graph_layout,
      edges = edges,
      branch_df = branch_df,
      plot = plot,
      edge_ratio_weigth = edge_ratio_weigth
    )


    if (!is.null(nodes_selection)) {
      # If some nodes were already selected by the user: merge with auto suggestion
      nodes_selection <- c(nodes_selection, cluster_stability$estimated_stable)
    }
  } # end of suggest = T



  ### Select Custom Clusters-----

  # automatically set selected nodes if none were given and suggest cut ran through
  if (is.null(nodes_selection) & suggest_cut) {
    nodes_selection <- cluster_stability$estimated_stable
  }

  ## cells selected? add in column regarding activity of selection
  if (!is.null(nodes_selection)) {
    nodes_selected <- custom_node_selection(
      nodes_cell = nodes_cell,
      cm = cm,
      nodes_selection = nodes_selection,
      merge_downwards = merge_downwards
    )
    return_list$selected_nodes <- nodes_selected$selected_nodes
    return_list$MRCO_clustering <- nodes_selected$MRCO_clustering
  }


  # if cell selection is not NULL: rerun graph creation for plotting with cell highlighting
  graph_layout$"is_selected" <- if_else(graph_layout$id %in% nodes_selection,
    TRUE, FALSE
  )
  if (!quo_is_null(metadata_column_name)) {
    graph_arc$"is_selected" <- if_else(graph_arc$id %in% nodes_selection,
      TRUE, FALSE
    )
  }

  return_list$graph_layout <- graph_layout

  ### Plot Graphs-----


  if (plot == TRUE) {
    return_list$plot <- plot_graph_MRCO(
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

  return(invisible(return_list))
}







#' Custom Node  Selection MRCO
#' @param nodes_cell tibble of unique nodes, meaning for each resolution each cluster
#' @param cm cluster matrix to match outputs row order
#' @param nodes_selection vector or list, two ways to manually select a node by its global id from the graph:
#' either give resolution step and cluster id from that resolution as underscore separated character string,
#'  or give a list named where each name corresponds to a resolution step and its element is a single vector containing the cluster ids that are to be selected of that resolution.
#' @param merge_downwards logical, cells which are in multiple selected clusters can either be assorted into the first or last cluster available for them.
#' FALSE means that clusters are merged from the bottom towards the top of the graph, hence cells which are part of a higher resolution cluster will remain in it.
#' TRUE means that clusters are merged from top towards the bottom, hence cells will end up in the lowest resolution cluster that they may end in.
#' @param silent logical, FALSE to suppress messages and warnings
#' @import tibble rlang dplyr
#' @importFrom stats setNames

custom_node_selection <- function(
    nodes_cell, nodes_selection,
    cm, merge_downwards,
    silent = TRUE) {
  if (!all(nodes_selection %in% nodes_cell$id)) {
    stop("Given vector of nodes selection is not fully matching to available nodes.
                                                         Graph Node-Ids are generated by resolution-step and cluster name seperated by underscore eg. resolution_cluster.
                                                         Did you forget to seperate resolution-step from selected cluster with _ ?")
  }


  # Clean up input
  nodes_selection <- nodes_selection %>%
    unique() %>%
    sort()

  # filter from long tibble of all possible cell stats (nodes_cell)
  # which ids are select as stable
  # either take largest or lowest resolution from a cell with multiple id choices
  if (merge_downwards) {
    cells_selection <- nodes_cell %>%
      filter(id %in% nodes_selection) %>%
      group_by(.data$cell) %>%
      slice_min(
        order_by = resolution,
        n = 1, with_ties = FALSE
      )
  } else {
    cells_selection <- nodes_cell %>%
      filter(id %in% nodes_selection) %>%
      group_by(.data$cell) %>%
      slice_max(
        order_by = resolution,
        n = 1, with_ties = FALSE
      )
  }

  if (nrow(cells_selection) < nrow(cm)) {
    if (!silent) message("Some cells have not been selected,
            they are now annotated as cluster \'debris\'.")
  } else if (nrow(cells_selection) > nrow(cm)) {
    warning("Exceptional Warning: More cells selected than present in initial metadata, please report this bug!
              Do not use the resulting clustering for analysis as it likely contains multiple assignments for the same cells.")
  }

  # assign debris cells for remaining non selected cells
  cells_assignment <- data.frame("cell" = rownames(cm)) %>%
    left_join(cells_selection %>% select("cell", "id"), by = "cell") %>%
    mutate("id" = if_else(is.na(.data$id), "debris", .data$id))
  rownames(cells_assignment) <- cells_assignment$cell

  if (!all(rownames(cm) == rownames(cells_assignment))) {
    warning("Exceptional Warning: Rownames do not match input metadata, please report this bug and do not use results for your analysis!")
  }

  return_vals <- list(
    "selected_nodes" = nodes_selection,
    "MRCO_clustering" = cells_assignment
  )

  return(return_vals)
}
