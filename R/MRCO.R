#' Multi Resolution Cluster Optimization
#' @author Peter Wolf
#' @description MRCO visualizes clustering results of different resolutions in a treelike graph to directly show differences in sample compositions and give an intuitive solution to choosing the best amount of clusters for your experiment.
#' MRCO uses multiple initial clusterings of any algorithm which were individually created iterating though some form of a resolution tuning parameter.
#' Next to the clustering resolutions, metadata may also contain columns regarding other sample level information that might help evaluation on whether or not sub-clustering of a certain cluster is necessary or not.
#' Additionally to the build in automated stable cluster suggestion one can choose to select clusters manually to easily receive a custom clustering for downstream analysis.
#' @param metadata data.frame or tibble with dim-names: row names cell identities and column names cell level metadata variables
#' @param metadata_column_name character or tidy-selection style unquoted name of a metadata column to plot piechart nodes from
#' @param prefix character, prefix of metadata columns which contain the increments of resolution
#' @param suffix character, suffix of metadata columns which contain the increments of resolution
#' @param metadata_column_nbins NULL, "all" or a numeric, set the number of bins to create from given metadata column if it is continuous data
#' @param plot_col_gradient NULL for auto detect, or logical, TRUE to use continuous fill, FALSE for discrete fill; only applies when continuous metadata column is selected

#' @param suggest_cut logical, TRUE to suggest stable nodes based on graph structure, FALSE to skip
#' @param reduce_branchlist logical, TRUE to walk as few paths as possible while still visiting every node, FALSE to walk any possible path passing the edge_filters; warning memory expensive, adjust branches_overflow parameter! Only relevant if suggest_cut is TRUE.
#' @param edge_num_size_filter numeric, give the number of cells that must be surpassed to draw edge, has to be set for each data set as its highly dependent on nr of cells within data
#' @param edge_prop_size_filter numeric, give the proportion of cells that must move between nodes to draw edge
#' @param branches_overflow numeric giving the max number of paths through the graph that is checked for each resolution step to protect memory overflow

#' @param nodes_selection vector or list, two ways to manually select a node by its global id from the graph:
#' either give resolution step and cluster id from that resolution as underscore separated character string,
#'  or give a list named where each name corresponds to a resolution step and its element is a single vector containing the cluster ids that are to be selected of that resolution.
#' @param merge_downwards logical, cells which are in multiple selected clusters can either be assorted into the first or last cluster available for them.
#' FALSE means that clusters are merged from the bottom towards the top of the graph, hence cells which are part of a higher resolution cluster will remain in it.
#' TRUE means that clusters are merged from top towards the bottom, hence cells will end up in the lowest resolution cluster that they may end in.

#' @param silent logical, TRUE to suppress messages
#' @param warnings logical, FALSE to suppress warnings
#' @param plot logical, FALSE to skip plotting
#' @param no_labels logical, TRUE to turn off cluster labels, FALSE default for labeled clusters
#' @param highlight_selection logical, TRUE to highlight selected nodes
#' @param igraph_layout_type character giving the igraph layout type for graph creation; either "tree" or "sugiyama"
#' @param edge_ratio_weigth numeric between 0 and 1, when stable edges are determined their edge ratio must be larger than the branch paths maximum edge ratio timed edge_ratio_weight.
#' Therefore, a value closer to 1 is less permissive towards noise, whereas a value closer to 0 may handle noisier graphs better.

#' @import dplyr tidyr tibble stringr tidygraph ggraph ggplot2 rlang ggnewscale

#' @export

MRCO <- function(metadata = NULL,
                 metadata_column_name = NULL,
                 prefix = NULL, suffix = NULL,
                 metadata_column_nbins = NULL,
                 plot_col_gradient = NULL,

                 suggest_cut = FALSE,
                 reduce_branchlist = TRUE,
                 edge_num_size_filter = 0,
                 edge_prop_size_filter = 0.05,
                 branches_overflow = 50000,

                 nodes_selection = NULL,
                 merge_downwards = FALSE,

                 plot = TRUE,
                 no_labels = FALSE,
                 highlight_selection = TRUE,
                 silent = FALSE,
                 warnings = TRUE,

                #Advanced user options/tuning
                igraph_layout_type = "tree", #tree or sugiyama ~soft deprecated
                edge_ratio_weigth = .9
                ){

  ###Check Function Input-----

  #initiate NGCS return value list
  return_list <- list()
  #capture user given columns
  {
    metadata_column_name  <- rlang::enexpr(metadata_column_name)
    #coerce columns names from character to symbol
    if (!is.null(metadata_column_name)){
      if (class(metadata_column_name) == "character"){
        metadata_column_name <- sym(metadata_column_name)
      }
    }
  }


  input_list <- input_checker_MRCO(
    metadata = metadata,
    metadata_column_name = !!metadata_column_name,
    prefix = prefix, suffix = suffix,
    metadata_column_nbins = metadata_column_nbins,
    plot_col_gradient = plot_col_gradient,

    suggest_cut = suggest_cut,
    reduce_branchlist = reduce_branchlist,
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
  metadata_column_nbins <- input_list$metadata_column_nbins
  rm(input_list)



  ###Build Graph-----

  graph_list <- build_graph_MRCO(
    cm = cm,
    cells_selected = NULL,
    metadata = metadata,
    metadata_column_name = !!metadata_column_name,
    metadata_column_nbins = metadata_column_nbins,
    silent = silent,
    warnings = warnings,
    igraph_layout_type = igraph_layout_type
    )

  graph_layout <- graph_list$graph_layout
  edges <- graph_list$edges
  graph_arc <- graph_list$graph_arc
  nodes_cell <- graph_list$nodes_cell
  rm(graph_list)


  ###Suggest Stable Clusters -----

  if (suggest_cut){

    #Create Branchlist
    branch_df <- create_node_branch_list_bf_MRCO(graph_layout,
                                            edges,
                                            reduce = reduce_branchlist,
                                            edge_num_size_filter = edge_num_size_filter,
                                            edge_prop_size_filter = edge_prop_size_filter,
                                            debugg = F, #deprecated argument
                                            silent = silent,
                                            branches_overflow = branches_overflow)
    #Suggest stable clusters
    cluster_stability <- cluster_stability_estimator_MRCO(graph_layout = graph_layout,
                                                     edges = edges,
                                                     branch_df = branch_df,
                                                     plot = plot,
                                                     edge_ratio_weigth = edge_ratio_weigth
                                                     )


  if (!is.null(nodes_selection)){
    #If some nodes were already selected by the user: merge with auto suggestion
    nodes_selection <- c(nodes_selection, cluster_stability$estimated_stable)
  }

  }#end of suggest = T



  ###Select Custom Clusters-----

  #automatically set selected nodes if none were given and suggest cut ran through
  if (is.null(nodes_selection) & suggest_cut){
    nodes_selection <- cluster_stability$estimated_stable
  }

  ## cells selected? add in column regarding activity of selection
  if (!is.null(nodes_selection)){
    nodes_selected <- custom_node_selection(nodes_cell = nodes_cell,
                                            cm = cm,
                                            nodes_selection = nodes_selection,
                                            merge_downwards = merge_downwards)
    return_list$nodes_selected <- nodes_selected
  }


  #if cell selection is not NULL: rerun graph creation for plotting with cell highlighting
  graph_layout$"is_selected" <- if_else(graph_layout$id %in% nodes_selection,
                                        TRUE,FALSE)
  if (!is.null(metadata_column_name)){
    graph_arc$"is_selected" <- if_else(graph_arc$id %in% nodes_selection,
                                          TRUE,FALSE)
  }

  return_list$graph_layout <- graph_layout

  ###Plot Graphs-----


  if (plot == T){
    plot_list <- plot_graph_NGCS(metadata_column_name = metadata_column_name,
                  graph_layout = graph_layout,
                  graph_arc = graph_arc,
                  edge_num_size_filter = edge_num_size_filter,
                  edge_prop_size_filter = edge_prop_size_filter,
                  highlight_selection = highlight_selection,
                  plot_col_gradient = plot_col_gradient,
                  no_labels = no_labels)

    plot_list$plot_node_tree
  return_list$plot_node_tree <- plot_list$plot_node_tree
 }


  return(invisible(return_list))
}







#' Custom Node  Selection wNGCS
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

custom_node_selection <- function(nodes_cell, nodes_selection,
                                  cm,merge_downwards,
                                  silent = T){


  if (!all(nodes_selection %in% nodes_cell$id)) { stop("Given vector of nodes selection is not fully matching to available nodes.
                                                         Graph Node-Ids are generated by resolution-step and cluster name seperated by underscore eg. resolution_cluster.
                                                         Did you forget to seperate resolution-step from selected cluster with _ ?")}


  #Clean up input
  nodes_selection <- nodes_selection %>%
    unique() %>%
    sort()

  #filter from long tibble of all possible cell stats (nodes_cell)
  # which ids are select as stable
  # either take largest or lowest resolution from a cell with multiple id choices
  if (merge_downwards){
    cells_selection <- nodes_cell %>%
      filter(id %in% nodes_selection) %>%
      group_by(cell) %>%
      slice_min(order_by = resolution,
                n = 1, with_ties = FALSE)
  } else {
    cells_selection <- nodes_cell %>%
      filter(id %in% nodes_selection) %>%
      group_by(cell) %>%
      slice_max(order_by = resolution,
                n = 1, with_ties = FALSE)
  }

  if (nrow(cells_selection) < nrow(cm)) {
    if (!silent) message("Some cells have not been selected,
            they are now annotated as cluster \'debris\'.")
  } else if (nrow(cells_selection) > nrow(cm)){
      warning("Exceptional Warning: More cells selected than present in initial metadata, please report this bug!
              Do not use the resulting clustering for analysis as it likely contains multiple assignments for the same cells.")
  }

  #assign debris cells for remaining non selected cells
  cells_assignment <- data.frame("cell" = rownames(cm)) %>%
    left_join(cells_selection %>% select("cell","id"), by = "cell") %>%
    mutate("id" = if_else(is.na(.data$id), "debris", .data$id))
  rownames(cells_assignment) <- cells_assignment$cell

  if (!all(rownames(cm) == rownames(cells_assignment))){
    warning("Exceptional Warning: Rownames do not match input metadata, please report this bug and do not use results for your analysis!")
  }

  return_vals <- list("nodes_selection" = nodes_selection,
                      "cells_NonGlobalClustering" = cells_assignment)

  return(return_vals)
}










