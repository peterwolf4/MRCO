
#' Create Transition Matrix for MRCO
#' @param cm abbreviation for cluster matrix: data.frame, tibble or matrix excerpt from metadata, only containing each resolution step (in order)
#' @param cells_selected contains which cellnames are selected
#' @param metadata data.frame or tibble with dim-names: row names cell identities and column names cell level metadata variables
#' @param metadata_column_name character or tidy-selection style unquoted name of a metadata column to plot piechart nodes from
#' @param metadata_column_nbins NULL, "all" or a numeric, set the number of bins to create from given metadata column if it is continuous data
#' @param silent logical, FALSE to suppress messages and warnings
#' @param igraph_layout_type character giving the igraph layout type for graph creation; either "tree" or "sugiyama"
#'
#' @import tibble rlang dplyr

build_graph_MRCO <- function(cm,
                             cells_selected,
                             metadata,
                             metadata_column_name,
                             metadata_column_nbins,
                             silent,
                             warnings,
                             igraph_layout_type
){

  #prepare variables
  metadata_column_name <- enexpr(metadata_column_name)

  return_list <- list("graph_layout" = NULL,
                      "edges" = NULL,
                      "graph_arc" = NULL)

  #make resolution steps independent of column name, expect equal resolution increments
  colnames(cm) <- as.character(seq_along(colnames(cm)))

  nodes_cell <- cm %>%
    mutate(across(everything(), as.character)) %>%
    rownames_to_column(var = "cell") %>%
    pivot_longer(cols = c(everything(), -"cell"),
                 names_to = "resolution",
                 values_to = "cluster") %>%
    mutate("resolution" = as.numeric(.data$resolution)) %>%
    arrange(.data$resolution) %>%
    mutate("id" = str_c(.data$resolution,.data$cluster,sep="_"))


  ##Count unique nodes
  nodes <- nodes_cell %>% group_by(across(c(everything(),-"cell"))) %>%
    summarise("n_size" = n()) %>% ungroup() %>%
    mutate("n_psize" = .data$n_size/nrow(cm)#, #proportional size of all cells
           # "is_selected" = if_else(id %in% nodes_selected[["nodes_selection"]],T,F)
    )


  ##Create Transition matrix containing all edges
  edges <- create_transtion_matrix_MRCO(cm = cm, nodes = nodes)



  #Transfer Nodes & Edges to a graph object
  graph <- tbl_graph(nodes = nodes, edges = edges, node_key = "id")

  #Create graph from most abundant edges
  graph_layout <- graph %>%
    tidygraph::activate("edges") %>%
    tidygraph::filter(.data$is_core) %>%
    ggraph::create_layout(igraph_layout_type) #minimizes edge crossings: sugiyama

  #add non core edges back in
  attributes(graph_layout)$graph <- graph

  #check if each x position is unique, if not; adjust x position slightly so that group by x handles the right groups
  graph_layout <- check_unique_x_positions_MRCO(graph_layout = graph_layout, silent = silent)

  #after using tidyverse function attributes will be lost,
  #  therefore we have to update them
  #check_unique_x_positions avoids calling tidyverse functions on graph_layout
  #  so we do not have to refeed attributes

  return_list$graph_layout <- graph_layout
  return_list$edges <- edges
  return_list$nodes_cell <- nodes_cell

  #Metadata Graph creation
  if (!is.null(metadata_column_name)){

    #data tidy input - for metadata summarized plot
    if (metadata %>% pull(!!metadata_column_name) %>%
        class() %in% c("numeric","integer")){


      tmp_metadata <- metadata %>%
        select(!!metadata_column_name,"cell") %>%
        mutate(metadata_labels = cut(.data[[!!metadata_column_name]],
                                     breaks = metadata_column_nbins),
               !!metadata_column_name := dense_rank(metadata_labels))


    } else {
      #add grouping var, in this scenario redundant
      tmp_metadata <- metadata %>%
        select(!!metadata_column_name,"cell") %>%
        mutate(metadata_labels = !!metadata_column_name)
    }

    metadata_cell <- nodes_cell %>%
      select("cell","id") %>%
      left_join(tmp_metadata,
                by = "cell")


    metadata_summary <- metadata_cell %>% group_by(.data$id,
                                                   !!metadata_column_name,
                                                   .data$metadata_labels) %>%
      summarise("count" = n())

    #Nodes with Metadata for arc splitting

    #Create graph with subelements
    graph_arc <- graph_layout %>%
      left_join(metadata_summary %>% ungroup(), by = "id") %>%
      group_by(.data$id) %>%
      mutate("end" = cumsum(.data$count),
             "start" = .data$end-.data$count,
             "end" = .data$end/.data$n_size,
             "start" = .data$start/.data$n_size)

    return_list$graph_arc <- graph_arc
  }

  return(return_list)

}



#' Create Transition Matrix MRCO
#' @param cm synonym to cm, cluster matrix: data.frame, tibble or matrix excerpt from metadata, only containing each resolution step (in order)
#' @param nodes tibble of unique nodes, meaning for each resolution each cluster
#' @import tibble rlang dplyr

create_transtion_matrix_MRCO <- function(cm = cm,
                                    nodes = nodes
){

  #have a long list consisting of all possible possible clusters each cell might have
  #extract edge size as recombination of per cell from to summary,
  # resulting in each edge from to some other node individually
  cm_long <- cm %>%
    rownames_to_column(var = "cell") %>%
    pivot_longer(cols = c(everything(),-"cell"),
                 names_to = "resolution",
                 values_to = "cluster") %>%
    mutate("from" = str_c(resolution,cluster, sep  = "_")) %>%
    group_by("cell") %>%
    mutate("to" = lead(.data$from)) %>%
    group_by(from, to) %>%
    summarise("e_size" = n())

  #estimate edge size and proportions
  edge_df <- cm_long %>%
    inner_join(nodes %>%
                 dplyr::rename("Nsize_p" = "n_size") %>%
                 filter(.data$resolution != max(.data$resolution, na.rm =TRUE)) %>%
                 select("id", "Nsize_p"),
               by = c("from" = "id")) %>%
    inner_join(nodes %>%
                 dplyr::rename("Nsize_n" = "n_size") %>%
                 select("id", "Nsize_n"),
               by = c("to" = "id")) %>%
    mutate("e_prop_size" = (e_size/(2*Nsize_n))+(e_size/(2*Nsize_p))) %>%
    group_by(.data$to) %>%
    mutate("is_core" = .data$e_prop_size == max(.data$e_prop_size)) %>%
    select(-"Nsize_p", -"Nsize_n")

  return(edge_df)
}



#' check for uniqueness of x nodes in MRCO graph
#' @param graph_layout tibble, used graph_layout to plot and to be summarized
#' @param silent logical, FALSE to suppress messages and warnings
#' @import tibble rlang dplyr stringr
#'

check_unique_x_positions_MRCO <- function(graph_layout = graph_layout,
                                     silent = silent){

  #detect sequence breaks within x grouping
  # if diff is NA(first element) or 1(equal distance to next element) its a safe x stretch,
  # else 0 or larger 1 is not safe and must be corrected
  # split to be corrected stretches by cumsum label that collects each TRUE whenever break occurs
  # correct each x by their cumsum/10000 to not change graph appearance
  # by minimal shifted value x is usable as group for each continuous stretch of nodes
  graph_fixed <- graph_layout %>%
    # select(x,resolution,id) %>%
    group_by(.data$x) %>%
    arrange(resolution) %>%
    mutate("diff" = resolution-lag(resolution),
           "fix_break" = if_else(is.na(diff) | diff == 1, FALSE,TRUE),
           "break_groups" = cumsum(fix_break)) %>%
    group_by(.data$x, .data$break_groups) %>%
    mutate(x = x+(break_groups/10000))

  #update the fixed x coordinates
  graph_layout$x <- graph_fixed$x


  return(graph_layout) #return secure to use graph_layout
}


