#' Input checker MRCO

#' @param metadata data.frame or tibble: with row names (cell ids) and
#'  column names (cell metadata variables)
#' @param metadata_column_name character or unquoted name: name the metadata
#'  column to plot piechart nodes from
#' @param clustering_columns character prefix or tidy-select: to distinguish
#' clustering resolution columns from metadata columns.
#' @param nbins numeric, set the number of bins to create from given metadata
#'  column if it is numeric data
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

#' @param silent logical, FALSE to suppress messages and warnings
#' @param plot logical, FALSE to skip plotting
#' @param no_labels logical, TRUE to turn off cluster labels, FALSE default for labeled clusters

#' @returns list, containing cleaned up and additional functional internal dataframe subsets such as cm
#' @import tibble rlang dplyr
#'
input_checker_MRCO <- function(metadata = NULL,
                               metadata_column_name = NULL,
                               clustering_columns = NULL,
                               nbins = NULL,
                               suggest_cut = NULL,
                               # reduce_branchlist = NULL,
                               edge_num_size_filter = NULL,
                               edge_prop_size_filter = NULL,
                               #
                               nodes_selection = NULL,
                               merge_downwards = NULL,
                               #
                               plot = NULL,
                               no_labels = NULL,
                               silent = NULL,
                               highlight_selection = NULL) {
  # prepare return vars
  return_vars <- list(
    "metadata" = NULL,
    "cm" = NULL
  )



  ### check metadata ----
  metadata_check <- input_check_metadata_MRCO(
    metadata = metadata,
    metadata_column_name = metadata_column_name,
    nbins = nbins
  )

  return_vars$metadata <- metadata_check$metadata
  return_vars$nbins <- metadata_check$nbins
  rm(metadata_check)


  ### Creating Cluster Matrix with user selection guidance
  return_vars$cm <- input_check_clustermatrix_MRCO(
    metadata = return_vars$metadata,
    clustering_columns = clustering_columns
  )



  # return_vars$cm <- cm



  # Edge filter:
  ## numerical:
  if (!is.numeric(edge_num_size_filter)) stop("Expected input class for edge_num_size_filter is numeric")
  if (edge_num_size_filter >= nrow(metadata)) stop("Given numerical size filter for edges is larger than available nr of cells in input metadata, please reduce filter value.")
  if (edge_num_size_filter < 0) stop("Numerical size filter for edges must be larger or equal to zero, please raise filter value.")

  ## proportional:
  if (!is.numeric(edge_prop_size_filter)) stop("Expected input class for edge_prop_size_filter is numeric")
  if (edge_prop_size_filter >= 1) stop("Given proportional size filter for edges is larger or equal to one, please reduce filter value.")
  if (edge_prop_size_filter < 0) stop("Proportional size filter for edges must be larger or equal to zero, please raise filter value.")


  ## Other Arguments, Simple Checks
  if (!(is.logical(suggest_cut))) {
    stop(
      "Expected input class for suggest_cut ",
      "is logical."
    )
  }
  if (!(is.logical(merge_downwards))) {
    stop(
      "Expected input class for ",
      "merge_downwards is logical."
    )
  }
  if (!(is.logical(silent))) stop("Expected input class for silent is logical.")
  if (!is.logical(plot)) stop("User argument plot must be of class logical.")
  if (!is.logical(highlight_selection)) {
    stop(
      "User argument ",
      "highlight_selection must be of ",
      "class logical."
    )
  }

  ## check node selection & convert from list to vector:
  if (is.list(nodes_selection)) {
    res_names <- names(nodes_selection)
    id_selected <- c()

    nodes_selection <- lapply(seq_along(nodes_selection), function(l) {
      id_selected <- c(
        id_selected,
        paste(res_names[l], nodes_selection[[l]], sep = "_")
      )
    })
    return_vars$nodes_selection <- unlist(nodes_selection)
  }


  return(return_vars)
}


#' Input checker metadata MRCO

#' @param metadata data.frame or tibble: with row names (cell ids) and
#'  column names (cell metadata variables)
#' @param metadata_column_name character or unquoted name: name the metadata
#'  column to plot piechart nodes from
#' @param nbins numeric, set the number of bins to create from given metadata
#'  column if it is numeric data
#' @returns list of updated input variables
#' @import tibble rlang dplyr
#'
input_check_metadata_MRCO <- function(
    metadata = NULL,
    metadata_column_name = NULL,
    nbins = NULL) {
  if (!is.numeric(nbins)) {
    stop(
      "Unsupported nbins argument format. ",
      "Must be numeric, user supplied: ", class(nbins)
    )
  }
  if (!is.data.frame(metadata)) stop("Unsupported metadata argument format.
                                     Must be either data.frame or tibble.")
  if (is.null(colnames(metadata))) {
    stop(
      "metadata must contain columnnames to ",
      "identifiy clustering resolutions from",
      " other cell level metadata such as ",
      "cellnames or batch."
    )
  }
  if (any(colnames(metadata) == "cell")) {
    warning(
      "Metadata contains a column named 'cell'. Column dropped because ",
      "variable name is reserved for internal purposes. If you require the ",
      "variable cell to be plotted please rename it."
    )
    metadata <- metadata %>% select(-"cell")
  }
  if (is.null(rownames(metadata))) {
    metadata <- metadata %>% mutate("cell" = seq_len(nrow(metadata)))
  } else {
    # if rownames are present
    metadata <- metadata %>% tibble::rownames_to_column(var = "cell")
  }

  ##### metadata_column_name checks:
  # if column is present in input data automatically checked by tidyselect
  # if column name targeted is cell, which is (so far) internally used

  if (!quo_is_null(metadata_column_name)) {
    if (as_name(metadata_column_name) == "cell") {
      stop(
        "Metadata to plot as piechart must not exist in a column named ",
        "'cell'. Please rename metadata_column_name because 'cell' column is ",
        "used inside MRCO internally."
      )
    }

    if (is.numeric(pull(metadata, !!metadata_column_name))) {
      # if numeric, check that number of wanted bins is feasible

      ##### nbins check
      max_bins <- pull(metadata, !!metadata_column_name) %>%
        unique() %>%
        length()

      if (nbins > max_bins) {
        stop(
          "Number of bins request by user: ", nbins,
          ", is larger than maximum available bins: ", max_bins,
          " from selected numeric variable ", as_name(metadata_column_name), "."
        )
      }
      if (nbins <= 0) {
        stop("nbins number must not be negative or zero. User input is")
      }
    }
  }


  list(
    "metadata" = metadata,
    "nbins" = nbins
  )
}






#' Input checker cluster matrix MRCO

#' @param metadata data.frame or tibble with dim-names: row names cell identities and column names cell level metadata variables
#' @param clustering_columns character or tidyselection call
#' @returns data.frame of all clustering resolutions selected from metadata
#' @import tibble rlang dplyr
#'
input_check_clustermatrix_MRCO <- function(
    metadata = NULL,
    clustering_columns = NULL) {
  if (quo_is_null(clustering_columns)) {
    stop(
      "Specifying clustering_columns is required to have",
      " safe data handling!"
    )
  }
  if (quo_is_call(clustering_columns)) {
    cm <- metadata %>%
      dplyr::select(!!clustering_columns, -"cell") %>%
      base::as.data.frame()
  } else {
    cm <- metadata %>%
      dplyr::select(c(starts_with(as_name(clustering_columns)), -"cell")) %>%
      base::as.data.frame()
  }
  colnames(cm) <- seq_len(ncol(cm))
  rownames(cm) <- metadata %>% pull(.data$cell)

  if (ncol(cm) <= 2) {
    stop(
      "Only less or equal to two resolution steps found.",
      "Please re-run with more resolution steps.
                          Did you manage to select all of your clustering ",
      " columns? MRCO detected: ", ncol(cm), "  columns."
    )
  }

  return(cm)
}
