#' Input checker MRCO

#' @param metadata data.frame or tibble with dim-names: row names cell identities and column names cell level metadata variables
#' @param metadata_column_name character or tidy-selection style unquoted name of a metadata column to plot piechart nodes from
#' @param prefix character, prefix of metadata columns which contain the increments of resolution
#' @param suffix character, suffix of metadata columns which contain the increments of resolution
#' @param nbins NULL, "all" or a numeric, set the number of bins to create from given metadata column if it is continuous data
#' @param plot_col_gradient NULL for auto detect, or logical, TRUE to use continuous fill, FALSE for discrete fill; only applies when continuous metadata column is selected

#' @param suggest_cut logical, TRUE to suggest stable nodes based on graph structure, FALSE to skip
#' @param edge_num_size_filter numeric, give the number of cells that must be surpassed to draw edge, has to be set for each data set as its highly dependent on nr of cells within data
#' @param edge_prop_size_filter numeric, give the proportion of cells that must move between nodes to draw edge

#' @param nodes_selection vector or list, two ways to manually select a node by its global id from the graph:
#' either give resolution step and cluster id from that resolution as underscore separated character string,
#'  or give a list named where each name corresponds to a resolution step and its element is a single vector containing the cluster ids that are to be selected of that resolution.
#' @param merge_downwards logical, cells which are in multiple selected clusters can either be assorted into the first or last cluster available for them.
#' FALSE means that clusters are merged from the bottom towards the top of the graph, hence cells which are part of a higher resolution cluster will remain in it.
#' TRUE means that clusters are merged from top towards the bottom, hence cells will end up in the lowest resolution cluster that they may end in.

#' @param silent logical, FALSE to suppress messages and warnings
#' @param plot logical, FALSE to skip plotting
#' @param no_labels logical, TRUE to turn off cluster labels, FALSE default for labeled clusters
#' @param warnings logical, set to FALSE if warnings should not be printed to console.
#' @param highlight_selection logical, TRUE to highlight selected nodes
#' @returns list, containing cleaned up and additional functional internal dataframe subsets such as cm
#' @import tibble rlang dplyr
#'
input_checker_MRCO <- function(metadata = NULL,
                               metadata_column_name = NULL,
                               prefix = NULL, suffix = NULL,
                               nbins = NULL,
                               plot_col_gradient = NULL,
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
                               warnings = NULL,
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
    nbins = nbins,
    plot_col_gradient = plot_col_gradient
  )

  metadata <- metadata_check$metadata
  return_vars$plot_col_gradient <- metadata_check$plot_col_gradient
  return_vars$nbins <- metadata_check$nbins
  rm(metadata_check)

  # check prefix & suffix
  if (is.null(prefix) & is.null(suffix)) {
    if (!silent) message("Arguments prefix and suffix are highly suggested! Please make sure that MRCO can identify each clustering resolution column.")
  }
  metadata_colnames <- colnames(metadata)
  if (!is.null(prefix)) {
    if (is.character(prefix) & (length(prefix) == 1)) {
      i_prefix <- grepl(paste0("^", prefix, collapse = ""), metadata_colnames)
    } else {
      stop("Given prefix is not a single character string!")
    }
  } else {
    i_prefix <- grepl("", metadata_colnames)
  }

  if (!is.null(suffix)) {
    if (is.character(suffix) & (length(suffix) == 1)) {
      i_suffix <- grepl(paste0(suffix, "$", collapse = ""), metadata_colnames)
    } else {
      stop("Given suffix is not a single character string!")
    }
  } else {
    i_suffix <- grepl("", metadata_colnames)
  }

  i_select <- which(i_prefix & i_suffix)
  if (!any(i_select)) stop("
  No columns detected in metadata with given prefix and suffix.")
  if (length(i_select) == 1) {
    warning("Only a single resolution step found. Check input data or prefix?")
  }



  # create cluster matrix from metadata
  cm <- metadata %>%
    dplyr::select(c(all_of(i_select), -"cell")) %>%
    base::as.data.frame()
  colnames(cm) <- seq_len(ncol(cm))
  rownames(cm) <- metadata %>% pull(.data$cell)

  # save changed input vars
  return_vars$cm <- cm
  return_vars$metadata <- metadata



  # Edge filter:
  ## numerical:
  if (!class(edge_num_size_filter) %in% c("numeric", "integer")) stop("Expected input class for edge_num_size_filter is numeric")
  if (edge_num_size_filter >= nrow(metadata)) stop("Given numerical size filter for edges is larger than available nr of cells in input metadata, please reduce filter value.")
  if (edge_num_size_filter < 0) stop("Numerical size filter for edges must be larger or equal to zero, please raise filter value.")

  ## proportional:
  if (!class(edge_prop_size_filter) %in% c("numeric", "integer")) stop("Expected input class for edge_prop_size_filter is numeric")
  if (edge_prop_size_filter >= 1) stop("Given proportional size filter for edges is larger or equal to one, please reduce filter value.")
  if (edge_prop_size_filter < 0) stop("Proportional size filter for edges must be larger or equal to zero, please raise filter value.")


  ## Other Arguments, Simple Checks
  if (!(class(suggest_cut) %in% c("logical"))) stop("Expected input class for suggest_cut is logical.")
  # if (!(class(reduce_branchlist) %in% c("logical"))) stop("Expected input class for reduce_branchlist is logical.")
  if (!(class(merge_downwards) %in% c("logical"))) stop("Expected input class for merge_downwards is logical.")
  if (!(class(silent) %in% c("logical"))) stop("Expected input class for silent is logical.")
  if (!(class(warnings) %in% c("logical"))) stop("Expected input class for warnings is logical.")
  if (!class(plot) == "logical") stop("User argument plot must be of class logical.")
  if (!class(highlight_selection) == "logical") stop("User argument highlight_selection must be of class logical.")

  ## check node selection & convert from list to vector:
  if (is.list(nodes_selection)) {
    res_names <- names(nodes_selection)
    id_selected <- c()
    for (l in seq_along(nodes_selection)) {
      id_selected <- c(id_selected, paste(res_names[l], nodes_selection[[l]], sep = "_"))
    } # end for loop
    nodes_selection <- id_selected
  }


  return_vars$nodes_selection <- nodes_selection


  return(return_vars)
}




#' Input checker metadata MRCO

#' @param metadata data.frame or tibble with dim-names: row names cell identities and column names cell level metadata variables
#' @param metadata_column_name character or tidy-selection style unquoted name of a metadata column to plot piechart nodes from
#' @param plot_col_gradient NULL for auto detect, or logical, TRUE to use continuous fill, FALSE for discrete fill; only applies when continuous metadata column is selected
#' @import tibble rlang dplyr
#'
input_check_metadata_MRCO <- function(
    metadata = NULL,
    metadata_column_name = NULL,
    nbins = NULL,
    plot_col_gradient = NULL) {
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
      stop("Metadata to plot as piechart must not exist in a column named 'cell'.
        Please rename metadata_column_name because 'cell' column is used inside MRCO internally.")
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
    } else {
      # if not numeric, check that plot_col_gradients is set to FALSE

      ##### plot_col_gradient checks:
      # if plot_col_gradients = T, only allow for numeric, else set to false!
      if (plot_col_gradient == TRUE) {
        if (!is.numeric(pull(metadata, !!metadata_column_name))) {
          warning("Plotting with colour gradients is only possible for numerical variables. Set plot_col_gradient to FALSE to remove warning.")
          plot_col_gradient <- FALSE
        }
      }
    }
  }

  list(
    "metadata" = metadata,
    "plot_col_gradient" = plot_col_gradient,
    "nbins" = nbins
  )
}
