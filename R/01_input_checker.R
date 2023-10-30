
#' Input checker NGCS
#' @param metadata data.frame or tibble with dim-names: row names cell identities and column names cell level metadata variables
#' @param metadata_column_name character or tidy-selection style unquoted name of a metadata column to plot piechart nodes from
#' @param prefix character, prefix of metadata columns which contain the increments of resolution
#' @param suffix character, suffix of metadata columns which contain the increments of resolution
#' @param cellnames_column_name Optional NULL, character or name of a column containing ceell names in metadata
#' @param metadata_column_nbins NULL, "all" or a numeric, set the number of bins to create from given metadata column if it is continuous data
#' @param plot_col_gradient NULL for auto detect, or logical, TRUE to use continuous fill, FALSE for discrete fill; only applies when continuous metadata column is selected

#' @param count_matrix UPDATE sparseMatrix or other matrix forms, UPDATE which exactly, containing the counts used for cluster_Scatter calculation
#' @param cluster_scatter_matrix UPDATE sparseMatrix or other matrix forms, UPDATE which exactly, containing calculated cluster scatter for each cluster found in metadata
#' @param suggest_cut logical, TRUE to suggest stable nodes based on graph structure, FALSE to skip
#' @param reduce_branchlist logical, TRUE to walk as few paths as possible while still visiting every node, FALSE to walk any possible path passing the edge_filters; warning memory expensive, adjust branches_overflow parameter! Only relevant if suggest_cut is TRUE.
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
#' @import tibble rlang dplyr
#'
input_checker_NGCS <- function(
    metadata = NULL,
    metadata_column_name = NULL,
    prefix = NULL, suffix = NULL,
    metadata_column_nbins = NULL,
    plot_col_gradient = NULL,

    # count_matrix = NULL,
    # cluster_scatter_matrix = NULL,
    suggest_cut = NULL,
    reduce_branchlist = NULL,
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
    highlight_selection = NULL
    ){
  #prepare return vars
  return_vars <- list(
    "metadata" = NULL,
    "cm" = NULL)

  #capture forwarding patterns
  metadata_column_name  <- rlang::enexpr(metadata_column_name)


  ###check metadata ----
  check_class <- class(metadata) %in% c("data.frame","tibble")
  if (!any(check_class)) stop("Unsupported metadata format.
                         Must be either data.frame or tibble.")

  if (is.null(colnames(metadata))) stop("Metadata must contain columnnames
                                        to identifiy clustering resolution
                                        and other cell level metadata
                                        such as cellname or batch.")



  #Extract from metadata rows must not result in NULL
  if (is.null(rownames(metadata))){
    # if (warnings) {
    #   warning("No rownames in metadata detected, recommendet is giving metadata with exact cell ids as rownames to ensure data safety.")
    # }
    metadata <- metadata %>% mutate("cell" = seq_len(nrow(metadata)))

  } else {
    # if rownames are present
    metadata <- metadata %>% tibble::rownames_to_column(var = "cell")
  }

  if (!is.null(metadata_column_name)){
    #if metadata_column_name is given we must be able to subset it from metadata
    if (! (as_string(metadata_column_name) %in% colnames(metadata))) stop(
      "Provided metadata_column_name is not in given metadata.")
  }

  #check if metadata column name and internal cell column overlap
  if (!base::is.null(metadata_column_name)){
    if (base::eval(base::as.character(metadata_column_name)) == "cell"){
      if (any(colnames(metadata) == "cell" )){
        stop("Metadata to plot as piechart must not exist in a column named cell.
        Please relable metada_column_name as cell column is used internally.")
      }
    }
  }



  #check prefix & suffix
  if (is.null(prefix) & is.null(suffix)){
    if (!silent) message("Arguments prefix and suffix are highly suggested! Please make sure that NGCS can identify each clustering resolution column.")
  }
  metadata_colnames <- colnames(metadata)
  if (!is.null(prefix)) {
    if (is.character(prefix) & (length(prefix) == 1)){
      i_prefix <- grepl(paste0("^",prefix,collapse = ""),metadata_colnames)
    } else stop("Given prefix is not a single character string!")
  } else {
    i_prefix <- grepl("",metadata_colnames)
  }

  if (!is.null(suffix)) {
    if (is.character(suffix) & (length(suffix) == 1)){
      i_suffix <- grepl(paste0(suffix,"$",collapse = ""),metadata_colnames)
    } else stop("Given suffix is not a single character string!")
  } else {
    i_suffix <- grepl("",metadata_colnames)
  }

  i_select <- which(i_prefix & i_suffix)
  if (!any(i_select)) stop("
  No columns detected in metadata with given prefix and suffix.")



  #create cluster matrix from metadata
  cm <- metadata %>%
    dplyr::select(c(all_of(i_select),-"cell")) %>%
    base::as.data.frame()
  colnames(cm) <- seq_len(ncol(cm))
  rownames(cm) <- metadata %>% pull(cell)

  #save changed input vars
  return_vars$cm <- cm
  return_vars$metadata <- metadata



  # metadata_column_nbins auto assignments and check:
  if (!is.null(metadata_column_name)){
    #detect nr of uniques:
    metadata_unique_val_n <- length(unique(metadata %>% pull(!!metadata_column_name)))
    #detect class:
    metadata_unique_class <- metadata %>% pull(!!metadata_column_name) %>% class()


    if (class(no_labels) != "logical"){
      stop("Unexpected Class for argument no_labels! TRUE or FALSE logicals required.")
    }

    if (is.null(metadata_column_nbins)){
      #if metadata_column name is given but not the nr of bins to split into:

      if (metadata_unique_class %in% c("numeric","integer")){
        #set bin size according to available nr of unqiues; or up to 6 bins by default:
        if (metadata_unique_val_n <= 6) {
          metadata_column_nbins <- metadata_unique_val_n
        } else {
          metadata_column_nbins <- 6
        }
      } else {
        #keep metadata_column_nbins NULL incase argument is unused due to metadata_column class
        metadata_column_nbins <- NULL #running this line is not required, just for logical overview
      }
    } else {
      #user supplied:
      if (metadata_unique_class %in% c("character","factor")){
        if (warnings) warning("Argument metadata_column_nbins only works on numeric and integer columns. Use NULL to avoid this warning.")
      }

      if (metadata_column_nbins == "all"){
        metadata_column_nbins <- length(unique(metadata %>% pull(!!metadata_column_name)))
      } else if (metadata_unique_class %in% c("numeric", "integer")){
        #if user supplied bin nr, does it fit?:
        if (metadata_unique_val_n < metadata_column_nbins){
          if (warnings) warning("Given number of metadata_column_nbins larger than available uniques in selected metadata_column. Adjusting to nr of uniques available.")
          metadata_column_nbins <- metadata_unique_val_n
        }
      }
    }

    #incase user has provided wishes regarding plot gradient:
    if (!is.null(plot_col_gradient)){
      if (metadata_unique_class %in% c("character","factor")){
    #     #warn that argument is dropped on character columns:
        if (warnings){warning("Argument plot_col_gradient only works on numeric and integer columns. Use NULL to avoid this warning.")}
      }
    }

    #incase user has not provided info about plot_col_gradient:
    if (!is.null(metadata_column_nbins) & is.null(plot_col_gradient)){
      if (metadata_unique_class %in% c("numeric","integer")){
        plot_col_gradient <- TRUE
      } else {plot_col_gradient <- FALSE}
    } else {plot_col_gradient <- FALSE}


  }



  return_vars$plot_col_gradient <- plot_col_gradient
  return_vars$metadata_column_nbins <- metadata_column_nbins






  #Edge filter:
  ##numerical:
  if (!class(edge_num_size_filter) %in% c("numeric", "integer")) stop("Expected input class for edge_num_size_filter is numeric")
  if (edge_num_size_filter >= nrow(metadata)) stop("Given numerical size filter for edges is larger than available nr of cells in input metadata, please reduce filter value.")
  if (edge_num_size_filter < 0) stop("Numerical size filter for edges must be larger or equal to zero, please raise filter value.")

  ##proportional:
  if (!class(edge_prop_size_filter) %in% c("numeric", "integer")) stop("Expected input class for edge_prop_size_filter is numeric")
  if (edge_prop_size_filter >= 1) stop("Given proportional size filter for edges is larger or equal to one, please reduce filter value.")
  if (edge_prop_size_filter < 0) stop("Proportional size filter for edges must be larger or equal to zero, please raise filter value.")


  ##Other Arguments, Simple Checks
  if (!(class(suggest_cut) %in% c("logical"))) stop("Expected input class for suggest_cut is logical.")
  if (!(class(reduce_branchlist) %in% c("logical"))) stop("Expected input class for reduce_branchlist is logical.")
  if (!(class(merge_downwards) %in% c("logical"))) stop("Expected input class for merge_downwards is logical.")
  if (!(class(silent) %in% c("logical"))) stop("Expected input class for silent is logical.")
  if (!(class(warnings) %in% c("logical"))) stop("Expected input class for warnings is logical.")
  if (!class(plot) == "logical") stop("User argument plot must be of class logical.")
  if (!class(highlight_selection) == "logical") stop("User argument highlight_selection must be of class logical.")

  ##check node selection & convert from list to vector:
  if (is.list(nodes_selection)){
    res_names <- names(nodes_selection)
    id_selected <- c()
    for (l in 1:length(nodes_selection)){
      id_selected <- c(id_selected,paste(res_names[l],nodes_selection[[l]], sep = "_"))
    }#end for loop
    nodes_selection <- id_selected
  }


  return_vars$nodes_selection <- nodes_selection


  return(return_vars)

}
