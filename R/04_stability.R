#' Cluster Stability Estimator
#' @description Estimate stable cluster twig in relation to whole graph by four measures:
#' \itemize{
#'  \item{"Mean edge proportional size of twig"}{\cr UPDATE DEBUG A twig, clusters of incremental resolution on same x axis position, may be declared to be stable if the edge proportion (edge_size/from_node_size) is always close to 1. Therefore the mean of that edge sequence will also be close to 1.}
#'  \item{"Number of clusters within a twig"}{\cr UPDATE DEBUG A twig is likely to be stable if it contains a large proportion of available clusters. Available clusters will be the same across each branch of the graph, one per resolution that is tested.}
#' }
#' Due to many possible options the best recombination is still being tested, for now the following parameters are used to estimate best cluster choices:
#' \itemize{
#'  \item {"Mean edge proportional size of twig"}{\cr UPDATE DEBUG edge_size/from_node_size where the first element of each twig is skipped. This reduces dependence of twigs creation eg if the first cluster of the twig derives from a perfect 50/50 split then from_cluster_size will be twice as large leaving the proportional edge size crippled though the rest of the twig might be perfect cell conservation.}
#'  \item {"Number of clusters within a twig"}{\cr UPDATE DEBUG n_cluster/resolution_steps where the (vertical) nr of clusters available is equal to the resolution steps. We take the proportion to avoid biases across different graph strucutres.}
#' }
#' @param graph_layout contains the to be plotted graph as tibble
#' @param edges tibble containing from and to of each edge
#' @param branch_df matrix, each column is one branch from root to leaf, length of list is nr of leafs
#' @param plot logical, FALSE to skip plotting
#' @param edge_ratio_weigth numeric between 0 and 1, when stable edges are determined their edge ratio must be larger than the branch paths maximum edge ratio timed edge_ratio_weight.
#' Therefore, a value closer to 1 is less permissive towards noise, whereas a value closer to 0 may handle noisier graphs better.
#' @import tibble rlang dplyr

cluster_stability_estimator_MRCO <- function(
    graph_layout,
    edges,
    branch_df,
    edge_ratio_weigth = .9,
    plot = TRUE) {
  ## create variables for scatter drop per branch:

  clustering_steps <- nrow(branch_df)

  ## For each branch path: quantify edge ratio
  # ER = (N_edge/(2*Nsize_p))+(N_edge/(2*Nsize_n))
  # N_edge is number of cells in edge
  # Nsize is number of cells in node _p previous and _n next of edge

  ## Needs: Edge list containing Nprev & Nnext
  edge_ratios <- edges %>%
    left_join(graph_layout %>%
      select("id", "n_size"), by = c("from" = "id")) %>%
    mutate("Nsize_p" = .data$n_size) %>%
    select(-"n_size") %>%
    left_join(graph_layout %>% select("id", "n_size"),
      by = c("to" = "id")
    ) %>%
    mutate(
      "Nsize_n" = .data$n_size,
      "ER" = (.data$e_size / (2 * .data$Nsize_p)) + (.data$e_size / (2 * .data$Nsize_n)) # ,
    ) %>%
    select(-"n_size")


  ## Then Create a long list of all paths and take their ER

  branch_paths <- branch_df %>%
    pivot_longer(everything(),
      names_to = "branch",
      values_to = "id"
    ) %>%
    group_by(.data$branch) %>%
    left_join(edge_ratios %>% ungroup() %>%
      select("from", "to", "ER"), by = c("id" = "from")) %>%
    filter(.data$to %in% lead(unique(.data$id))) %>%
    select(-"to") %>%
    left_join(graph_layout %>% select("x", "resolution", "id"), by = c("id")) %>%
    mutate(
      "ER_max_b" = max(.data$ER, na.rm = TRUE),
      "stable_edge" = .data$ER > (.data$ER_max_b * edge_ratio_weigth)
    )

  # like dense_rank but label gets higher for each time that diff in X is larger 1
  return_seq_class <- function(X) {
    diff_x <- diff(X)
    return_v <- vector(mode = "numeric", length = length(X))
    class_count <- 1
    return_v[1] <- 1
    for (i in seq_along(diff_x)) {
      if (diff_x[i] == 1) {
        return_v[i + 1] <- class_count
      }
      if (diff_x[i] != 1) {
        class_count <- class_count + 1
        return_v[i + 1] <- class_count
      }
    }
    return(return_v)
  }

  branch_twig_eval <- branch_paths %>%
    group_by(.data$branch, .data$x) %>%
    filter(.data$stable_edge == TRUE) %>%
    mutate("seq_label" = return_seq_class(.data$resolution)) %>%
    group_by(.data$branch, .data$x, .data$seq_label) %>%
    mutate("stable_twig_seq_n" = n())

  # if broader/upper clusters preferred use first on seq label as it will favor
  # the lower resolution twig, else use last for higher resolution
  branch_twig_stables <- branch_twig_eval %>%
    group_by(.data$branch) %>%
    filter(.data$stable_twig_seq_n == max(.data$stable_twig_seq_n,
      na.rm = TRUE
    )) %>%
    filter(first(.data$seq_label) == .data$seq_label) %>%
    slice_max(order_by = .data$ER, n = 1, with_ties = FALSE)


  return_list <- list("estimated_stable" = unique(branch_twig_stables %>%
    pull(.data$id)))
  return(return_list)
}
