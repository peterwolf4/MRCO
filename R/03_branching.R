#' Create node branch list breath first function
#' @param graph_layout active graph layout that is used, at this stage with unique x positions per unique branch
#' @param edges edge list
#' @returns data.frame tibble of branches required to walk the full graph
#' @import tibble rlang dplyr stringr



create_node_branch_list_MRCO <- function(graph_layout = graph_layout,
                                         edges = edges) {
    # initiate branches by using leafs (assuming highest resolution has most nodes)
    leaf_nodes <- graph_layout %>%
        filter(.data$resolution == max(.data$resolution)) %>%
        pull("id")
    res_steps <- graph_layout %>%
        select("resolution") %>%
        distinct() %>%
        pull()
    branch_str <-
        matrix("X_Y",
               nrow = length(res_steps),
               ncol = length(leaf_nodes))
    branch_str[length(res_steps),] <- leaf_nodes

    # fill in branches from bottom to top into pre-allocated matrix
    # for loop required, since each row depends on the results of previous

    for (n in seq((length(res_steps) - 1), 1,-1)) {
        to_nodes <- branch_str[n + 1,]
        to_edges <- edges %>%
            filter(.data$to %in% to_nodes) %>%
            group_by(.data$to) %>%
            slice_max(
                order_by = .data$e_prop_size,
                n = 1,
                with_ties = FALSE
            )

        if (any(is.na(match(to_nodes, to_edges$to)))) {
            stop(
                "Matching edges to node failed. Please report this bug!
             Current node found no input edge. Faulty data/Graph supplied?"
            )
        }
        branch_str[n,] <- to_edges$from[match(to_nodes, to_edges$to)]
    }

    # order branches according to plot
    plot_order <- graph_layout %>%
        filter(.data$resolution == max(.data$resolution)) %>%
        arrange(.data$x) %>%
        pull(.data$id)
    branch_str <- branch_str[, match(plot_order, leaf_nodes)]
    colnames(branch_str) <- seq_len(ncol(branch_str))

    return(as_tibble(branch_str, .name_repair = "minimal"))
}
