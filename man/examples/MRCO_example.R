if (requireNamespace("utils")){
    library(MRCO)
    data(example_data)

    #Plot Clusters as Treelike Graph. Use the prefix to select clustering columns.
    MRCO(example_data, clustering_columns = "0.")
    # Piechart plot both discrete ...
    MRCO(example_data, metadata_column_name = "quality",
         clustering_columns = "0.")
    # ... and continuous sample level metadata.
    MRCO(example_data, "expr_A", clustering_columns = "0.")
    # Select your clusters manually at wish.
    # Each node is pasted by its resolution step (Y axis) and its cluster NR.
    # The top node is called 0_1, the bottom leafs are 4_1, 4_2, 4_3, 4_0 etc.
    # Either select by named list or character vector.
    MRCO(example_data, "batch", clustering_columns = "0.",
         nodes_selection=list("3"=c(1, 0),
                              "4"=c(2, 3)))
    MRCO_res <- MRCO(example_data, clustering_columns = "0.",
                     nodes_selection=c("2_0","4_3"))

    # Find the selection & adjust it at wish.
    MRCO_res$selected_nodes
    #Any sample not selected end up in a 'debris' cluster
    head(MRCO_res$MRCO_clustering)
}
