### WARNING ONLY RUNS LOCAL - DO NOT RUN from GIT

# Load pbmc
pbmc_m <- readRDS("~/Documents/datasets/pbmc3k/pbmc_clustered_metadata.rds")
testthat::test_that("Metadata_From_Seurat", {
  testthat::expect_type(pbmc_m %>% MRCO("nFeature_RNA", "RNA"), "list")
  testthat::expect_type(pbmc_m %>% MRCO(nFeature_RNA, "RNA"), "list")
  testthat::expect_type(pbmc_m %>% MRCO(clustering_columns = "RNA"), "list")
})

pbmc_t <- pbmc_m %>% as_tibble()
testthat::test_that("Metadata_From_Seurat_asTibble", {
  testthat::expect_type(pbmc_t %>% MRCO("nFeature_RNA", "RNA"), "list")
  testthat::expect_type(pbmc_t %>% MRCO(nFeature_RNA, "RNA"), "list")
  testthat::expect_type(pbmc_t %>% MRCO(clustering_columns = "RNA"), "list")
})


pbmc_r <- pbmc_m %>% select(starts_with("RNA"))
testthat::test_that("Metadata_From_DF_OnlyClusters,Noclustering_columns", {
  testthat::expect_type(pbmc_r %>% MRCO(clustering_columns = "RNA"), "list")
  testthat::expect_error(pbmc_r %>% MRCO(clustering_columns = ".*"), "Only ")
  testthat::expect_error(pbmc_r %>% MRCO(), "Specifying clustering_columns")
})
testthat::test_that("Metadata_From_Matrix_error", {
  testthat::expect_error(
    pbmc_r %>% as.matrix() %>% MRCO(clustering_columns = "RNA"),
    "Unsupported metadata "
  )
})


testthat::test_that("Metadata_colname_cell_error", {
  testthat::expect_error(pbmc_r %>% MRCO(cell, clustering_columns = "RNA"),
    regexp = "Metadata to plot as piechart must not exist in a column named"
  )
  testthat::expect_error(pbmc_t %>% MRCO(cell, clustering_columns = "RNA"),
    regexp = "Metadata to plot as piechart must not exist in a column named"
  )
  testthat::expect_error(pbmc_m %>% MRCO(cell, clustering_columns = "RNA"),
    regexp = "Metadata to plot as piechart must not exist in a column named"
  )
})

testthat::test_that("Metadata_colname_cell_warning", {
  testthat::expect_warning(
    pbmc_t %>% add_column("cell" = "cell_values") %>%
      MRCO(clustering_columns = "RNA"),
    regexp = "Metadata contains a column named"
  )
  testthat::expect_warning(
    pbmc_m %>% add_column("cell" = "cell_values") %>%
      MRCO(clustering_columns = "RNA"),
    regexp = "Metadata contains a column named"
  )
})
