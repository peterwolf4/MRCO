# usethis::use_test("wrong_input_combinations")

# Load example_data
data(example_data)

test_that("wrong_suf_clustering_columns_input_types", {
  expect_error(example_data %>% MRCO(
    clustering_columns = "bad_clustering_columns",
    suggest_cut = T
  ), "Only less or equal to two resolution steps")

  expect_error(example_data %>% MRCO(
    clustering_columns = "bad_clustering_columns",
    suggest_cut = T
  ), "Only less or equal to two resolution steps")
  expect_error(example_data %>% MRCO(
    clustering_columns = "bad_clustering_columns",
    suggest_cut = T
  ), "Only less or equal to two resolution steps")

  ## without suggest cut:
  expect_error(example_data %>% MRCO(
    clustering_columns = "bad_clustering_columns",
    suggest_cut = F
  ), "Only less or equal to two resolution steps")
  expect_error(example_data %>% MRCO(
    clustering_columns = "bad_clustering_columns",
    suggest_cut = F
  ), "Only less or equal to two resolution steps")
  expect_error(example_data %>% MRCO(
    clustering_columns = "bad_clustering_columns",
    suggest_cut = F
  ), "Only less or equal to two resolution steps")
})




test_that("wrong_metadata_column", {
  expect_error(example_data %>% MRCO(
    metadata_column_name = "bad_metadata_column_name",
    clustering_columns = "RNA",
    suggest_cut = T
  ), "Can't extract")
})

test_that("wrong_metadata_nbin_ignored", {
  expect_type(example_data %>% MRCO(
    metadata_column_name = "quality",
    clustering_columns = "0.",
    suggest_cut = T,
    nbins = 15
  ), "list")
})
