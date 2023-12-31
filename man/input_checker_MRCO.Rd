% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/01_input_checker.R
\name{input_checker_MRCO}
\alias{input_checker_MRCO}
\title{Input checker MRCO}
\usage{
input_checker_MRCO(
  metadata = NULL,
  metadata_column_name = NULL,
  clustering_columns = NULL,
  nbins = NULL,
  suggest_cut = NULL,
  edge_num_size_filter = NULL,
  edge_prop_size_filter = NULL,
  nodes_selection = NULL,
  merge_downwards = NULL,
  plot = NULL,
  no_labels = NULL,
  silent = NULL,
  highlight_selection = NULL
)
}
\arguments{
\item{metadata}{data.frame or tibble: with row names (cell ids) and
column names (cell metadata variables)}

\item{metadata_column_name}{character or unquoted name: name the metadata
column to plot piechart nodes from}

\item{clustering_columns}{character prefix or tidy-select: to distinguish
clustering resolution columns from metadata columns.}

\item{nbins}{numeric, set the number of bins to create from given metadata
column if it is numeric data}

\item{suggest_cut}{logical, TRUE to suggest stable nodes based on graph
structure, FALSE to skip automated selection}

\item{edge_num_size_filter}{numeric, give the minimum number of samples
required to draw an edge}

\item{edge_prop_size_filter}{numeric, give the minimum edge ratio, which is
displaying the proportion of samples that move between nodes to draw an edge}

\item{nodes_selection}{character vector or list, two ways to manually select
a node by its global id from the graph}

\item{merge_downwards}{logical, cells which are in multiple selected clusters
 can either be assorted into the first or last cluster available for them.
FALSE means that clusters are merged from the bottom towards the top of
 the graph, hence cells which are part of a higher resolution cluster will
  remain in it.
TRUE means that clusters are merged from top towards the bottom, hence cells
 will end up in the lowest resolution cluster that they may end in.}

\item{plot}{logical, FALSE to skip plotting}

\item{no_labels}{logical, TRUE to turn off cluster labels, FALSE default for labeled clusters}

\item{silent}{logical, FALSE to suppress messages and warnings}

\item{highlight_selection}{logical, TRUE to highlight selected nodes}
}
\value{
list, containing cleaned up and additional functional internal dataframe subsets such as cm
}
\description{
Input checker MRCO
}
