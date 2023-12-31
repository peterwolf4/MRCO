% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/04_stability.R
\name{stable_split_vars_MRCO}
\alias{stable_split_vars_MRCO}
\title{Stable var split  finder}
\usage{
stable_split_vars_MRCO(
  metadata,
  cm,
  edges,
  nodes,
  clean_split_columns,
  graph_layout,
  nbins
)
}
\arguments{
\item{metadata}{full input to select clean_split_columns}

\item{cm}{dataframe of all clustering resolutions}

\item{edges}{tibble containing from and to of each edge}

\item{nodes}{tibble containing nodes}

\item{clean_split_columns}{tidyselection of interesting vars}

\item{graph_layout}{contains the to be plotted graph as tibble}

\item{nbins}{numeric number of bins to cut numeric data into}
}
\value{
list of full and reduced dataframe of potentially interesting splits
}
\description{
Try to locate if some of your variables of interest have splits
 correlating with one of the provided metadata variable
}
