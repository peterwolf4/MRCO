% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/05_plot_graph.R
\name{createplot_piechart}
\alias{createplot_piechart}
\title{create pie chart node plot MRCO}
\usage{
createplot_piechart(
  plot_MRCO,
  metadata_column_name,
  graph_layout,
  graph_arc,
  highlight_selection,
  no_labels
)
}
\arguments{
\item{plot_MRCO}{ggplot, MRCO plot with only edges}

\item{metadata_column_name}{character or tidy-selection style unquoted name of a metadata column to plot piechart nodes from}

\item{graph_layout}{tibble, used graph_layout to plot and to be summarized}

\item{graph_arc}{tibble, used graph_arc to plot piecharts}

\item{highlight_selection}{logical, TRUE to highlight selected nodes}

\item{no_labels}{logical, TRUE to turn off cluster labels, FALSE default for labeled clusters}
}
\value{
ggplot MRCO piechart plot
}
\description{
create MRCO plot with pie chart nodes (multiple colours per node).
}
