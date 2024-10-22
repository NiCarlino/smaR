% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_alpha_diversity.R
\name{calculate_alpha_diversity}
\alias{calculate_alpha_diversity}
\title{Calculate Alpha Diversity}
\usage{
calculate_alpha_diversity(
  abund_table,
  metric = "shannon",
  output_dir = NULL,
  file_name = "alpha_diversity_matrix.csv"
)
}
\arguments{
\item{abund_table}{A microbiome abundance table (matrix or data frame)}

\item{metric}{Alpha diversity metric to use: "richness", "shannon", or "simpson"}

\item{output_dir}{Optional; a character string specifying the file path to save the output. If provided, the output will be saved to this path. Default is NULL.}

\item{file_name}{Optional; a character string specifying the name of the file to save the plot as. Default is "alpha_diversity_matrix.csv".}
}
\value{
A data frame containing the calculated alpha diversity with the metric in the column name
}
\description{
This function calculates alpha diversity for a given microbiome table.
}
