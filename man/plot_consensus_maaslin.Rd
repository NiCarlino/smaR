% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_consensus_maaslin.R
\name{plot_consensus_maaslin}
\alias{plot_consensus_maaslin}
\title{Plot Consensus MaAsLin Results}
\usage{
plot_consensus_maaslin(
  df_maas_plot_,
  qthresh_,
  output_dir = NULL,
  file_name = "consensus_maaslin_plot_top_features.png"
)
}
\arguments{
\item{df_maas_plot_}{A data frame containing the processed MaAsLin results. Must include
columns: 'labelll', 'coef', 'type', 'lower', 'upper', and 'qval'.}

\item{qthresh_}{Numeric. The q-value threshold for significance, used for color scaling.
Must be a positive number.}

\item{output_dir}{Character. Optional. Directory path where the plot should be saved.
If NULL (default), the plot is not saved to a file.}

\item{file_name}{Character. Optional. Name of the file to save the plot, including extension.
Default is "consensus_maaslin_plot_top_features.png".}
}
\value{
A ggplot2 object representing the consensus MaAsLin plot.
}
\description{
This function generates a plot comparing the results from MaAsLin2 and MaAsLin3 analyses.
It visualizes the beta coefficients and their confidence intervals for features that are
significant in both analyses, allowing for easy comparison of the results.
}
\details{
The function creates a plot with points and error bars representing the beta coefficients
and their confidence intervals. MaAsLin2 results are shown in blue, and MaAsLin3 results
in red. The color intensity of each point represents the q-value, with darker colors
indicating lower (more significant) q-values.
}
\examples{
# Assuming df_maas_plot is your processed data frame
plot <- plot_consensus_maaslin(
  df_maas_plot_ = df_maas_plot,
  qthresh_ = 0.05,
  output_dir = "results",
  file_name = "my_consensus_plot.png"
)

# Display the plot
print(plot)

}
