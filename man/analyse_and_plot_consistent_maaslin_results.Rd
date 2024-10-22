% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyse_and_plot_consistent_maaslin_results.R
\name{analyse_and_plot_consistent_maaslin_results}
\alias{analyse_and_plot_consistent_maaslin_results}
\title{Analyse and Plot Consistent MaAsLin Results}
\usage{
analyse_and_plot_consistent_maaslin_results(
  maaslin2_output,
  maaslin3_output,
  pthresh = 0.05,
  qthresh = 0.1,
  top_features = 20,
  output_dir = NULL
)
}
\arguments{
\item{maaslin2_output}{List. The output object from running MaAsLin2.
Expected to contain a \code{results} data frame.}

\item{maaslin3_output}{List. The output object from running MaAsLin3.
Expected to contain a \code{fit_data_abundance$results} data frame.}

\item{pthresh}{Numeric. The p-value threshold for significance. Default is 0.05.
(0 < p-value <= q-value)}

\item{qthresh}{Numeric. The q-value (FDR-adjusted p-value) threshold for
significance. Default is 0.1. (p-value <= q-value <= 1)}

\item{top_features}{Integer. The number of top features to include in the
differential abundance plot. Default is 20.}

\item{output_dir}{The directory to save the tables and figures in output}
}
\value{
A list containing:
\item{df_maas}{Data frame of merged and filtered results from both analyses}
\item{df_maas_top}{Data frame of top common features}
\item{main_plot}{ggplot object of the main differential abundance plot}
\item{venn_plot}{grid object of the Venn diagram}
}
\description{
This function processes the output from MaAsLin2 and MaAsLin3 analyses,
merges the results, and generates visualizations to compare the findings.
It filters significant results based on p-value and q-value thresholds,
identifies common features between the two analyses, and creates a plot
of the top differential abundances as well as a Venn diagram showing the
overlap between significant features in MaAsLin2 and MaAsLin3 results.
}
\examples{
# Create dummy data
your_data <- matrix(rnorm(100), nrow = 10)
your_metadata <- data.frame(
  condition = rep(c("A", "B"), each = 5),
  age = runif(10, 20, 60)
)

# Run MaAsLin2 and MaAsLin3 (these are not real functions, just for illustration)
maaslin2_results <- data.frame(
  feature = paste0("feature", 1:5),
  pval = runif(5),
  qval = runif(5)
)
maaslin3_results <- data.frame(
  feature = paste0("feature", 1:5),
  pval = runif(5),
  qval = runif(5)
)

# Process the results
processed_results <- process_maaslin_results(
  maaslin2_output = maaslin2_results,
  maaslin3_output = maaslin3_results,
  pthresh = 0.05,
  qthresh = 0.1,
  top_features = 20
)

\dontrun{
# In a real scenario, you would run MaAsLin2 and MaAsLin3 like this:
# maaslin2_results <- run_maaslin2(your_data, your_metadata)
# maaslin3_results <- run_maaslin3(your_data, your_metadata)
}

}
