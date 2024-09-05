#' Analyse and Plot Consistent MaAsLin Results
#'
#' This function processes the output from MaAsLin2 and MaAsLin3 analyses,
#' merges the results, and generates visualizations to compare the findings.
#' It filters significant results based on p-value and q-value thresholds,
#' identifies common features between the two analyses, and creates a plot
#' of the top differential abundances as well as a Venn diagram showing the
#' overlap between significant features in MaAsLin2 and MaAsLin3 results.
#'
#' @param maaslin2_output List. The output object from running MaAsLin2.
#'   Expected to contain a `results` data frame.
#' @param maaslin3_output List. The output object from running MaAsLin3.
#'   Expected to contain a `fit_data_abundance$results` data frame.
#' @param pthresh Numeric. The p-value threshold for significance. Default is 0.05.
#'   (0 < p-value <= q-value)
#' @param qthresh Numeric. The q-value (FDR-adjusted p-value) threshold for
#'   significance. Default is 0.1. (p-value <= q-value <= 1)
#' @param top_features Integer. The number of top features to include in the
#'   differential abundance plot. Default is 20.
#' @param output_dir The directory to save the tables and figures in output
#'
#' @return A list containing:
#'   \item{df_maas}{Data frame of merged and filtered results from both analyses}
#'   \item{df_maas_top}{Data frame of top common features}
#'   \item{main_plot}{ggplot object of the main differential abundance plot}
#'   \item{venn_plot}{grid object of the Venn diagram}
#'
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import VennDiagram
#' @import grid
#' @import ggnewscale
#' @import Maaslin2
#' @import maaslin3
#' @import tryCatchLog
#' @import assertthat
#'
#' @examples
#' \dontrun{
#' # Assuming you have run MaAsLin2 and MaAsLin3 and have their outputs
#' maaslin2_results <- run_maaslin2(your_data, your_metadata)
#' maaslin3_results <- run_maaslin3(your_data, your_metadata)
#'
#' # Process the results
#' processed_results <- process_maaslin_results(
#'   maaslin2_output = maaslin2_results,
#'   maaslin3_output = maaslin3_results,
#'   qthresh = 0.05,
#'   pthresh = 0.1,
#'   top_features = 20
#' )
#' }
#'
#' @export
analyse_and_plot_consistent_maaslin_results <- function(maaslin2_output, maaslin3_output, pthresh = 0.05, qthresh = 0.1, top_features = 20, output_dir = NULL) {

    # Input validation
    ## if not default, pthresh and qthresh valid numbers, pthresh < qthresh
    if (!is.numeric(pthresh) || !is.numeric(qthresh) || pthresh < 0 || qthresh < 0 || pthresh > qthresh || pthresh > 1 || qthresh > 1) {
        stop("pthresh and qthresh must be numeric values between 0 and 1, with pthresh < qthresh.")
    }
    ## if not default, top features is a number > 0
    if (!is.numeric(top_features) || top_features <= 0) stop("top_features must be a positive number.")

    ## Are maaslin2_output and maaslin3_output existing and correct
    ## Define required columns
    key_cols <- c("feature", "metadata", "value")
    required_cols_maas <- c(key_cols, "pval", "qval", "coef", "stderr")

    ### Retrieve the dataframe output from run_maaslin2 or Maaslin2::Maaslin2
    df_maas2 <- as.data.frame( maaslin2_output$results )
    df_maas2 <- process_maaslin_results(df_maas2, pthresh, qthresh, required_cols_maas)

    ### Retrieve the dataframe output from run_maaslin3 or maaslin3::maaslin3
    df_maas3 <- as.data.frame( maaslin3_output$fit_data_abundance$results )
    ##### can use either _joint or _individual pvalues and qvalues
    df_maas3$pval <- df_maas3$pval_joint
    df_maas3$qval <- df_maas3$qval_joint
    df_maas3 <- process_maaslin_results(df_maas3, pthresh, qthresh, required_cols_maas)

    # Calculate common features between the output of MaAsLin2 and MaAsLin3
    ## Consider only coherent results in terms of species, metadata and direction of correlation

    coef_cols   <- c("coef.x", "coef.y")
    stderr_cols <- c("stderr.x", "stderr.y")
    qval_cols   <- c("qval.x", "qval.y")
    result_maas_consistency <- maas_consistency(df_maas2, df_maas3, top_features, key_cols, coef_cols, stderr_cols, qval_cols)
    df_maas      <- result_maas_consistency$df_maas_
    df_maas_top  <- result_maas_consistency$df_maas_top_
    df_maas_plot <- result_maas_consistency$df_maas_plot_

    # Plot a figure with the top consistent results of differential abundance analysis
    main_plot <- plot_consensus_maaslin(df_maas_plot, qthresh, output_dir)

    # Create also a Venn Diagram with the number of statiistically significant correlations
    venn_plot <- generate_venn_diagram(df_maas2, df_maas3, key_cols, output_dir)

    # Save the tables to the specified path if output_dir is provided
    if (!is.null(output_dir)) {
        output_dir <- ensure_output_dir(output_dir)

        # all common
        file_path <- file.path(output_dir, "common_features_maaslin2_maaslin3.csv")
        write.csv(df_maas, file = file_path, row.names = FALSE)
        message(paste("All common maaslin features saved to", file_path))

        # top common
        file_path <- file.path(output_dir, "top_common_features_maaslin2_maaslin3.csv")
        write.csv(df_maas_top, file = file_path, row.names = FALSE)
        message(paste("Top common maaslin features saved to", file_path))
    }

    return(list(df_maas = df_maas, df_maas_top = df_maas_top, df_maas_plot = df_maas_plot, main_plot = main_plot, venn_plot = venn_plot))
}
