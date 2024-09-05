#' Calculate Beta Diversity
#'
#' This function calculates beta diversity for a given microbiome table.
#'
#' @param abund_table A microbiome abundance table (matrix or data frame)
#' @param metric Beta diversity metric to use: "bray-curtis" or "jaccard"
#' @param output_dir Optional; a character string specifying the file path to save the output. If provided, the output will be saved to this path. Default is NULL.
#' @param file_name Optional; a character string specifying the name of the file to save the plot as. Default is "beta_diversity_matrix.csv".
#'
#' @return A list containing:
#'   \item{result_mat}{A distance object}
#'   \item{result_df}{A dataframe representing a NxN matrix with the distances, which is the one saved in output}
#'
#' @import rbiom
#'
#' @export
calculate_beta_diversity <- function(abund_table, metric = "bray-curtis", output_dir = NULL, file_name = "beta_diversity_matrix.csv") {

    # Input validation: Ensure abund_table is a matrix for rbiom::beta function
    if (!is.matrix(abund_table)) {
        if (!is.data.frame(abund_table)) {
            stop("abund_table must be a matrix or data frame")
    }

        abund_table <- as.matrix(abund_table)
    }

    valid_metrics <- c("bray-curtis", "jaccard")

    if (!(metric %in% valid_metrics)) {
        stop(paste0("Metric '", metric, "' name unknown. Beta-diversity can be calculated for 'bray-curtis' or 'jaccard'"))
    }

    result <- switch(metric,
        "bray-curtis" = rbiom::beta.div(abund_table, method = "bray-curtis", weighted = TRUE),
        "jaccard" = rbiom::beta.div(abund_table, method = "jaccard", weighted = FALSE)
    )

    # Convert result to a data frame
    result_df <- as.data.frame(as.matrix(result))

    # Save the result to the specified path if output_dir is provided
    if (!is.null(output_dir)) {
        output_dir <- ensure_output_dir(output_dir)
        file_path <- file.path(output_dir, file_name)

        # Save the result as a CSV file
        write.csv(result_df, file = file_path, row.names = TRUE)
        message(paste("Beta diversity results saved to", file_path))
    }

    return(list(result_dist = result, result_df = result_df))
}
