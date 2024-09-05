#' Calculate Alpha Diversity
#'
#' This function calculates alpha diversity for a given microbiome table.
#'
#' @param abund_table A microbiome abundance table (matrix or data frame)
#' @param metric Alpha diversity metric to use: "richness", "shannon", or "simpson"
#' @param output_dir Optional; a character string specifying the file path to save the output. If provided, the output will be saved to this path. Default is NULL.
#' @param file_name Optional; a character string specifying the name of the file to save the plot as. Default is "alpha_diversity_matrix.csv".
#'
#' @return A data frame containing the calculated alpha diversity with the metric in the column name
#'
#' @import microbiome
#'
#' @export
calculate_alpha_diversity <- function(abund_table, metric = "shannon", output_dir = NULL, file_name = "alpha_diversity_matrix.csv") {

    # Input validation: Ensure abund_table is a matrix for microbiome::alpha function
    if (!is.matrix(abund_table)) {
    if (!is.data.frame(abund_table)) {
        stop("abund_table must be a matrix or data frame")
    }

    abund_table <- as.matrix(abund_table)
    }

    valid_metrics <- c('richness', 'shannon', 'simpson')

    if (any(!(metric %in% valid_metrics))) {
        stop("Metrics must be one or more of 'richness', 'shannon', or 'simpson'")
    }

    # Initialize an empty list to store results
    results_list <- list()

    # Loop through each metric and calculate alpha diversity
    for (mm in metric) {
        result <- switch(mm,
            "richness" = microbiome::alpha(abund_table, index = "richness_observed"),
            "shannon" = microbiome::alpha(abund_table, index = "diversity_shannon"),
            "simpson" = microbiome::alpha(abund_table, index = "diversity_gini_simpson")
        )

        # Store the result in the list with a named element
        results_list[[paste0("alpha_diversity_", mm)]] <- result
      }

    # Combine the list into a single data frame
    result_df <- do.call(cbind, results_list)


    # Save the result to the specified path if output_dir is provided
    if (!is.null(output_dir)) {
        output_dir <- ensure_output_dir(output_dir)
        file_path <- file.path(output_dir, file_name)

        # Save the result as a CSV file
        write.csv(result_df, file = file_path, row.names = FALSE)
        message(paste("Alpha diversity results saved to", file_path))
    }

    return(result_df)
}
