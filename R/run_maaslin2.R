#' Run MaAsLin2 Analysis
#'
#' This function runs a MaAsLin2 analysis on the provided abundance table and metadata.
#'
#' @param abund_table A matrix or data frame of abundance data
#' @param metadata_table A data frame of metadata
#' @param fixed_effects A character vector of column names in metadata to use as fixed effects
#' @param random_effects A character vector of column names in metadata to use as random effects
#' @param other_params A list of other parameters to pass to MaAsLin2
#' @param output_dir The directory to save the MaAsLin2 output
#' @param rowtype The type of data in the rows of abund_table ("feature" or "sample")
#'
#' @return The results of the MaAsLin2 analysis
#'
#' @import Maaslin2
#' @import tryCatchLog
#' @import assertthat
#'
#' @export
run_maaslin2 <- function(abund_table, metadata_table, fixed_effects, random_effects = NULL,
                         other_params = list(), output_dir, rowtype = "feature") {

    # Validate and process input abundance table
    abund_table <- validate_abund_table(abund_table, rowtype)

    # Check metadata table and effects/columns
    assertthat::assert_that(is.data.frame(metadata_table),
            msg = "metadata_table must be a data frame")

    # Check effects
    assertthat::assert_that(is.character(fixed_effects), length(fixed_effects) > 0,
                msg = "fixed_effects must be a non-empty character vector")
    assertthat::assert_that(is.character(random_effects) || is.null(random_effects),
                msg = "random_effects must be a character vector or NULL")
    assertthat::assert_that(all(c(fixed_effects, random_effects) %in% colnames(metadata_table)),
                msg = "All effects must be column names in metadata_table")

    metadata_table_ <- metadata_table[ c(var_fix, var_ran) ]
    metadata_table_ <- convert_columns_to_factors( metadata_table_, c(var_fix, var_ran))

    # Ensure output directory exists
    output_dir <- ensure_output_dir(output_dir)

    # Run MaAsLin2
    # results <- Maaslin2::Maaslin2(
    #                         abund_table,
    #                         metadata_table_,
    #                         output_dir,
    #             fixed_effects = fixed_effects,
    #             random_effects = random_effects        )
    #     return(results)

    tryCatch({
        base_params <- list(
            input_data = abund_table,
            input_metadata = metadata_table_,
            output = output_dir,
            fixed_effects = fixed_effects,
            random_effects = random_effects
        )

        # Combine base_params with other_params, if other_params is not NULL
        all_params <- if (!is.null(other_params)) c(base_params, other_params) else base_params

        results <- do.call(Maaslin2::Maaslin2, all_params)
        return(results)
    }, error = function(e) {
        stop(paste("Error in MaAsLin2 execution:", e$message))
    })

}
