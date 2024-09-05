#' Validate and Preprocess Microbiome Relative Abundance Tables as Input
#'
#' This function performs validation and correction on a relative abundance table, following common preprocessing steps on inputs of a computational metagenomic pipeline.
#' It includes steps to handle zero-abundance rows and columns, check and correct table orientation, and normalize column values.
#'
#' This function validates and preprocesses the inputs of a computational metagenomic pipeline.
#'
#' @param abund_table A microbiome abundance table (matrix or data frame). Rows are species, and columns are samples.
#' @param rowtype A character string specifying the type of data in rows (e.g., "species", "OTUs" or "samples").
#'
#' @return The processed abund_table with transposed data if needed, samples with zero profiles removed, and normalized values.
validate_abund_table <- function(abund_table, rowtype) {

    # Input validation -> transform in dataframe if not already
    if (!is.data.frame(abund_table)) {
        if (!is.matrix(abund_table)) {
            stop("abund_table must be a matrix or data frame")
        }

        abund_table <- as.data.frame(abund_table)
    }

    # Ensure the data is a matrix of numeric values
    numeric_columns <- sapply(abund_table, is.numeric)
    if (!all(numeric_columns)) {
        warning("Some columns are not numeric and will be removed.")
        abund_table <- abund_table[, numeric_columns]
    }


  # Transpose if samples are in rows and species in columns
    if (tolower(rowtype) %in% c("sample", "samples")) {
        message("Transposing the table to have species as rows and samples as columns.")
        abund_table <- t(abund_table)
    } else if (tolower(rowtype) %in% c("feature", "features", "species", "specie", "otu", "otus", "asv", "asvs")) {
        message("Not transposing. Samples already columns and features already rows.")
    } else {
        stop("Row types should be either 'samples' or 'features'/'species'/'OTUs'/'ASVs'. Please check your data.")
    }


    # Fill NA with 0s and check negative values
    abund_table[is.na(abund_table)] <- 0
    if (any(abund_table < 0, na.rm = TRUE)) {
        stop("abund_table must contain only non-negative values")
    }

    # Remove samples with empty profiles (all zeros)
    empty_samples <- which(colSums(abund_table) == 0)

    if (length(empty_samples) > 0) {
        if (length(empty_samples) == 1) {
            message("# WARNING: 1 sample with 100% unknown species was removed from the input table.")
        } else {
            message(paste0("# WARNING: ", length(empty_samples), " samples with 100% unknown species were removed from the input table."))
        }
        abund_table <- abund_table[, -empty_samples, drop = FALSE]
    }


    # Remove species not present in the cohort
    absentspecies <- which(rowSums(abund_table) == 0)
    if (length(absentspecies) > 0) {
        if (length(absentspecies) == 1) {
            message("# WARNING: 1 absent species was removed from the input table.")
        } else {
            message("# WARNING: ", length(absentspecies), " absent species with 0 abundance were removed from the input table.")
        }
        abund_table <- abund_table[-absentspecies, ]
    }


    # Check and normalize relative abundances if necessary
    cs <- colSums(abund_table)
    mxcs <- max(cs)
    mics <- min(cs)

    if (mxcs <= 100.01 & mics >= 99.99) {
        message("Matrix to be normalized. Dividing by 100.")
        abund_table <- abund_table / 100
    } else if (mxcs <= 1.0001 & mics >= 0.9999) {
        message("Matrix already normalized.")
    } else {
        # Check if sums are consistent enough for normalization
        if (sd(cs) / mean(cs) < 0.01) {
            message("Matrix has consistent sums but not normalized. Normalizing by column sums.")
            abund_table <- sweep(abund_table, 2, cs, "/")
        } else {
            stop("Relative abundances do not sum up to 1 or 100, and column sums are inconsistent. Please check your data.")
        }
    }

    return(abund_table)
}
