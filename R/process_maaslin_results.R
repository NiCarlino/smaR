#' Process MaAsLin Results
#'
#' This function filters and processes the results from a MaAsLin analysis based on
#' specified p-value and q-value thresholds. It also ensures that all required columns
#' are present and contain non-NA values.
#'
#' @param df_maas_ A data frame containing the MaAsLin results.
#' @param pthresh_maas Numeric. The p-value threshold for filtering results. (0 < p-value <= q-value)
#' @param qthresh_maas Numeric. The q-value threshold for filtering results. (p-value <= q-value <= 1)
#' @param required_cols Character vector. Names of columns that must be present and non-NA.
#'
#' @return A data frame containing the filtered and processed MaAsLin results.
#'
#' @export
process_maaslin_results <- function(df_maas_, pthresh_maas, qthresh_maas, required_cols) {

    # Input validation
    if (!is.data.frame(df_maas_)) {
        stop("df_maas_ must be a data frame")
    }
    if (!is.numeric(pthresh_maas) || !is.numeric(qthresh_maas) || pthresh_maas < 0 || pthresh_maas > 1 || qthresh_maas < 0 || qthresh_maas > 1 || pthresh_maas > qthresh_maas) {
        stop("pthresh_maas and qthresh_maas must be numeric values between 0 and 1")
    }
    if (!is.character(required_cols) || length(required_cols) == 0) {
        stop("required_cols must be a non-empty character vector")
    }

    # Check if the necessary columns are present
    missing_cols <- setdiff(required_cols, colnames(df_maas_))
    if (length(missing_cols) > 0) {
        stop(sprintf("The following required columns are missing in df_maas_: %s",
                     paste(missing_cols, collapse = ", ")))
    }

    # Filter all features to only significant ones, based on p-value and q-value thresholds
    df_maas_filtered <- df_maas_[df_maas_$pval < pthresh_maas & df_maas_$qval < qthresh_maas, ]

    # Delete rows where there are NA values in the required columns
    df_maas_filtered <- df_maas_filtered[complete.cases(df_maas_filtered[required_cols]), ]

    # Check if there are statistically significant features
    if (nrow(df_maas_filtered) == 0) {
        stop("No statistically significant features found after filtering.")
    }

    return(df_maas_filtered)

}
