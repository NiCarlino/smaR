#' Compare and Process MaAsLin2 and MaAsLin3 Results
#'
#' This function merges and processes the results from MaAsLin2 and MaAsLin3 analyses,
#' identifying consistent features and preparing data for visualization.
#'
#' @param df_maas2_ Data frame containing MaAsLin2 results.
#' @param df_maas3_ Data frame containing MaAsLin3 results.
#' @param top_features_ Integer. Number of top features to select based on coefficient magnitude.
#' @param key_cols_ Character vector. Names of columns to use as keys for merging.
#' @param coef_cols_ Character vector. Names of coefficient columns.
#' @param stderr_cols_ Character vector. Names of standard error columns.
#' @param qval_cols_ Character vector. Names of q-value columns.
#'
#' @return A list containing three data frames:
#'   \item{df_maas_}{Merged and filtered results from both analyses}
#'   \item{df_maas_top_}{Top features based on coefficient magnitude}
#'   \item{df_maas_plot_}{Data prepared for plotting}
#'
#' @import tidyr
#'
#' @examples
#' df_maas2 <- data.frame(feature = c("A", "B", "C"), coef.x = c(1, -2, 3),
#'                        stderr.x = c(0.1, 0.2, 0.3), qval.x = c(0.01, 0.02, 0.03))
#' df_maas3 <- data.frame(feature = c("B", "C", "D"), coef.y = c(-2.1, 3.1, 4),
#'                        stderr.y = c(0.15, 0.25, 0.35), qval.y = c(0.015, 0.025, 0.035))
#' result <- maas_consistency(df_maas2, df_maas3, 2, "feature",
#'                            c("coef.x", "coef.y"), c("stderr.x", "stderr.y"),
#'                            c("qval.x", "qval.y"))
#'
#' @export
maas_consistency <- function(df_maas2_, df_maas3_, top_features_, key_cols_, coef_cols_, stderr_cols_, qval_cols_) {

    # Input validation
    if (!is.data.frame(df_maas2_) || !is.data.frame(df_maas3_)) {
        stop("df_maas2_ and df_maas3_ must be data frames")
    }

    if (!is.numeric(top_features_) || top_features_ <= 0) {
        stop("top_features_ must be a positive integer")
    }

    if (!all(sapply(list(key_cols_, coef_cols_, stderr_cols_, qval_cols_), is.character))) {
        stop("key_cols_, coef_cols_, stderr_cols_, and qval_cols_ must be character vectors")
    }

    # Merge the dataframes by key columns: common features
    df_maas_ <- merge(df_maas2_, df_maas3_, by=c(key_cols_), all=FALSE)

    # Filter features  with the same direction
    select_cols_ = c(key_cols_, coef_cols_, stderr_cols_, qval_cols_)
    df_maas_ <- df_maas_[ sign(df_maas_$coef.x) == sign(df_maas_$coef.y), select_cols_ ]

    # Stop if no common features found
    if (nrow(df_maas_) == 0) stop("No common features found between Maaslin2 and Maaslin3 that meet the criteria.")

    # Calculate the maximum of the absolute values of coef.x and coef.y
    df_maas_$max_coef <- pmax(abs(df_maas_$coef.x), abs(df_maas_$coef.y))

    # Sort the dataframe by max_coef in descending order
    df_maas_ <- df_maas_[order(-df_maas_$max_coef), ]

    # Select the top #top_features_ features
    df_maas_top_ <- head(df_maas_, n = min(nrow(df_maas_), top_features_))

    # Create a new column to order the top features with the correct sign
    df_maas_top_$custom_order <- ifelse(df_maas_top_$coef.x > 0, #& df_maas_top_$coef.y > 0, coefficients are of the same sign for the above constraint
                                   pmax(df_maas_top_$coef.x, df_maas_top_$coef.y),
                                   pmin(df_maas_top_$coef.x, df_maas_top_$coef.y))

    # Sort by the custom_order column in descending order and then drop the column
    df_maas_top_ <- df_maas_top_[order(-df_maas_top_$custom_order), ]
    df_maas_top_$custom_order <- NULL

    # Transform table to plot the consensus different abundance analysis
    # Pivot longer on coefficient columns
    df_long <- tidyr::pivot_longer(df_maas_top_,
                                   cols = c(coef_cols_, stderr_cols_, qval_cols_),
                                   names_to = c(".value", "type"),
                                   names_sep = "\\.")

    # Filter the long dataframe for plotting
#    df_maas_plot_ <- df_long[(df_long$type == "x" & df_long$qval == df_long$qval.x) |
#                             (df_long$type == "y" & df_long$qval == df_long$qval.y), ]

    # Create lower and upper bounds for the error bars
    df_maas_plot_ <- df_long
    df_maas_plot_$lower <-  df_maas_plot_$coef - df_maas_plot_$stderr
    df_maas_plot_$upper <-  df_maas_plot_$coef + df_maas_plot_$stderr
    # Create a label column based on key columns
    df_maas_plot_$labelll <- apply( df_maas_plot_[, key_cols_], 1, paste0, collapse = " | ")

    # Return the three dataframes as a list
    return(list(df_maas_ = df_maas_, df_maas_top_ = df_maas_top_, df_maas_plot_ = df_maas_plot_))
}
