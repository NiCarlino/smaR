#' Convert Specified Columns to Factors
#'
#' This function takes a data frame and a vector of column names, and converts
#' the specified columns to factors if they are not already factors or numeric.
#'
#' @param df A data frame containing the data.
#' @param columns A character vector of column names to be checked and possibly converted to factors.
#'
#' @return The modified data frame with specified columns converted to factors.
#'
#' @export
convert_columns_to_factors <- function(df, columns) {
  # Input validation
  if (!is.data.frame(df)) {
    stop("Input df must be a data frame")
  }

  if (!all(columns %in% colnames(df))) {
    stop("All specified columns must exist in the data frame")
  }

  # Convert specified columns to factors if they are not already factors or numeric
  df[columns] <- lapply(df[columns], function(col) {
    if (!is.factor(col) & !is.numeric(col)) {
      return(as.factor(col))
    }
    return(col)
  })

  return(df)
}
