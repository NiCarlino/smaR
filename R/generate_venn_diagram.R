#' Generate Venn Diagram for MaAsLin2 and MaAsLin3 Results
#'
#' This function creates a Venn diagram to visualize the overlap between
#' significant features identified by MaAsLin2 and MaAsLin3 analyses.
#'
#' @param df1 Data frame containing MaAsLin2 results.
#' @param df2 Data frame containing MaAsLin3 results.
#' @param key_cols_ Character vector specifying the columns to use for creating labels.
#' @param output_dir Character. Optional. Directory path where the plot should be saved.
#'   If NULL (default), the plot is not saved to a file.
#' @param file_name Character. Optional. Name of the file to save the plot, including extension.
#'   Default is "venn_diagram_maaslin.png".
#'
#' @return A grid object representing the Venn diagram (invisibly).
#'
#' @import VennDiagram
#' @import grid
#'
#' @examples
#' df1 <- data.frame(feature = c("A", "B", "C"), metadata = c("X", "Y", "Z"))
#' df2 <- data.frame(feature = c("B", "C", "D"), metadata = c("Y", "Z", "W"))
#' venn_plot <- generate_venn_diagram(df1, df2, c("feature", "metadata"))
#'
#' @export
generate_venn_diagram <- function(df1, df2, key_cols_, output_dir = NULL, file_name = "venn_diagram_maaslin.png") {
  # Input validation
  if (!is.data.frame(df1) || !is.data.frame(df2)) {
    stop("df1 and df2 must be data frames")
  }
  if (!is.character(key_cols_) || length(key_cols_) == 0) {
    stop("key_cols_ must be a non-empty character vector")
  }
  if (!all(key_cols_ %in% colnames(df1)) || !all(key_cols_ %in% colnames(df2))) {
    stop("All key_cols_ must be present in both df1 and df2")
  }

  # Helper function to compute sign as symbols
    sign_to_symbol <- function(x) {
      if (!is.numeric(x)) return(NA_character_)
      ifelse(is.na(x), NA_character_,
             ifelse(x < 0, '-', ifelse(x == 0, '0', '+')))
    }

    # Check if "coef" is in both df1 and df2
    if (all(c("coef") %in% colnames(df1), "coef" %in% colnames(df2))) {
      # Apply the function to both dataframes
      df1$signcoef <- sign_to_symbol(df1$coef)
      df2$signcoef <- sign_to_symbol(df2$coef)

      # Add "signcoef" to key_cols_ if neither "coef" nor "signcoef" are present
      if (!any(c("coef", "signcoef") %in% key_cols_)) {
        key_cols_ <- c(key_cols_, "signcoef")
      }
    }

  # Create labels
  create_labels <- function(df) {
    if (length(key_cols_) == 1) {
      return(as.character(df[[key_cols_]]))
    } else {
      return(apply(df[, key_cols_, drop = FALSE], 1, paste, collapse = " | "))
    }
  }

  venn_data <- list(
    Maaslin2 = create_labels(df1),
    Maaslin3 = create_labels(df2)
  )

  # Create the Venn diagram
  venn.plot <- VennDiagram::venn.diagram(
    x = venn_data,
    category.names = c("Maaslin2", "Maaslin3"),
    fill = c("blue", "red"),
    alpha = 0.3,
    cex = 2,
    cat.cex = 2,
    lwd = 1,
    cat.pos = c(-20, 20),
    main = "Common Features between Maaslin2 and Maaslin3",
    filename = NULL
  )

  # Save the plot if output_dir is provided
  if (!is.null(output_dir)) {
    output_dir <- ensure_output_dir(output_dir)
    file_path <- file.path(output_dir, file_name)
    png(file_path, width = 800, height = 600)
    grid::grid.draw(venn.plot)
    dev.off()
    message("Venn diagram saved to: ", file_path)
  }

  # Display the plot
  grid::grid.newpage()
  grid::grid.draw(venn.plot)

  return(invisible(venn.plot))
}
