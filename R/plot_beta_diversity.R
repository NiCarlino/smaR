#' Perform NMDS on Beta Diversity Matrix and Plot Results
#'
#' This function calculates a 2D NMDS (Non-metric Multidimensional Scaling) from a beta diversity distance matrix
#' and generates a scatter plot of the resulting points, optionally coloring the points based on a metadata variable.
#'
#' @param beta_mat A distance matrix representing beta diversity, either as a matrix or a dist object.
#' @param metadata_table A data frame containing metadata information, where the rownames match the sample names in beta_mat.
#' @param plot_variables A character string specifying the metadata columns to show in the plot. Default is NULL.
#'        Can be either one variable or a list with two variables.
#' @param maxit An integer specifying the maximum number of iterations for the NMDS algorithm (default is 50).
#' @param output_dir Optional; a character string specifying the directory where the plot should be saved. If provided, the directory will be created if it does not exist.
#' @param file_name Optional; a character string specifying the name of the file to save the plot as. Default is "beta_diversity_plot.png".
#'
#' @return A ggplot2 plot object.
#'
#' @import ggplot2
#' @import labdsv
#'
#' @export
plot_beta_diversity <- function(beta_mat, metadata_table, plot_variables = NULL, maxit = 50,
                                output_dir = NULL, file_name = "beta_diversity_plot.png") {

    # Input validation
     if (!is.matrix(beta_mat) && !inherits(beta_mat, "dist") && !is.data.frame(beta_mat)) {
        stop("beta_mat must be a distance matrix (matrix, dist object, or data frame).")
     }

    if (is.matrix(beta_mat)) {
        beta_mat <- dist(beta_mat)
      }

    if (!all(rownames(beta_mat) == colnames(beta_mat))) {
      stop("If beta_mat is a matrix or data frame, it must have matching rownames and colnames.")
    }

    if (!is.data.frame(metadata_table)) {
        stop("metadata_table must be a data frame with samples as rownames")
    }

    if (!is.character(plot_variables) || length(plot_variables) == 0) {
      stop("key_cols_ must be a non-empty character vector")
    }

    if (!all(plot_variables %in% colnames(metadata_table))) {
        stop("plot_variables must be a valid column name in metadata_table")
    }

  # Perform NMDS
  nmds <- labdsv::nmds(beta_mat, k = 2, maxit = maxit, trace = FALSE)
  betas <- as.data.frame(nmds$points)
  colnames(betas) <- c("NMDS1", "NMDS2")

  # Merge with metadata
  df_beta_plot <- merge(betas, metadata_table, by = "row.names", all = FALSE)
  rownames(df_beta_plot) <- df_beta_plot$Row.names
  df_beta_plot <- df_beta_plot[, -1]  # Remove the Row.names column

  # Check for missing samples in metadata or beta diversity table
  missing_in_metadata <- setdiff(rownames(betas), rownames(metadata_table))
  missing_in_beta     <- setdiff(rownames(metadata_table), rownames(betas))
  n_missing_in_beta    <- length(missing_in_beta)
  n_missing_in_metadata <- length(missing_in_metadata)

  if (n_missing_in_beta) {
    warning("# WARNING: Samples in metadata but not in beta diversity table: ", n_missing_in_beta)
  }
  if (n_missing_in_metadata) {
    warning("# WARNING: Samples in beta diversity table but not in metadata: ", n_missing_in_metadata)
  }

  # Convert plot_variables column to factor if necessary
   df_beta_plot <- convert_columns_to_factors(df_beta_plot, plot_variables)

  # Create the plot
  p <- ggplot2::ggplot(data = df_beta_plot, ggplot2::aes(x = NMDS1, y = NMDS2)) +
    ggplot2::geom_point(size = 2, ggplot2::aes_string(colour = plot_variables[1],
                                            shape = if(length(plot_variables) > 1) plot_variables[2] else NULL)) +
    ggplot2::theme_light() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 11, colour = 'black'),
      axis.text.y = ggplot2::element_text(size = 11, colour = 'black'),
      legend.text = ggplot2::element_text(size = 10)
    ) +
    ggplot2::labs(x = "NMDS1", y = "NMDS2", colour = plot_variables[1],
                  shape = if(length(plot_variables) > 1) plot_variables[2] else NULL)

  # Save the plot if output_dir is provided
  if (!is.null(output_dir)) {
    output_dir <- ensure_output_dir(output_dir)
    file_path <- file.path(output_dir, file_name)
    ggplot2::ggsave(filename = file_path, plot = p)
  }

  return(p)
}
