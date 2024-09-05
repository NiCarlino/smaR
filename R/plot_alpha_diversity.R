#' Plot Alpha Diversity with Optional Faceting
#'
#' This function creates a boxplot or violin plot from a data frame of alpha diversity measures.
#' If multiple metadata columns are provided, it will automatically apply faceting.
#'
#' @param alpha_table A data frame (or matrix) where the rownames are samples and the first column contains the alpha diversity measure to plot.
#' @param metadata_table A data frame where the rownames are samples and the columns contain metadata information.
#' @param plot_type A character string specifying the type of plot to create: "boxplot" (default) or "violinplot".
#' @param plot_variables A character string or vector specifying the name(s) of the column(s) to use for separating the samples on the x-axis of the resulting plot. If more than one column is provided, faceting will be applied.
#' @param title Optional; a character string specifying the title of the plot. Default is NULL.
#' @param output_dir Optional; a character string specifying the directory where the plot should be saved. If provided, the directory will be created if it does not exist.
#' @param file_name Optional; a character string specifying the name of the file to save the plot as. Default is "alpha_diversity_plot.png".
#'
#' @return A ggplot2 plot object.
#'
#' @import ggplot2
#'
#' @export
plot_alpha_diversity <- function(alpha_table, metadata_table, plot_type = "boxplot", plot_variables, title = NULL, output_dir = NULL, file_name = "alpha_diversity_plot.png") {

    # Input validation -> transform in dataframe if not already
    if (!is.data.frame(alpha_table)) {
      if (!is.matrix(alpha_table)) {
          stop("Alpha diversity must be given in form of a matrix or data frame, with samples as rownames")
      }
      alpha_table <- as.data.frame(alpha_table)
    }

    if (!is.data.frame(metadata_table)) {
        stop("Metadata table must be given in form of a dataframe with samples as rownames")
    }

    if (!plot_type %in% c("boxplot", "violinplot")) {
        stop("plot_type must be either 'boxplot' or 'violinplot'")
    }

    if (!all(plot_variables %in% colnames(metadata_table))) {
        stop("plot_variables must be valid column names in metadata_table")
    }

    # check on merge between alpha and metadata tables
    common_samples <- intersect(rownames(alpha_table), rownames(metadata_table))
    if (length(common_samples) == 0) {
           stop("No common samples between alpha_table and metadata_table")
       }
    alpha_table    <- alpha_table[common_samples, , drop = FALSE]
    metadata_table <- metadata_table[common_samples, plot_variables, drop = FALSE]
    df_alpha_plot  <- merge(alpha_table, metadata_table, by = "row.names", all = FALSE)
    rownames(df_alpha_plot) <- df_alpha_plot$Row.names
    df_alpha_plot <- df_alpha_plot[, -1]  # Remove the Row.names column

    # Check for missing samples
    #samples without alpha values
    missing_in_alpha      <- setdiff(rownames(metadata_table), rownames(alpha_table))
    missing_in_metadata   <- setdiff(rownames(alpha_table), rownames(metadata_table))
    n_missing_in_alpha    <- length(missing_in_alpha)
    n_missing_in_metadata <- length(missing_in_metadata)

    if (n_missing_in_alpha) {
        message("# WARNING: Samples in metadata but not in alpha diversity table: ", n_missing_in_alpha)
    }

    if (n_missing_in_metadata) {
        message("# WARNING: Samples in alpha diversity table but not in metadata: ", n_missing_in_metadata)
    }

    # Convert metadata columns to factors
    df_alpha_plot <- convert_columns_to_factors( df_alpha_plot, plot_variables)

    # Define the y-axis label as the name of the first column (alpha diversity measure)
    y_axis_label <- colnames(df_alpha_plot)[1]

    # Create the ggplot object
    p <- ggplot2::ggplot(df_alpha_plot, ggplot2::aes_string(x = plot_variables[1], y = y_axis_label))

    # Add the appropriate plot layer based on plot_type
    plot_layer <- switch(plot_type,
                        "boxplot"   = ggplot2::geom_boxplot(ggplot2::aes_string(fill = plot_variables[1])),
                        "violinplot" = ggplot2::geom_violin(ggplot2::aes_string(fill = plot_variables[1])),
                        stop("Invalid plot_type specified"))
    p <- p + plot_layer

    # Add faceting if more than one metadata field is provided
    if (length(plot_variables) > 1) {
    facet_formula <- paste("~", paste(plot_variables[-1], collapse = " + "))
    p <- p + ggplot2::facet_wrap(as.formula(facet_formula))
  }

    # Customize the plot appearance
    p <- p +
    ggplot2::labs(x = plot_variables[1], y = y_axis_label, title = title) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.position = "right",
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
    )

    # Saving plot if required
    if (!is.null(output_dir)) {
        output_dir <- ensure_output_dir(output_dir)
        file_path <- file.path(output_dir, file_name)
        ggplot2::ggsave(filename = file_path, plot = p)
    }

    # Return the ggplot object
    return(p)
}
