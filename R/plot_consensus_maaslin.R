#' Plot Consensus MaAsLin Results
#'
#' This function generates a plot comparing the results from MaAsLin2 and MaAsLin3 analyses.
#' It visualizes the beta coefficients and their confidence intervals for features that are
#' significant in both analyses, allowing for easy comparison of the results.
#'
#' @param df_maas_plot_ A data frame containing the processed MaAsLin results. Must include
#'   columns: 'labelll', 'coef', 'type', 'lower', 'upper', and 'qval'.
#' @param qthresh_ Numeric. The q-value threshold for significance, used for color scaling.
#'   Must be a positive number.
#' @param output_dir Character. Optional. Directory path where the plot should be saved.
#'   If NULL (default), the plot is not saved to a file.
#' @param file_name Character. Optional. Name of the file to save the plot, including extension.
#'   Default is "consensus_maaslin_plot_top_features.png".
#'
#' @return A ggplot2 object representing the consensus MaAsLin plot.
#'
#' @details
#' The function creates a plot with points and error bars representing the beta coefficients
#' and their confidence intervals. MaAsLin2 results are shown in blue, and MaAsLin3 results
#' in red. The color intensity of each point represents the q-value, with darker colors
#' indicating lower (more significant) q-values.
#'
#' @import ggplot2
#' @import dplyr
#' @import ggnewscale
#'
#' @examples
#' # Assuming df_maas_plot is your processed data frame
#' plot <- plot_consensus_maaslin(
#'   df_maas_plot_ = df_maas_plot,
#'   qthresh_ = 0.05,
#'   output_dir = "results",
#'   file_name = "my_consensus_plot.png"
#' )
#'
#' # Display the plot
#' print(plot)
#'
#' @export
plot_consensus_maaslin <- function(df_maas_plot_, qthresh_ , output_dir = NULL, file_name =  "consensus_maaslin_plot_top_features.png") {

    # Check if required columns are present in the dataframe
    required_cols <- c("labelll", "coef", "type", "lower", "upper", "qval")
    missing_cols <- setdiff(required_cols, colnames(df_maas_plot_))

    if (length(missing_cols) > 0) {
      stop("The following required columns are missing from df_maas_plot_: ", paste(missing_cols, collapse = ", "))
    }

    # Ensure qthresh_ is numeric and positive
    if (!is.numeric(qthresh_) || qthresh_ <= 0) {
      stop("qthresh_ must be a positive numeric value.")
    }

    # Plot the consensus differential abundances
    plot <- ggplot2::ggplot(df_maas_plot_, ggplot2::aes(y = reorder(labelll, coef))) +
      # Layer for x
      ggplot2::geom_point(data = dplyr::filter(df_maas_plot_, type == "x"),
                          ggplot2::aes(x = coef, color = qval, shape = type), alpha = 0.7) +
      ggplot2::geom_errorbarh(data = dplyr::filter(df_maas_plot_, type == "x"),
                              ggplot2::aes(xmin = lower, xmax = upper, color = qval), height = 0.2) +
      ggplot2::scale_color_gradient(low = "darkblue", high = "lightblue", na.value = "lightblue", guide = ggplot2::guide_colorbar(title = "Q-Value Maaslin2"),
                                    breaks=c(qthresh_/100,qthresh_/5,qthresh_*1), limits=c(0, qthresh_)) + #, trans = "log"

      ggnewscale::new_scale_color() +  # Reset color scale

      # Layer for y
      ggplot2::geom_point(data = dplyr::filter(df_maas_plot_, type == "y"),
                          ggplot2::aes(x = coef, color = qval, shape = type), alpha = 0.7) +
      ggplot2::geom_errorbarh(data = dplyr::filter(df_maas_plot_, type == "y"),
                              ggplot2::aes(xmin = lower, xmax = upper, color = qval), height = 0.2) +
      ggplot2::scale_color_gradient(low = "darkred", high = "pink", na.value = "pink", guide = ggplot2::guide_colorbar(title = "Q-Value Maaslin3"),
                                    breaks=c(qthresh_/100,qthresh_/5,qthresh_*1), limits=c(0, qthresh_*1)) + #, trans = "log"

      # Common shape scale and labels
      ggplot2::scale_shape_manual(values = c("x" = 16, "y" = 17), labels = c("Maaslin2", "Maaslin3")) +
      ggplot2::labs(x = "Beta-Coefficient", y = "Feature", shape = "Tool") +

      # Common theme adjustments
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(hjust = 1),
        plot.margin = ggplot2::margin(20, 10, 10, 5.5),
        legend.position = "right",
        base_size = 25
      )

    # Save the result to the specified path if output_dir is provided
    if (!is.null(output_dir)) {
        output_dir <- ensure_output_dir(output_dir)
        file_path <- file.path(output_dir, file_name)
        ggplot2::ggsave(file_path, plot, width = 10, height = 8, dpi = 300)
        message("Plot saved to: ", file_path)
    }

    # Return the plot
    return(plot)
}
