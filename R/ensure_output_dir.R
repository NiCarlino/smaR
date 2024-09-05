#' Ensure Output Directory Exists
#'
#' This function checks if the specified directory exists. If it does not exist,
#' the function creates it, including any necessary parent directories.
#'
#' @param output_dir The directory path
#' @param output_dir A character string specifying the path to the directory.
#'
#' @return The directory path (created if it didn't exist)
#'
#' @importFrom assertthat assert_that
#'
#' @export
ensure_output_dir <- function(output_dir) {

    assertthat::assert_that(is.character(output_dir), length(output_dir) == 1,
              msg = "output_dir must be a single character string")

    output_dir <- normalizePath(output_dir, mustWork = FALSE)

    # Check if the directory exists and create it if it does not
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
        if (!dir.exists(output_dir)) {
            stop("Failed to create directory: ", output_dir)
        }
        message(paste("Directory", output_dir, "created."))
    }
    return(output_dir)
}
