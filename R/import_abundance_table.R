#' Import and Validate Relative Abundance Table
#'
#' This function imports a relative abundance table from various file formats (CSV, TSV, or Excel),
#' and then validates and preprocesses the data by checking for zero-abundance rows, ensuring proper
#' orientation of the table (samples as columns and species as rows), and normalizing the data if needed.
#'
#' @param file_path A character string specifying the path to the file to be imported.
#' @param file_type A character string specifying the type of file to import. Options are "csv", "tsv",
#'   or "excel". Default is "csv".
#' @param sheet An integer specifying the sheet number to import from an Excel file. Default is 1.
#' @param delimiter A character string specifying the delimiter used in CSV or TSV files. Default is NULL,
#'   which uses a comma for CSV files and a tab for TSV files.
#' @param header A logical value indicating whether the file contains a header row. Default is TRUE.
#' @param rownames_column An integer specifying which column to use as row names, if any. Default is 1.
#' @param rowtype A character string specifying the type of data in rows (e.g., "species", "OTU" or "samples").
#'
#' @return A data frame of the imported and validated relative abundance table.
#'
#' @import readxl
#' @importFrom dplyr %>%
#'
#' @export
import_abundance_table <- function(file_path, file_type = c("csv", "tsv", "excel"),
                                   sheet = 1, delimiter = NULL, header = TRUE, rownames_column = 1, rowtype = "species") {

    # Ensure file_type is correctly matched
    file_type <- match.arg(file_type)


    # Read the file based on its type
    if (file_type == "csv") {
        if (is.null(delimiter)) delimiter <- ","
        df <- read.csv(file_path, sep = delimiter, header = header, row.names = rownames_column)
    } else if (file_type == "tsv") {
        if (is.null(delimiter)) delimiter <- "\t"
        df <- read.csv(file_path, sep = delimiter, header = header, row.names = rownames_column)
    } else if (file_type == "excel") {
        if (!requireNamespace("readxl", quietly = TRUE)) {
            stop("The 'readxl' package is required to read Excel files. Please install it.")
        }
        df <- readxl::read_excel(file_path, sheet = sheet)
        if (!is.null(rownames_column)) {
            rownames(df) <- df[[rownames_column]]
            df <- df[,-rownames_column]
        }
    } else {
        stop("Unsupported file type. Please choose from 'csv', 'tsv', or 'excel'.")
    }


    # Pass the data frame to the validation function
    validated_df <- validate_abund_table(df, rowtype)

    return(validated_df)
}
