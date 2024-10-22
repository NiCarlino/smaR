% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_abundance_table.R
\name{import_abundance_table}
\alias{import_abundance_table}
\title{Import and Validate Relative Abundance Table}
\usage{
import_abundance_table(
  file_path,
  file_type = c("csv", "tsv", "excel"),
  sheet = 1,
  delimiter = NULL,
  header = TRUE,
  rownames_column = 1,
  rowtype = "species"
)
}
\arguments{
\item{file_path}{A character string specifying the path to the file to be imported.}

\item{file_type}{A character string specifying the type of file to import. Options are "csv", "tsv",
or "excel". Default is "csv".}

\item{sheet}{An integer specifying the sheet number to import from an Excel file. Default is 1.}

\item{delimiter}{A character string specifying the delimiter used in CSV or TSV files. Default is NULL,
which uses a comma for CSV files and a tab for TSV files.}

\item{header}{A logical value indicating whether the file contains a header row. Default is TRUE.}

\item{rownames_column}{An integer specifying which column to use as row names, if any. Default is 1.}

\item{rowtype}{A character string specifying the type of data in rows (e.g., "species", "OTU" or "samples").}
}
\value{
A data frame of the imported and validated relative abundance table.
}
\description{
This function imports a relative abundance table from various file formats (CSV, TSV, or Excel),
and then validates and preprocesses the data by checking for zero-abundance rows, ensuring proper
orientation of the table (samples as columns and species as rows), and normalizing the data if needed.
}
