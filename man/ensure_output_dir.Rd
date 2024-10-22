% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ensure_output_dir.R
\name{ensure_output_dir}
\alias{ensure_output_dir}
\title{Ensure Output Directory Exists}
\usage{
ensure_output_dir(output_dir)
}
\arguments{
\item{output_dir}{A character string specifying the path to the directory.}
}
\value{
The directory path (created if it didn't exist)
}
\description{
This function checks if the specified directory exists. If it does not exist,
the function creates it, including any necessary parent directories.
}
