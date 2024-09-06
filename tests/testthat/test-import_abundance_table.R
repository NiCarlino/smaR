library(testthat)
library(smaR)

test_that("import_abundance_table works correctly", {
    # Create temporary files for testing
    csv_file <- tempfile(fileext = ".csv")
    tsv_file <- tempfile(fileext = ".tsv")
    excel_file <- tempfile(fileext = ".xlsx")
    
    # Create sample data
    sample_data <- data.frame(
        Species = c("Species1", "Species2", "Species3"),
        Sample1 = c(10, 20, 30),
        Sample2 = c(5, 15, 25),
        Sample3 = c(1, 2, 3)
    )
    
    # Write sample data to files
    write.csv(sample_data, csv_file, row.names = FALSE)
    write.table(sample_data, tsv_file, sep = "\t", row.names = FALSE)
    if (requireNamespace("writexl", quietly = TRUE)) {
        writexl::write_xlsx(sample_data, excel_file)
    }
    
    # Test CSV import
    csv_result <- import_abundance_table(csv_file, file_type = "csv", rownames_column = 1)
    expect_is(csv_result, "data.frame")
    expect_equal(nrow(csv_result), 3)
    expect_equal(ncol(csv_result), 3)
    expect_equal(rownames(csv_result), c("Species1", "Species2", "Species3"))
    
    # Test TSV import
    tsv_result <- import_abundance_table(tsv_file, file_type = "tsv", rownames_column = 1)
    expect_is(tsv_result, "data.frame")
    expect_equal(nrow(tsv_result), 3)
    expect_equal(ncol(tsv_result), 3)
    expect_equal(rownames(tsv_result), c("Species1", "Species2", "Species3"))
    
    # Test Excel import (if readxl is available)
    if (requireNamespace("readxl", quietly = TRUE)) {
        excel_result <- import_abundance_table(excel_file, file_type = "excel", rownames_column = 1)
        expect_is(excel_result, "data.frame")
        expect_equal(nrow(excel_result), 3)
        expect_equal(ncol(excel_result), 3)
        expect_equal(rownames(excel_result), c("Species1", "Species2", "Species3"))
    } else {
        expect_error(import_abundance_table(excel_file, file_type = "excel"),
                     "The 'readxl' package is required to read Excel files. Please install it.")
    }
    
    # Test with custom delimiter
    write.table(sample_data, csv_file, sep = ";", row.names = FALSE)
    custom_delimiter_result <- import_abundance_table(csv_file, file_type = "csv", delimiter = ";", rownames_column = 1)
    expect_is(custom_delimiter_result, "data.frame")
    expect_equal(nrow(custom_delimiter_result), 3)
    expect_equal(ncol(custom_delimiter_result), 3)
    
    # Test with no header
    write.table(sample_data, csv_file, sep = ",", row.names = FALSE, col.names = FALSE)
    no_header_result <- import_abundance_table(csv_file, file_type = "csv", header = FALSE, rownames_column = 1)
    expect_is(no_header_result, "data.frame")
    expect_equal(nrow(no_header_result), 3)
    expect_equal(ncol(no_header_result), 3)
    
    # Test error handling
    expect_error(import_abundance_table("nonexistent_file.csv", file_type = "csv"),
                 "No such file or directory")
    expect_error(import_abundance_table(csv_file, file_type = "invalid_type"),
                 "should be one of")
    
    # Clean up temporary files
    unlink(c(csv_file, tsv_file, excel_file))
})

test_that("import_abundance_table handles edge cases", {
    # Test with empty file
    empty_file <- tempfile(fileext = ".csv")
    file.create(empty_file)
    expect_error(import_abundance_table(empty_file, file_type = "csv"),
                 "Empty data set")
    
    # Test with file containing only header
    header_only_file <- tempfile(fileext = ".csv")
    write.csv(data.frame(Species = character(), Sample1 = numeric(), Sample2 = numeric()),
              header_only_file, row.names = FALSE)
    expect_warning(result <- import_abundance_table(header_only_file, file_type = "csv", rownames_column = 1),
                   "Empty data set")
    expect_equal(nrow(result), 0)
    
    # Clean up
    unlink(c(empty_file, header_only_file))
})