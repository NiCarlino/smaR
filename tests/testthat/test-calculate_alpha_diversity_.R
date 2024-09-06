library(testthat)
library(smaR)

test_that("calculate_alpha_diversity works correctly", {
    # Create a sample abundance table
    abund_table <- data.frame(
        Sample1 = c(10, 20, 30, 0),
        Sample2 = c(5, 15, 25, 5),
        Sample3 = c(1, 2, 3, 4)
    )
    rownames(abund_table) <- c("OTU1", "OTU2", "OTU3", "OTU4")
    
    # Test with default parameters
    result <- calculate_alpha_diversity(abund_table)
    
    # Check the structure of the result
    expect_is(result, "data.frame")
    expect_equal(nrow(result), ncol(abund_table))
    expect_equal(colnames(result), "shannon")
    
    # Check if values are within expected range for Shannon index
    expect_true(all(result$shannon >= 0))
    expect_true(all(result$shannon <= log(nrow(abund_table))))
    
    # Test with Simpson index
    result_simpson <- calculate_alpha_diversity(abund_table, metric = "simpson")
    
    expect_equal(colnames(result_simpson), "simpson")
    expect_true(all(result_simpson$simpson >= 0))
    expect_true(all(result_simpson$simpson <= 1))
    
    # Test file output
    temp_dir <- tempdir()
    output_file <- file.path(temp_dir, "test_alpha_diversity.csv")
    
    calculate_alpha_diversity(abund_table, output_dir = temp_dir, file_name = "test_alpha_diversity.csv")
    
    expect_true(file.exists(output_file))
    
    # Read the file and check its contents
    file_contents <- read.csv(output_file, row.names = 1)
    expect_equal(nrow(file_contents), ncol(abund_table))
    expect_equal(colnames(file_contents), "shannon")
    
    # Clean up
    unlink(output_file)
    
    # Test error handling
    expect_error(calculate_alpha_diversity(abund_table, metric = "invalid_metric"),
                 "Invalid metric specified")
    
    # Test with empty abundance table
    expect_error(calculate_alpha_diversity(data.frame()),
                 "Abundance table is empty")
    
    # Test with non-numeric abundance table
    invalid_table <- data.frame(Sample1 = c("a", "b", "c"))
    expect_error(calculate_alpha_diversity(invalid_table),
                 "Abundance table contains non-numeric values")
})