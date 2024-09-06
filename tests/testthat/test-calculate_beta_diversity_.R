library(testthat)
library(smaR) 

test_that("calculate_beta_diversity works correctly", {
    # Create a sample abundance table
    abund_table <- data.frame(
        Sample1 = c(10, 20, 30, 0),
        Sample2 = c(5, 15, 25, 5),
        Sample3 = c(1, 2, 3, 4),
        Sample4 = c(0, 10, 20, 30)
    )
    rownames(abund_table) <- c("OTU1", "OTU2", "OTU3", "OTU4")
    
    # Test with default parameters (Bray-Curtis)
    result <- calculate_beta_diversity(abund_table)
    
    # Check the structure of the result
    expect_is(result, "matrix")
    expect_equal(nrow(result), ncol(abund_table))
    expect_equal(ncol(result), ncol(abund_table))
    expect_equal(rownames(result), colnames(abund_table))
    expect_equal(colnames(result), colnames(abund_table))
    
    # Check if values are within expected range for Bray-Curtis (0 to 1)
    expect_true(all(result >= 0 & result <= 1))
    
    # Check diagonal (should be all zeros)
    expect_equal(diag(result), rep(0, ncol(abund_table)))
    
    # Test with Jaccard index
    result_jaccard <- calculate_beta_diversity(abund_table, metric = "jaccard")
    
    expect_is(result_jaccard, "matrix")
    expect_true(all(result_jaccard >= 0 & result_jaccard <= 1))
    
    # Test file output
    temp_dir <- tempdir()
    output_file <- file.path(temp_dir, "test_beta_diversity.csv")
    
    calculate_beta_diversity(abund_table, output_dir = temp_dir, file_name = "test_beta_diversity.csv")
    
    expect_true(file.exists(output_file))
    
    # Read the file and check its contents
    file_contents <- as.matrix(read.csv(output_file, row.names = 1))
    expect_equal(dim(file_contents), c(ncol(abund_table), ncol(abund_table)))
    expect_equal(rownames(file_contents), colnames(abund_table))
    expect_equal(colnames(file_contents), colnames(abund_table))
    
    # Clean up
    unlink(output_file)
    
    # Test error handling
    expect_error(calculate_beta_diversity(abund_table, metric = "invalid_metric"),
                 "Invalid metric specified")
    
    # Test with empty abundance table
    expect_error(calculate_beta_diversity(data.frame()),
                 "Abundance table is empty")
    
    # Test with non-numeric abundance table
    invalid_table <- data.frame(Sample1 = c("a", "b", "c"), Sample2 = c("d", "e", "f"))
    expect_error(calculate_beta_diversity(invalid_table),
                 "Abundance table contains non-numeric values")
    
    # Test with single sample (should return an error or a 1x1 matrix)
    single_sample <- data.frame(Sample1 = c(1, 2, 3))
    expect_error(calculate_beta_diversity(single_sample),
                 "At least two samples are required for beta diversity calculation")
    
    # Test with samples that have all zero abundances
    zero_sample <- cbind(abund_table, Sample5 = c(0, 0, 0, 0))
    expect_warning(calculate_beta_diversity(zero_sample),
                   "Some samples have all zero abundances")
})