library(testthat)
library(smaR)

test_that("convert_columns_to_factors works correctly", {
    # Create a sample data frame
    df <- data.frame(
        A = c("a", "b", "c", "a"),
        B = 1:4,
        C = c("x", "y", "z", "x"),
        D = as.factor(c("m", "n", "o", "m")),
        stringsAsFactors = FALSE
    )
    
    # Test converting a single character column
    result1 <- convert_columns_to_factors(df, "A")
    expect_true(is.factor(result1$A))
    expect_false(is.factor(result1$C))
    expect_equal(levels(result1$A), c("a", "b", "c"))
    
    # Test converting multiple columns
    result2 <- convert_columns_to_factors(df, c("A", "C"))
    expect_true(is.factor(result2$A))
    expect_true(is.factor(result2$C))
    expect_false(is.factor(result2$B))
    
    # Test that numeric columns are not converted
    result3 <- convert_columns_to_factors(df, c("A", "B"))
    expect_true(is.factor(result3$A))
    expect_true(is.numeric(result3$B))
    
    # Test that existing factor columns are not changed
    result4 <- convert_columns_to_factors(df, c("A", "D"))
    expect_true(is.factor(result4$A))
    expect_true(is.factor(result4$D))
    expect_equal(levels(result4$D), levels(df$D))
    
    # Test with all columns
    result5 <- convert_columns_to_factors(df, colnames(df))
    expect_true(all(sapply(result5[c("A", "C", "D")], is.factor)))
    expect_true(is.numeric(result5$B))
    
    # Test that the function doesn't modify the original dataframe
    convert_columns_to_factors(df, "A")
    expect_false(is.factor(df$A))
})

test_that("convert_columns_to_factors handles edge cases", {
    # Empty data frame
    empty_df <- data.frame()
    expect_equal(convert_columns_to_factors(empty_df, character(0)), empty_df)
    
    # Data frame with no character columns
    numeric_df <- data.frame(A = 1:3, B = 4:6)
    expect_equal(convert_columns_to_factors(numeric_df, c("A", "B")), numeric_df)
    
    # Data frame with only factor columns
    factor_df <- data.frame(A = factor(c("a", "b", "c")), B = factor(c("x", "y", "z")))
    expect_equal(convert_columns_to_factors(factor_df, c("A", "B")), factor_df)
    
    # Converting no columns
    expect_equal(convert_columns_to_factors(df, character(0)), df)
})

test_that("convert_columns_to_factors handles errors correctly", {
    df <- data.frame(A = c("a", "b", "c"), B = 1:3, stringsAsFactors = FALSE)
    
    # Non-data frame input
    expect_error(convert_columns_to_factors(list(A = 1:3), "A"), "Input df must be a data frame")
    
    # Non-existent column
    expect_error(convert_columns_to_factors(df, "C"), "All specified columns must exist in the data frame")
    
    # Mix of existing and non-existing columns
    expect_error(convert_columns_to_factors(df, c("A", "C")), "All specified columns must exist in the data frame")
    
    # NULL input
    expect_error(convert_columns_to_factors(NULL, "A"), "Input df must be a data frame")
})