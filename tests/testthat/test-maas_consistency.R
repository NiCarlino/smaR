library(testthat)
library(smaR)  

test_that("maas_consistency works correctly", {
    # Create sample data frames
    df_maas2 <- data.frame(
        feature = c("A", "B", "C", "D"),
        coef = c(0.5, -0.3, 0.2, -0.1),
        stderr = c(0.1, 0.05, 0.08, 0.03),
        qval = c(0.01, 0.02, 0.03, 0.04)
    )
    
    df_maas3 <- data.frame(
        feature = c("A", "B", "C", "E"),
        coef = c(0.6, -0.4, -0.1, 0.3),
        stderr = c(0.12, 0.06, 0.07, 0.09),
        qval = c(0.015, 0.025, 0.035, 0.045)
    )
    
    # Test basic functionality
    result <- maas_consistency(df_maas2, df_maas3, 3, "feature", 
                               c("coef.x", "coef.y"), 
                               c("stderr.x", "stderr.y"), 
                               c("qval.x", "qval.y"))
    
    expect_type(result, "list")
    expect_length(result, 3)
    expect_named(result, c("df_maas_", "df_maas_top_", "df_maas_plot_"))
    
    # Check df_maas_
    expect_equal(nrow(result$df_maas_), 2)  # Only A and B should remain
    expect_true(all(c("coef.x", "coef.y", "stderr.x", "stderr.y", "qval.x", "qval.y") %in% colnames(result$df_maas_)))
    
    # Check df_maas_top_
    expect_equal(nrow(result$df_maas_top_), 2)
    expect_true(all(c("coef.x", "coef.y", "stderr.x", "stderr.y", "qval.x", "qval.y") %in% colnames(result$df_maas_top_)))
    
    # Check df_maas_plot_
    expect_equal(nrow(result$df_maas_plot_), 4)  # 2 features * 2 types (x and y)
    expect_true(all(c("coef", "stderr", "qval", "lower", "upper", "labelll") %in% colnames(result$df_maas_plot_)))
})

test_that("maas_consistency handles edge cases", {
    # Test with empty data frames
    empty_df <- data.frame(feature = character(), coef = numeric(), stderr = numeric(), qval = numeric())
    expect_error(maas_consistency(empty_df, empty_df, 3, "feature", 
                                  c("coef.x", "coef.y"), 
                                  c("stderr.x", "stderr.y"), 
                                  c("qval.x", "qval.y")),
                 "No common features found between Maaslin2 and Maaslin3 that meet the criteria.")
    
    # Test with no common features
    df1 <- data.frame(feature = c("A", "B"), coef = c(0.5, -0.3), stderr = c(0.1, 0.05), qval = c(0.01, 0.02))
    df2 <- data.frame(feature = c("C", "D"), coef = c(0.6, -0.4), stderr = c(0.12, 0.06), qval = c(0.015, 0.025))
    expect_error(maas_consistency(df1, df2, 3, "feature", 
                                  c("coef.x", "coef.y"), 
                                  c("stderr.x", "stderr.y"), 
                                  c("qval.x", "qval.y")),
                 "No common features found between Maaslin2 and Maaslin3 that meet the criteria.")
    
    # Test with fewer features than requested
    df1 <- data.frame(feature = c("A", "B"), coef = c(0.5, -0.3), stderr = c(0.1, 0.05), qval = c(0.01, 0.02))
    df2 <- data.frame(feature = c("A", "B"), coef = c(0.6, -0.4), stderr = c(0.12, 0.06), qval = c(0.015, 0.025))
    result <- maas_consistency(df1, df2, 5, "feature", 
                               c("coef.x", "coef.y"), 
                               c("stderr.x", "stderr.y"), 
                               c("qval.x", "qval.y"))
    expect_equal(nrow(result$df_maas_top_), 2)
})

test_that("maas_consistency handles errors correctly", {
    df <- data.frame(feature = c("A", "B"), coef = c(0.5, -0.3), stderr = c(0.1, 0.05), qval = c(0.01, 0.02))
    
    # Non-data frame input
    expect_error(maas_consistency(list(), df, 3, "feature", 
                                  c("coef.x", "coef.y"), 
                                  c("stderr.x", "stderr.y"), 
                                  c("qval.x", "qval.y")),
                 "df_maas2_ and df_maas3_ must be data frames")
    
    # Invalid top_features_
    expect_error(maas_consistency(df, df, -1, "feature", 
                                  c("coef.x", "coef.y"), 
                                  c("stderr.x", "stderr.y"), 
                                  c("qval.x", "qval.y")),
                 "top_features_ must be a positive integer")
    
    # Invalid column names
    expect_error(maas_consistency(df, df, 3, 1, 
                                  c("coef.x", "coef.y"), 
                                  c("stderr.x", "stderr.y"), 
                                  c("qval.x", "qval.y")),
                 "key_cols_, coef_cols_, stderr_cols_, and qval_cols_ must be character vectors")
})