library(testthat)

test_that("annotaR() creates a valid object", {
  genes <- c("TP53", "EGFR", "BRCA1")
  result <- annotaR(genes)

  # Test 1: Output is a tibble
  expect_s3_class(result, "tbl_df")

  # Test 2: Output has the correct column
  expect_equal(colnames(result), "gene")

  # Test 3: Output contains the correct data
  expect_equal(result$gene, genes)
})

test_that("annotaR() handles duplicates", {
  genes <- c("TP53", "EGFR", "TP53")
  result <- annotaR(genes)

  # Test: Duplicates are removed
  expect_equal(nrow(result), 2)
  expect_equal(result$gene, c("TP53", "EGFR"))
})

test_that("annotaR() validates input", {
  # Test: Throws error for non-character input
  expect_error(annotaR(123), "Input 'genes' must be a non-empty character vector")

  # Test: Throws error for empty vector
  expect_error(annotaR(c()), "Input 'genes' must be a non-empty character vector")

  # Test: Throws error for NULL input
  expect_error(annotaR(NULL), "Input 'genes' must be a non-empty character vector")
})
