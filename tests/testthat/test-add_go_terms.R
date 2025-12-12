library(testthat)

# Create a mock function for gprofiler2::gost
mock_gost <- function(query, organism, sources, ...) {
  # A realistic but fake result structure
  if (is.null(query) || length(query) == 0) {
    return(NULL)
  }

  mock_result <- data.frame(
    term_id = "GO:0006915",
    term_name = "apoptotic process",
    p_value = 0.001,
    source = "GO:BP",
    intersection = I(list(c("TP53"))),
    stringsAsFactors = FALSE
  )

  # Create an object with a 'result' element like the real gprofiler2 object
  list(result = tibble::as_tibble(mock_result))
}


test_that("add_go_terms() works with a successful API call", {
  # Mock the gost function within the scope of this test
  with_mocked_bindings(
    gost = mock_gost,
    .package = "gprofiler2",
    {
      input_obj <- annotaR(c("TP53"))
      result <- add_go_terms(input_obj)

      # Test: Columns are added correctly
      expect_true(all(c("gene", "term_id", "term_name", "p_value") %in% colnames(result)))

      # Test: Data is joined correctly
      expect_equal(result$term_name, "apoptotic process")
      expect_equal(result$p_value, 0.001)
    }
  )
})

test_that("add_go_terms() handles no enrichment results", {
  # Mock a version of gost that returns an empty result
  mock_gost_empty <- function(...) {
    list(result = tibble::tibble())
  }

  with_mocked_bindings(
    gost = mock_gost_empty,
    .package = "gprofiler2",
    {
      input_obj <- annotaR(c("XYZ123")) # A gene with no results

      # Test: It should return the original object and give a warning
      expect_warning(
        result <- add_go_terms(input_obj),
        "No functional enrichment results found"
      )

      # Test: The returned object should be identical to the input
      expect_equal(result, input_obj)
    }
  )
})

test_that("add_go_terms() validates input", {
  bad_input <- data.frame(x = 1:5)
  expect_error(add_go_terms(bad_input), "Input must be an annotaR object")
})

