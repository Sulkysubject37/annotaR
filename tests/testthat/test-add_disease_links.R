library(testthat)

# ---- Mock Functions ----

# Mock for biomaRt::getBM
mock_getBM <- function(attributes, filters, values, mart) {
  tibble::tibble(
    hgnc_symbol = "EGFR",
    ensembl_gene_id = "ENSG00000146648"
  )
}

# Mock for httr::content to return a canned JSON response
mock_content_disease <- function(res, as = "text", ...) {
  '{
    "data": {
      "target": {
        "associatedDiseases": {
          "rows": [
            { "disease": { "name": "lung adenocarcinoma" }, "score": 0.98 },
            { "disease": { "name": "cancer" }, "score": 0.92 }
          ]
        }
      }
    }
  }'
}

# ---- Test Cases ----

test_that("add_disease_links() works with a successful API call", {
  with_mocked_bindings(getBM = mock_getBM, .package = "biomaRt", {
    with_mocked_bindings(
      # POST just needs to return something to not fail, the real action is in `content`
      POST = function(...) { structure(list(status_code = 200), class = "response") },
      content = mock_content_disease,
      .package = "httr",
    {
      input_obj <- annotaR("EGFR")
      result <- add_disease_links(input_obj, score_threshold = 0.9)

      # Test: Correct columns are present
      expect_true(all(c("gene", "disease_name", "association_score") %in% colnames(result)))

      # Test: Correct data is joined
      expect_equal(nrow(result), 2)
      expect_true("lung adenocarcinoma" %in% result$disease_name)
      expect_true(all(result$association_score >= 0.9))
    })
  })
})

test_that("add_disease_links() handles no Ensembl ID mapping", {
  # Mock getBM to return an empty data frame
  mock_getBM_empty <- function(...) {
    tibble::tibble(hgnc_symbol = character(), ensembl_gene_id = character())
  }

  with_mocked_bindings(
    getBM = mock_getBM_empty, .package = "biomaRt",
    {
      input_obj <- annotaR("UNKNOWN_GENE")
      expect_warning(
        result <- add_disease_links(input_obj),
        "Could not map any of the provided gene symbols"
      )
      expect_equal(result, input_obj)
    }
  )
})

test_that("add_disease_links() handles no disease associations found", {
  # Mock POST to return an empty result via the content function
  mock_content_empty <- function(...) { '{"data":{"target":null}}' }

  with_mocked_bindings(getBM = mock_getBM, .package = "biomaRt", {
    with_mocked_bindings(
      POST = function(...) { structure(list(status_code = 200), class = "response") },
      content = mock_content_empty,
      .package = "httr",
    {
      input_obj <- annotaR("EGFR")
      expect_warning(
        result <- add_disease_links(input_obj),
        "No disease associations found"
      )
      expect_equal(result, input_obj)
    })
  })
})
