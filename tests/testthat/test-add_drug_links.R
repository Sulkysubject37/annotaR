library(testthat)

# ---- Mock Functions ----

# Re-use the mock for biomaRt::getBM
mock_getBM <- function(attributes, filters, values, mart) {
  tibble::tibble(
    hgnc_symbol = "EGFR",
    ensembl_gene_id = "ENSG00000146648"
  )
}

# Mock for httr::content to return a canned JSON response for drugs
mock_content_drug <- function(res, as = "text", ...) {
  '{
    "data": {
      "target": {
        "knownDrugs": {
          "rows": [
            {
              "approvedName": "OSIMERTINIB",
              "drugType": "SMALL_MOLECULE",
              "mechanismOfAction": "EGFR (erbB1) inhibitor",
              "phase": 4
            },
            {
              "approvedName": "ERLOTINIB",
              "drugType": "SMALL_MOLECULE",
              "mechanismOfAction": "EGFR (erbB1) inhibitor",
              "phase": 4
            }
          ]
        }
      }
    }
  }'
}

# ---- Test Cases ----

test_that("add_drug_links() works with a successful API call", {
  with_mocked_bindings(getBM = mock_getBM, .package = "biomaRt", {
    with_mocked_bindings(
      POST = function(...) { structure(list(status_code = 200), class = "response") },
      content = mock_content_drug,
      .package = "httr",
    {
      input_obj <- annotaR("EGFR")
      result <- add_drug_links(input_obj)

      # Test: Correct columns are present
      expect_true(all(c("gene", "drug_name", "drug_type", "mechanism_of_action", "phase") %in% colnames(result)))

      # Test: Correct data is joined
      expect_equal(nrow(result), 2)
      expect_true("OSIMERTINIB" %in% result$drug_name)
      expect_equal(result$phase[1], 4)
    })
  })
})

test_that("add_drug_links() handles no known drugs found", {
  # Mock content to return an empty result
  mock_content_empty <- function(...) { '{"data":{"target":{"knownDrugs":{"rows":[]}}}}' }

  with_mocked_bindings(getBM = mock_getBM, .package = "biomaRt", {
    with_mocked_bindings(
      POST = function(...) { structure(list(status_code = 200), class = "response") },
      content = mock_content_empty,
      .package = "httr",
    {
      input_obj <- annotaR("EGFR")
      expect_warning(
        result <- add_drug_links(input_obj),
        "No known drugs found"
      )
      expect_equal(result, input_obj)
    })
  })
})
