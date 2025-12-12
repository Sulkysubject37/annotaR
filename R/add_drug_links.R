#' Add known drug association data
#'
#' Augments an annotaR object with known drug/compound data from the
#' OpenTargets platform. This includes the drug name, type, mechanism of
#' action, and clinical trial phase.
#'
#' @param annotaR_object A tibble, typically from `annotaR()`, containing a
#'   'gene' column with HGNC symbols.
#'
#' @return A new tibble with the original data joined with drug association
#'   columns (e.g., drug_name, drug_type, mechanism_of_action, phase).
#' @export
#'
#' @importFrom httr POST add_headers content
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom dplyr select left_join rename
#' @importFrom purrr map_dfr
#' @importFrom biomaRt getBM useEnsembl
#' @importFrom tibble tibble
#'
#' @examples
#' \dontrun{
#'   annotaR(c("EGFR", "BRAF")) %>%
#'     add_drug_links()
#' }
add_drug_links <- function(annotaR_object) {
  if (!"gene" %in% names(annotaR_object)) {
    stop("Input must be an annotaR object with a 'gene' column.")
  }

  genes <- unique(annotaR_object$gene)

  # Get Ensembl IDs
  ensembl_ids <- biomaRt::getBM(
    attributes = c("hgnc_symbol", "ensembl_gene_id"),
    filters = "hgnc_symbol",
    values = genes,
    mart = biomaRt::useEnsembl("genes", "hsapiens_gene_ensembl")
  )

  if (nrow(ensembl_ids) == 0) {
    warning("Could not map any provided gene symbols to Ensembl IDs.")
    return(annotaR_object)
  }

  query_template <- '
  query TargetKnownDrugs($ensemblId: String!) {
    target(ensemblId: $ensemblId) {
      knownDrugs {
        rows {
          approvedName
          drugType
          mechanismOfAction
          phase
        }
      }
    }
  }'

  results <- purrr::map_dfr(ensembl_ids$ensembl_gene_id, function(ens_id) {
    variables <- list(ensemblId = ens_id)
    body <- list(query = query_template, variables = variables)

    res <- httr::POST(
      url = "https://api.platform.opentargets.org/api/v4/graphql",
      httr::add_headers("Content-Type" = "application/json"),
      body = jsonlite::toJSON(body, auto_unbox = TRUE)
    )

    rows <- tryCatch(
      jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"))$data$target$knownDrugs$rows,
      error = function(e) NULL
    )

    if (is.null(rows) || length(rows) == 0 || nrow(rows) == 0) return(NULL)

    tibble::tibble(
      ensembl_gene_id = ens_id,
      drug_name = rows$approvedName,
      drug_type = rows$drugType,
      mechanism_of_action = rows$mechanismOfAction,
      phase = rows$phase
    )
  })

  if (nrow(results) == 0) {
    warning("No known drugs found for the given genes.")
    return(annotaR_object)
  }

  # Join back with original symbols and the input object
  drug_data <- results %>%
    dplyr::left_join(ensembl_ids, by = "ensembl_gene_id") %>%
    dplyr::select(gene = hgnc_symbol, drug_name, drug_type, mechanism_of_action, phase)

  dplyr::left_join(annotaR_object, drug_data, by = "gene")
}
