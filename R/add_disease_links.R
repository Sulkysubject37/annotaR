#' Add disease association data
#'
#' Augments an annotaR object with disease association data from the
#' OpenTargets platform.
#'
#' @param annotaR_object A tibble, typically from `annotaR()`, containing a
#'   'gene' column with HGNC symbols.
#' @param score_threshold Minimum association score (from 0 to 1) to include.
#'   Defaults to 0.5.
#'
#' @return A new tibble with the original data joined with disease association
#'   columns (disease_name, association_score).
#' @export
#'
#' @importFrom httr POST add_headers content
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom dplyr select filter left_join
#' @importFrom purrr map_dfr
#' @importFrom biomaRt getBM useEnsembl
#'
#' @examples
#' \dontrun{
#'   annotaR(c("TP53", "EGFR")) %>%
#'     add_disease_links(score_threshold = 0.8)
#' }
add_disease_links <- function(annotaR_object, score_threshold = 0.5) {
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
    warning("Could not map any of the provided gene symbols to Ensembl IDs.")
    return(annotaR_object)
  }

  query_template <- '
  query TargetDiseases($ensemblId: String!) {
    target(ensemblId: $ensemblId) {
      associatedDiseases {
        rows {
          disease { name }
          score
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
      jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"))$data$target$associatedDiseases$rows,
      error = function(e) NULL
    )

    if (is.null(rows) || length(rows) == 0 || nrow(rows) == 0) return(NULL)

    tibble::tibble(
      ensembl_gene_id = ens_id,
      disease_name = rows$disease$name,
      association_score = rows$score
    )
  })

  if (nrow(results) == 0) {
    warning("No disease associations found for the given genes.")
    return(annotaR_object)
  }

  # Join back with original symbols and the input object
  disease_data <- results %>%
    dplyr::filter(association_score >= score_threshold) %>%
    dplyr::left_join(ensembl_ids, by = "ensembl_gene_id") %>%
    dplyr::select(gene = hgnc_symbol, disease_name, association_score)

  dplyr::left_join(annotaR_object, disease_data, by = "gene")
}
