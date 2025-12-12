# annotaR.R

#' Annotate genes with GO, KEGG, Reactome terms and disease relevance
#'
#' @param genes A character vector of HGNC gene symbols (e.g., "TP53", "BRCA1").
#' @param organism Organism code for g:Profiler (default: "hsapiens").
#' @param sources Functional annotation databases (default: GO, KEGG, REAC).
#' @param score_threshold Minimum disease relevance score (default: 0.3).
#' @param max_terms Max functional terms per gene to include (default: 10).
#'
#' @return A list with `functional` and `disease` tibbles.
#' @export
annotate_genes <- function(genes,
                           organism = "hsapiens",
                           sources = c("GO", "KEGG", "REAC"),
                           score_threshold = 0.3,
                           max_terms = 10) {
  if (!is.character(genes)) {
    cli::cli_abort("`genes` must be a character vector.")
  }
  
  # ---- Functional annotation via g:Profiler ----
  gprof_res <- gprofiler2::gost(
    query = genes,
    organism = organism,
    sources = sources,
    combine = TRUE
  )
  
  if (is.null(gprof_res$result) || nrow(gprof_res$result) == 0) {
    cli::cli_alert_warning("No enrichment found for the provided genes.")
    func_annot <- tibble::tibble()
  } else {
    func_annot <- gprof_res$result |>
      dplyr::select(
        gene_list = intersection,
        term_name,
        term_id,
        p_value,
        source
      ) |>
      tidyr::separate_rows(gene_list, sep = ",") |>
      dplyr::rename(gene = gene_list) |>
      dplyr::group_by(gene) |>
      dplyr::slice_head(n = max_terms) |>
      dplyr::ungroup()
  }
  
  # ---- Disease relevance annotation via OpenTargets ----
  disease_annot <- get_disease_links(genes, score_threshold)
  
  return(list(
    functional = func_annot,
    disease = disease_annot
  ))
}


#' Get disease associations for genes using OpenTargets API
#'
#' @param genes A character vector of HGNC gene symbols.
#' @param score_threshold Minimum association score to include.
#'
#' @return A tibble with gene, disease, and score.
#' @noRd
get_disease_links <- function(genes, score_threshold = 0.3) {
  if (!requireNamespace("httr") || !requireNamespace("jsonlite")) {
    cli::cli_abort("You must install {httr} and {jsonlite} packages.")
  }
  
  query_template <- '
  {
    target(ensemblId: "%s") {
      associatedDiseases {
        rows {
          disease { name }
          score
        }
      }
    }
  }
  '
  
  # Convert gene symbols → Ensembl IDs
  ensembl_ids <- biomaRt::getBM(
    attributes = c("hgnc_symbol", "ensembl_gene_id"),
    filters = "hgnc_symbol",
    values = genes,
    mart = biomaRt::useEnsembl("genes", "hsapiens_gene_ensembl")
  )
  
  results <- purrr::map_dfr(ensembl_ids$ensembl_gene_id, function(ens_id) {
    q <- sprintf(query_template, ens_id)
    
    res <- httr::POST(
      url = "https://api.platform.opentargets.org/api/v4/graphql",
      httr::add_headers("Content-Type" = "application/json"),
      body = jsonlite::toJSON(list(query = q), auto_unbox = TRUE)
    )
    
    parsed <- jsonlite::fromJSON(httr::content(res, as = "text"))
    
    rows <- parsed$data$target$associatedDiseases$rows
    
    if (length(rows) == 0) return(NULL)
    
    tibble::tibble(
      ensembl_gene_id = ens_id,
      disease = purrr::map_chr(rows, ~ .x$disease$name),
      score = purrr::map_dbl(rows, ~ .x$score)
    ) |>
      dplyr::filter(score >= score_threshold)
  })
  
  dplyr::left_join(results, ensembl_ids, by = "ensembl_gene_id") |>
    dplyr::select(gene = hgnc_symbol, disease, score)
}


#' Merge and summarize annotations into one tidy table
#'
#' @param annotation_result Output from `annotate_genes()`.
#'
#' @return A joined tibble with gene, term, and disease relevance.
#' @export
summarize_annotations <- function(annotation_result) {
  if (!is.list(annotation_result) ||
      !all(c("functional", "disease") %in% names(annotation_result))) {
    cli::cli_abort("Input must be the result of `annotate_genes()`.")
  }
  
  dplyr::full_join(
    annotation_result$functional,
    annotation_result$disease,
    by = "gene"
  ) |>
    dplyr::arrange(gene, dplyr::desc(score), p_value)
}

genes <- c("TP53", "BRCA1", "EGFR")
result <- annotate_genes(genes)
summary_tbl <- summarize_annotations(result)
print(summary_tbl)