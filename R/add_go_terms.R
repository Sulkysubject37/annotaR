#' Add GO functional enrichment data
#'
#' Augments an annotaR object with functional enrichment data from g:Profiler.
#' It performs a Gene Ontology (GO) analysis on the gene list and joins the
#' results.
#'
#' @param annotaR_object A tibble, typically the output of `annotaR()`. Must
#'   contain a 'gene' column.
#' @param organism The organism name to use for the query (e.g., "hsapiens").
#'   Passed to `gprofiler2::gost`.
#' @param sources A vector of data sources to query. Defaults to GO Biological
#'   Process. See `gprofiler2::gost` for options.
#' @param ... Additional parameters passed on to `gprofiler2::gost`.
#'
#' @return A new tibble with the original 'gene' column joined with functional
#'   annotation columns (e.g., term_id, term_name, p_value, source).
#' @export
#'
#' @importFrom gprofiler2 gost
#' @importFrom dplyr select left_join rename
#' @importFrom tidyr separate_rows
#'
#' @examples
#' \dontrun{
#'   annotaR(c("TP53", "EGFR")) %>%
#'     add_go_terms()
#' }
add_go_terms <- function(annotaR_object, organism = "hsapiens", sources = c("GO:BP"), ...) {
  if (!"gene" %in% names(annotaR_object)) {
    stop("Input must be an annotaR object with a 'gene' column.")
  }

  gene_list <- annotaR_object$gene

  gprof_res <- gprofiler2::gost(
    query = gene_list,
    organism = organism,
    sources = sources,
    evcodes = TRUE,
    ...
  )

  if (is.null(gprof_res) || nrow(gprof_res$result) == 0) {
    warning("No functional enrichment results found for the given genes.")
    return(annotaR_object)
  }

  # Process and join results
  processed_results <- gprof_res$result %>%
    dplyr::select(term_id, term_name, p_value, source, intersection) %>%
    tidyr::separate_rows(intersection, sep = ",") %>%
    dplyr::rename(gene = intersection)

  dplyr::left_join(annotaR_object, processed_results, by = "gene")
}
