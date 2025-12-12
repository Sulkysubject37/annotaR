#' Create an annotaR object
#'
#' Initializes the annotation pipeline by creating a tibble from a character
#' vector of gene symbols. This is the entry point for a typical annotaR
#' workflow.
#'
#' @param genes A character vector of HGNC gene symbols (e.g., c("TP53", "BRCA1")).
#'
#' @return A tibble with a single column 'gene', ready to be used in
#'   downstream annotation functions.
#' @export
#'
#' @examples
#' my_genes <- c("TP53", "EGFR", "BRCA1")
#' annotaR(my_genes)
annotaR <- function(genes) {
  if (!is.character(genes) || length(genes) == 0) {
    stop("Input 'genes' must be a non-empty character vector of gene symbols.")
  }

  tibble::tibble(gene = unique(genes))
}
