#' Plot GO Enrichment Results as a Dot Plot
#'
#' Creates a publication-ready dot plot from the results of an
#' `add_go_terms()` call. The plot shows the top enriched terms, with dot
#' size representing the number of genes and color representing the p-value.
#'
#' @param annotaR_object An object processed by `add_go_terms()`. Must contain
#'   `term_name`, `p_value`, and `gene` columns.
#' @param n_terms The maximum number of top terms to display, ordered by p-value.
#'   Defaults to 20.
#' @param title The title of the plot.
#'
#' @return A ggplot object.
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_point scale_color_viridis_c labs theme_bw theme element_text
#' @importFrom dplyr filter distinct group_by summarise n top_n ungroup
#' @importFrom stats reorder
#'
#' @examples
#' \dontrun{
#'   # Assuming 'annotated_data' is the result of add_go_terms()
#'   plot_enrichment_dotplot(annotated_data)
#' }
plot_enrichment_dotplot <- function(annotaR_object, n_terms = 20, title = "Top GO Enrichment Results") {
  
  # --- Input validation ---
  required_cols <- c("gene", "term_name", "p_value")
  if (!all(required_cols %in% names(annotaR_object))) {
    stop(paste("Input object must contain the following columns:", paste(required_cols, collapse = ", ")))
  }
  
  # --- Data preparation ---
  plot_data <- annotaR_object %>%
    dplyr::filter(!is.na(term_name)) %>%
    dplyr::distinct(gene, term_name, p_value) %>%
    dplyr::group_by(term_name, p_value) %>%
    dplyr::summarise(gene_count = dplyr::n(), .groups = 'drop') %>%
    dplyr::top_n(-n_terms, wt = p_value) # Select top N terms by smallest p-value

  if (nrow(plot_data) == 0) {
    warning("No enrichment data to plot.")
    return(ggplot2::ggplot() + ggplot2::theme_void())
  }
  
  # --- Create plot ---
  p <- ggplot2::ggplot(plot_data,
                  ggplot2::aes(x = gene_count,
                               y = stats::reorder(term_name, gene_count))) +
    ggplot2::geom_point(ggplot2::aes(color = -log10(p_value), size = gene_count)) +
    ggplot2::scale_color_viridis_c(name = "-log10(p-value)") +
    ggplot2::labs(
      title = title,
      x = "Gene Count",
      y = "Enriched Term",
      size = "Gene Count"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 10)
    )
  
  return(p)
}
