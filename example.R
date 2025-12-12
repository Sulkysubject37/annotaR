# example.R - A script to demonstrate the annotaR package workflow

# --- Create Output Directory ---
dir.create("results", showWarnings = FALSE)

# --- Load Package and Dependencies ---
# In a real scenario, you would just do `library(annotaR)`.
# For development, we load functions and dependencies manually.
devtools::load_all()
library(ggplot2)

# --- Define Input Genes ---
# A small list of well-known genes involved in cancer
genes_of_interest <- c(
  "TP53", "EGFR", "BRCA1", "BRCA2", "KRAS", "PIK3CA", "AKT1", "BRAF",
  "MYC", "ERBB2", "CDKN2A", "PTEN"
)

# --- Run Annotation Pipeline ---
# 1. Create the initial object
annotaR_obj <- annotaR(genes_of_interest)

# 2. Add functional enrichment data (GO terms)
# This step calls the g:Profiler API and may take a moment
message("Fetching GO term annotations from g:Profiler...")
annotaR_obj_go <- add_go_terms(annotaR_obj, sources = c("GO:BP"))

# 3. Add disease links from OpenTargets
message("Fetching disease associations from OpenTargets...")
annotaR_obj_disease <- add_disease_links(annotaR_obj_go)

# 4. Add known drug links from OpenTargets
message("Fetching known drug associations from OpenTargets...")
full_annotation <- add_drug_links(annotaR_obj_disease)

# --- Explore the Results ---
message("Annotation pipeline complete. Here is a summary of the results:")
print(full_annotation)

# --- Generate Visualization ---
message("Generating enrichment dot plot...")
enrichment_plot <- plot_enrichment_dotplot(
  annotaR_obj_go,
  n_terms = 25,
  title = "Top 25 Enriched GO Biological Processes"
)

# --- Save Plot ---
output_file <- "results/enrichment_dot_plot.png"
ggsave(
  output_file,
  plot = enrichment_plot,
  width = 10,
  height = 8,
  dpi = 300
)

message(paste("Plot saved to:", output_file))
