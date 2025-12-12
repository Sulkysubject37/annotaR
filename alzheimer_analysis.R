# alzheimer_analysis.R - Analysis of differentially expressed genes in Alzheimer's Disease

# --- Load Package and Dependencies ---
devtools::load_all()
library(ggplot2)

# --- Define Input Genes (extracted from Arora et al., 2023, Supplementary Table 2) ---
alzheimer_genes <- c(
  "A4GALT", "ABCC12", "ABHD15", "ABHD4", "ABI3", "ACER2", "ACOX2", "ACTG1P1",
  "ACVRL1", "ADAMTS1", "ADAMTSL4", "ADGRG1", "AGTRAP", "AMOT", "ANKRD20A7P", "ANKRD26P3",
  "ANXA1", "AQP1", "ARHGAP30", "ARHGAP31", "ARID5A", "ARRDC4", "ATN1", "ATOH8",
  "ATP1B2", "ATP5F1AP10", "AURKA", "BCL3", "BCL6", "BCRP2", "BGN", "BMS1P4",
  "BOC", "BSG", "C1QA", "C1QC", "C2CD4C", "C3", "CAP1P1", "CASP4",
  "CASTOR1", "CASZ1", "CAVIN1", "CBFA2T3", "CBX2", "CCSER1", "CD14", "CD300A",
  "CD302", "CD44-AS1", "CD81", "CD99", "CD99P1", "CDC14A", "CDC42EP4", "CDH5",
  "CDKL5", "CEBPB", "CEBPD", "CENPB", "CFAP221", "CFD", "CHDH", "CHST3",
  "CKS1B", "CLEC14A", "CMAHP", "CMTM3", "CMTM6", "CMTM7", "CNN2", "CNTNAP2",
  "COL18A1", "COL4A1", "COL6A2", "COLEC12", "CPT1A", "CRB2", "CRIM1", "CRTAP",
  "CSF2RB", "CSPG4", "CYBA", "DCN", "DDR1", "DEDD2", "DENND2D", "DNAJC5G",
  "DOCK6", "DOK1", "DRICH1", "EBI3", "EFCAB14", "EFNB1", "EGFL7", "ELOVL7",
  "EMILIN1", "EMILIN2", "EPAS1", "EPHA2", "EPHB4", "ESAM", "ETNPPL", "ETS1",
  "EVC", "F11R", "FAM181A", "FAM43A", "FAM89A", "FBLN1", "FBLN2", "FCGRT",
  "FES", "FGD3", "FGD5", "FGFRL1", "FHL3", "FLI1", "FLT3", "FN1",
  "FOXO4", "FREM1", "FTL", "FXYD1", "GABRE", "GADD45B", "GAL3ST4", "GAPDHP66",
  "GAS1", "GAS2", "GFAP", "GIMAP1", "GIMAP6", "GIMAP8", "GJC1", "GNA12",
  "GNG11", "GPR35", "GPX3", "GSDMD", "GYG2", "GYPC", "H2BC6", "HAGHL",
  "HAP1", "HEBP2", "HELZ2", "HMG20B", "HSD3B7", "HSP90B2P", "HVCN1", "HYAL1",
  "HYAL2", "ICAM2", "ID1", "ID4", "IGDCC4", "IGFBP4", "IGFBP7", "IHO1",
  "IKZF5", "IL13RA1", "IL4R", "INHBB", "INTS4P2", "IPO8P1", "IRF1", "ITGB2",
  "ITGB4", "ITPKB", "JUN", "JUNB", "KCNJ8", "KCTD12", "KCTD15", "KDM6B",
  "KIF1C", "KIRREL1", "KLF15", "KRT18P6", "LAMA5", "LAMB2", "LAPTM5", "LATS2",
  "LCP1", "LCP2", "LFNG", "LGALS3", "LHX2", "LIMK2", "LINC01277", "LINC01721",
  "LINC02068", "LMO2", "LRP10", "LRRC37A4P", "LTBR", "LTC4S", "LY6G5B", "LYL1",
  "MAFK", "MAGED4", "MAGED4B", "MALRD1", "MAPK4", "MCL1", "MDH1B", "MFNG",
  "MIDN", "MLC1", "MMP14", "MRC2", "MSN", "MTSS2", "MTURN", "MXRA8",
  "MYL9", "MYO1C", "NAALAD2", "NECTIN2", "NEDD9", "NES", "NFAM1", "NFATC1",
  "NFATC4", "NFKB2", "NIBAN2", "NID1", "NKD1", "NKD2", "NOTCH1", "NOTCH3",
  "NPM1P33", "NPR1", "NTN3", "OAF", "OLFML3", "ORAI1", "OTOGL", "PAIP2B",
  "PALLD", "PAQR6", "PARP9", "PARVG", "PCDHGC3", "PCOLCE", "PCYOX1L", "PDCL3P2",
  "PDK4", "PDLIM4", "PDLIM5", "PEX26", "PGAM2", "PGGHG", "PI16", "PIEZO1",
  "PLAAT4", "PLCD3", "PLCG2", "PLEKHA4", "PLEKHO2", "PLIN2", "PLIN5", "PLOD3",
  "PLP2", "PLSCR4", "PLTP", "PML", "PODXL", "POLD1", "PON2", "PPP1R13L",
  "PPP2CB", "PRICKLE3", "PRKX", "PSTPIP1", "PTGES3", "PTTG1IP", "PXN", "PYCR1",
  "RAB13", "RAB32", "RARRES2", "RASIP1", "RASL12", "RDH10", "RELL1", "REXO1L1P",
  "RFX4", "RGL3", "RGR", "RGS19", "RGS9BP", "RHBDF2", "RHOC", "RHOJ",
  "RIN3", "RNF122", "RNF148", "RPS3AP29", "RPS4XP5", "RRAS", "RREB1", "RRH",
  "S100A16", "S1PR1", "SEMA3F", "SERTAD1", "SH3TC1", "SHE", "SHKBP1", "SIGIRR",
  "SIGLEC14", "SIPA1", "SIX5", "SLC12A4", "SLC12A7", "SLC1A3", "SLC2A1", "SLC2A4RG",
  "SLC30A1", "SLC39A1", "SLC44A5", "SLC4A2", "SLC7A5", "SLC9A3R1", "SLC9A3R2", "SLCO2A1",
  "SMAD6", "SMO", "SMOX", "SNORA50B", "SNX10", "SNX18P7", "SOX18", "SOX21",
  "SOX9", "SPARC", "SPARCL1", "SPHK1", "SQOR", "SRARP", "SSTR2", "STAB1",
  "STEAP3", "STOM", "STON2", "SUSD2", "SYDE1", "SYTL4", "TAGLN2", "TARBP1",
  "TBX15", "TBX18", "TBX2", "TCEA1P2", "TCIRG1", "TCTEX1D1", "TGFB1", "THCAT155",
  "TIE1", "TLN1", "TMBIM1", "TMEM100", "TMEM176A", "TMEM176B", "TMEM204", "TNFAIP3",
  "TNFRSF10B", "TNFRSF1A", "TNFRSF1B", "TNIP2", "TNXB", "TOB2", "TOR4A", "TP53I13",
  "TPM2", "TPST2", "TRIM47", "TRIM56", "TRIOBP", "TRIP10", "TSPAN12", "TSPAN4",
  "TSPO", "TXNIP", "UBALD2", "VEGFC", "VSIG4", "VSX1", "WFIKKN1", "WFIKKN2",
  "WIPF2", "WSCD1", "WWTR1", "YAP1", "YES1", "ZBED6CL", "ZBED9", "ZBTB42",
  "ZC3HAV1", "ZCCHC24", "ZFP36L1", "ZFP36L2", "ZIC1", "ZIC2", "ZIC4", "ZNF204P",
  "ZNF385B", "ZNF395", "ZNF778", "ZNF804B"
)

# --- Create Output Directory ---
dir.create("results", showWarnings = FALSE)

# --- Run Annotation Pipeline ---
message(paste0("Analyzing ", length(alzheimer_genes), " Alzheimer's related genes..."))

# 1. Create the initial object
annotaR_obj <- annotaR(alzheimer_genes)

# 2. Add functional enrichment data (GO terms)
message("Fetching GO term annotations from g:Profiler...")
annotaR_obj_go <- add_go_terms(annotaR_obj, sources = c("GO:BP", "REAC", "KEGG"))

# 3. Add disease links from OpenTargets
message("Fetching disease associations from OpenTargets...")
annotaR_obj_disease <- add_disease_links(annotaR_obj_go)

# 4. Add known drug links from OpenTargets
message("Fetching known drug associations from OpenTargets...")
full_annotation <- add_drug_links(annotaR_obj_disease)

# --- Save Raw Results ---
output_table_file <- "results/alzheimer_full_annotation.csv"
readr::write_csv(full_annotation, output_table_file)
message(paste("Full annotation table saved to:", output_table_file))

# --- Generate Visualization ---
message("Generating enrichment dot plot...")
enrichment_plot <- plot_enrichment_dotplot(
  annotaR_obj_go,
  n_terms = 25,
  title = "Top 25 Enriched GO/Reactome/KEGG Terms in Alzheimer's Genes"
)

# --- Save Plot ---
output_plot_file <- "results/alzheimer_enrichment_dot_plot.png"
ggsave(
  output_plot_file,
  plot = enrichment_plot,
  width = 12,
  height = 10,
  dpi = 300
)

message(paste("Enrichment plot saved to:", output_plot_file))

message("Alzheimer's disease gene analysis complete.")
