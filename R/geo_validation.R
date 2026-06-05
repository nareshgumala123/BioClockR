# =============================================================================
# FILE:    R/geo_validation.R
# PROJECT: BioClockR — Biological Age Estimation
# PHASE:   Phase 4 — Gene Expression Cross-Validation
# AUTHOR:  Naresh Gumala, N2 Cloud Tech (Full-time Data Analyst)
# DATE:    August – November 2024
#
# PURPOSE:
#   Cross-validate BioClockR biological age using blood gene expression
#   data (GEO GSE65765). Tests whether individuals who are biologically
#   older (higher PhenoAge) also show transcriptomic aging signatures.
#
#   Analysis steps:
#   1. Load GSE65765 (n=1,202 blood transcriptomes, ages 20-89)
#   2. Compute a transcriptomic aging score using published aging gene sets
#   3. Regress transcriptomic aging score on PhenoAge-estimated biological age
#   4. Compare to regression on chronological age
#
# GENE SETS USED:
#   MSigDB HALLMARK gene sets from:
#   https://www.gsea-msigdb.org/gsea/msigdb/
#   Specifically: HALLMARK_TNFA_SIGNALING_VIA_NFKB (inflammation with aging)
#                 HALLMARK_P53_PATHWAY (cellular senescence)
#                 HALLMARK_REACTIVE_OXYGEN_SPECIES_PATHWAY (oxidative stress)
# =============================================================================

library(GEOquery)
library(Biobase)
library(limma)
library(fgsea)
library(tidyverse)
library(ggplot2)
library(broom)


#' Prepare GSE65765 for Aging Analysis
#'
#' Loads the GSE65765 ExpressionSet, extracts expression matrix and
#' phenotype data, and performs log2 normalisation if needed.
#'
#' @param gse65765_path Path to saved GSE65765 ExpressionSet .rds file.
#' @return A list with: expr_matrix (genes × samples), pheno (sample metadata).
#'
#' @export
prepare_gse65765 <- function(gse65765_path = "data-raw/geo_cache/gse65765_eset.rds") {
  
  message("Loading GSE65765 (Peters et al. 2015, Nature Comms)...")
  
  if (!file.exists(gse65765_path)) {
    stop(paste0("File not found: ", gse65765_path,
                "\nRun data-raw/03_download_geo.R first."))
  }
  
  gse <- readRDS(gse65765_path)
  
  # Extract expression matrix
  expr <- exprs(gse)
  
  message(paste0("Expression matrix: ", nrow(expr), " probes × ",
                 ncol(expr), " samples"))
  
  # Check if log2 transformed (values > 20 suggest non-log scale)
  if (max(expr, na.rm = TRUE) > 20) {
    message("Expression values appear non-log2. Applying log2(x+1) transformation.")
    expr <- log2(expr + 1)
  }
  
  # Extract and clean phenotype data
  pheno <- pData(gse) %>%
    as_tibble(rownames = "sample_id") %>%
    # Flexible age column extraction
    mutate(
      age = as.numeric(
        # Try common column name patterns for age
        coalesce(
          if ("age" %in% tolower(colnames(.))) .[[grep("^age$", colnames(.), ignore.case = TRUE)[1]]] else NA,
          if (any(grepl("age.*yr", colnames(.), ignore.case = TRUE))) .[[grep("age.*yr", colnames(.), ignore.case = TRUE)[1]]] else NA,
          NA_character_
        )
      )
    ) %>%
    select(sample_id, age)
  
  message(paste0("Phenotype data: ", nrow(pheno), " samples"))
  message(paste0("Age range: ", min(pheno$age, na.rm = TRUE),
                 "–", max(pheno$age, na.rm = TRUE)))
  
  return(list(
    expr_matrix = expr,
    pheno       = pheno,
    eset        = gse
  ))
}


#' Identify Differentially Expressed Genes Associated with Age
#'
#' Runs limma linear model: expression ~ age, on all probes in GSE65765.
#' Returns ranked gene list for GSEA.
#'
#' @param gse_data Output from \code{prepare_gse65765()}.
#' @return A named numeric vector of t-statistics ranked for fgsea.
#'
#' @export
run_age_de_analysis <- function(gse_data) {
  
  message("Running limma age-associated differential expression...")
  
  expr  <- gse_data$expr_matrix
  pheno <- gse_data$pheno
  
  # Align samples
  common_samples <- intersect(colnames(expr), pheno$sample_id)
  expr  <- expr[, common_samples]
  pheno <- pheno %>% filter(sample_id %in% common_samples) %>%
    arrange(match(sample_id, common_samples))
  
  # Remove probes with too many NAs
  na_rate <- rowMeans(is.na(expr))
  expr    <- expr[na_rate < 0.1, ]
  
  message(paste0("After NA filter: ", nrow(expr), " probes"))
  
  # Build design matrix
  design <- model.matrix(~age, data = pheno)
  
  # Fit limma model
  fit <- lmFit(expr, design)
  fit <- eBayes(fit)
  
  # Extract results for 'age' coefficient
  de_results <- topTable(
    fit,
    coef   = "age",
    number = Inf,
    sort.by = "t"
  ) %>%
    as_tibble(rownames = "probe_id")
  
  message(paste0("DE analysis complete: ", nrow(de_results), " probes tested"))
  message(paste0("Probes significantly age-associated (FDR<0.05): ",
                 sum(de_results$adj.P.Val < 0.05, na.rm = TRUE)))
  
  # Create ranked vector for GSEA (ranked by t-statistic)
  ranked_genes <- de_results$t
  names(ranked_genes) <- de_results$probe_id
  ranked_genes <- sort(ranked_genes, decreasing = TRUE)
  
  return(list(
    de_results   = de_results,
    ranked_genes = ranked_genes
  ))
}


#' Run GSEA on Age-Associated Gene Expression
#'
#' Uses fgsea to test MSigDB Hallmark gene sets for enrichment
#' in age-associated differentially expressed genes.
#'
#' @param de_results Output from \code{run_age_de_analysis()}.
#' @param hallmark_gmt_path Path to MSigDB Hallmark GMT file.
#'   Download from: https://www.gsea-msigdb.org/gsea/msigdb/
#'   File: h.all.v2023.1.Hs.symbols.gmt
#' @return GSEA results tibble with NES, p-value, FDR for each pathway.
#'
#' @export
run_aging_gsea <- function(de_results, hallmark_gmt_path = "data-raw/h.all.v2023.1.Hs.symbols.gmt") {
  
  if (!file.exists(hallmark_gmt_path)) {
    stop(paste0(
      "Hallmark GMT file not found: ", hallmark_gmt_path, "\n",
      "Download from: https://www.gsea-msigdb.org/gsea/msigdb/human/collections.jsp\n",
      "Click: HALLMARK > Download GMT (symbols)"
    ))
  }
  
  message("Running GSEA with MSigDB Hallmark gene sets...")
  
  # Load gene sets
  hallmark_sets <- gmtPathways(hallmark_gmt_path)
  
  # Focus on aging-relevant pathways
  aging_pathways <- c(
    "HALLMARK_TNFA_SIGNALING_VIA_NFKB",       # Inflammation
    "HALLMARK_P53_PATHWAY",                    # Senescence
    "HALLMARK_REACTIVE_OXYGEN_SPECIES_PATHWAY",# Oxidative stress
    "HALLMARK_INFLAMMATORY_RESPONSE",          # General inflammation
    "HALLMARK_APOPTOSIS",                      # Cell death
    "HALLMARK_DNA_REPAIR",                     # DNA damage
    "HALLMARK_MYC_TARGETS_V1",                 # Proliferation
    "HALLMARK_MITOTIC_SPINDLE"                 # Cell cycle
  )
  
  hallmark_subset <- hallmark_sets[names(hallmark_sets) %in% aging_pathways]
  
  # Run fgsea
  set.seed(42)
  gsea_results <- fgsea(
    pathways   = hallmark_subset,
    stats      = de_results$ranked_genes,
    minSize    = 10,
    maxSize    = 500,
    nperm      = 1000
  ) %>%
    as_tibble() %>%
    arrange(padj) %>%
    mutate(
      pathway_short = gsub("HALLMARK_", "", pathway),
      direction     = ifelse(NES > 0, "Up with aging", "Down with aging"),
      significant   = padj < 0.05
    )
  
  message(paste0("GSEA results: ", nrow(gsea_results), " pathways tested"))
  message(paste0("Significant (FDR<0.05): ", sum(gsea_results$significant), " pathways"))
  
  return(gsea_results)
}


#' Compute Per-Sample Aging Pathway Score
#'
#' For each GSE65765 sample, compute a mean expression score of the
#' top aging-upregulated pathway genes. Then correlate this score with
#' chronological age.
#'
#' @param gse_data Output from prepare_gse65765().
#' @param aging_genes Character vector of aging-upregulated gene/probe IDs.
#' @return A tibble with sample_id, age, aging_score, and correlation results.
#'
#' @export
compute_aging_expression_score <- function(gse_data, aging_genes) {
  
  expr  <- gse_data$expr_matrix
  pheno <- gse_data$pheno
  
  # Filter to aging genes present in expression matrix
  genes_present <- intersect(aging_genes, rownames(expr))
  message(paste0("Aging genes in expression matrix: ",
                 length(genes_present), " / ", length(aging_genes)))
  
  # Compute mean expression score per sample
  aging_score <- colMeans(expr[genes_present, ], na.rm = TRUE)
  
  sample_scores <- tibble(
    sample_id    = names(aging_score),
    aging_score  = aging_score
  ) %>%
    left_join(pheno, by = "sample_id") %>%
    filter(!is.na(age))
  
  # Correlation: aging score vs. chronological age
  cor_test <- cor.test(
    sample_scores$aging_score,
    sample_scores$age,
    method = "pearson"
  )
  
  message(paste0(
    "Correlation (aging expression score ~ chronological age):\n",
    "  r = ", round(cor_test$estimate, 3),
    ", 95% CI: [", round(cor_test$conf.int[1], 3),
    ", ", round(cor_test$conf.int[2], 3), "]",
    ", p = ", formatC(cor_test$p.value, digits = 3, format = "e")
  ))
  
  return(sample_scores)
}


#' Plot Aging Expression Score vs. Chronological Age
#'
#' @param sample_scores Output from compute_aging_expression_score().
#' @return A ggplot2 scatter plot with regression line.
#'
#' @export
plot_aging_score_vs_age <- function(sample_scores) {
  
  cor_val <- cor(sample_scores$aging_score, sample_scores$age,
                 use = "complete.obs")
  
  ggplot(sample_scores, aes(x = age, y = aging_score)) +
    geom_point(alpha = 0.4, color = "#2E75B6", size = 1.5) +
    geom_smooth(method = "lm", color = "#C00000", se = TRUE, linewidth = 1) +
    annotate("text", x = Inf, y = Inf,
             label = paste0("r = ", round(cor_val, 3)),
             hjust = 1.2, vjust = 2, size = 5, color = "#1F4E79", fontface = "bold") +
    labs(
      title    = "Blood Transcriptomic Aging Score vs. Chronological Age",
      subtitle = "GEO GSE65765 — Peters et al. (2015). n = 1,202",
      x        = "Chronological Age (years)",
      y        = "Mean Expression: Aging-Upregulated Genes",
      caption  = "Aging score = mean log2 expression of HALLMARK_TNFA + P53 + ROS upregulated genes.\nGEO: GSE65765. DOI: 10.1038/ncomms9570"
    ) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold", color = "#1F4E79"))
}
