# =============================================================================
# FILE:    03_download_geo.R
# PROJECT: BioClockR — Biological Age Estimation
# PHASE:   Phase 4 — Gene Expression Cross-Validation
# AUTHOR:  Naresh Gumala, N2 Cloud Tech (Full-time Data Analyst)
# DATE:    August 2024
#
# PURPOSE:
#   Download two GEO datasets used to cross-validate BioClockR biological
#   age estimates against independent gene expression and DNA methylation
#   aging signatures.
#
#   Dataset 1 — GSE65765:
#     Blood transcriptome aging study. 1,202 participants aged 20–89.
#     Source: Peters et al. (2015). Nature Comms. DOI: 10.1038/ncomms9570
#     Used to: check if high age acceleration (from BioClockR) correlates
#              with transcriptomic aging signatures.
#
#   Dataset 2 — GSE40279:
#     Whole blood DNA methylation (450K array). 656 individuals, ages 19-101.
#     Source: Hannum et al. (2013). Mol Cell. DOI: 10.1016/j.molcel.2012.10.016
#     Used to: compare PhenoAge acceleration to epigenetic age acceleration
#              (Horvath clock) at population level.
#
# DATA SOURCE:
#   NCBI Gene Expression Omnibus (GEO)
#   URL: https://www.ncbi.nlm.nih.gov/geo/
#   Free access, no registration required.
#
# OUTPUT:
#   data-raw/geo_gse65765_eset.rds    — ExpressionSet for GSE65765
#   data-raw/geo_gse40279_pheno.rds   — Phenotype data for GSE40279
# =============================================================================

library(GEOquery)
library(Biobase)
library(tidyverse)

# ── 1. Configure GEO download settings ───────────────────────────────────────

# Cache downloaded files to avoid repeated downloads
options(GEOquery.inmemory.gpl = FALSE)

geo_cache_dir <- "data-raw/geo_cache"
if (!dir.exists(geo_cache_dir)) {
  dir.create(geo_cache_dir, recursive = TRUE)
}

# ── 2. Download GSE65765 — Blood Transcriptome Aging ─────────────────────────

message("=== Downloading GSE65765 (Blood transcriptome, n=1202) ===")
message("Source: Peters et al. (2015) Nature Comms. DOI: 10.1038/ncomms9570")

gse65765_path <- file.path(geo_cache_dir, "gse65765_eset.rds")

if (!file.exists(gse65765_path)) {
  
  gse65765 <- getGEO(
    GEO        = "GSE65765",
    destdir    = geo_cache_dir,
    GSEMatrix  = TRUE,
    AnnotGPL   = FALSE
  )
  
  # GEO often returns a list; extract the ExpressionSet
  if (is.list(gse65765)) {
    gse65765 <- gse65765[[1]]
  }
  
  message(paste0("  ✓ Dimensions: ", nrow(exprs(gse65765)), " probes x ",
                 ncol(exprs(gse65765)), " samples"))
  
  # Extract phenotype data (sample metadata)
  pheno_65765 <- pData(gse65765) %>%
    as_tibble(rownames = "sample_id") %>%
    select(
      sample_id,
      title,
      geo_accession,
      contains("age"),
      contains("sex"),
      contains("gender")
    )
  
  message(paste0("  Phenotype variables available: ",
                 paste(colnames(pheno_65765), collapse = ", ")))
  
  saveRDS(gse65765, gse65765_path)
  message("  ✓ Saved: ", gse65765_path)
  
} else {
  message("  Loading from cache: ", gse65765_path)
  gse65765 <- readRDS(gse65765_path)
}

# ── 3. Extract and clean GSE65765 phenotype data ─────────────────────────────

pheno_65765 <- pData(gse65765) %>%
  as_tibble(rownames = "sample_id") %>%
  # Age and sex column names vary by GEO submission — use flexible extraction
  mutate(
    # Age: look for column containing 'age' (case-insensitive)
    age = as.numeric(
      coalesce(
        get(grep("^age", colnames(.), ignore.case = TRUE, value = TRUE)[1]),
        NA_real_
      )
    ),
    sex = tolower(
      coalesce(
        get(grep("sex|gender", colnames(.), ignore.case = TRUE, value = TRUE)[1]),
        NA_character_
      )
    )
  ) %>%
  select(sample_id, age, sex)

message(paste0("  GSE65765 phenotype: ", nrow(pheno_65765), " samples"))
message(paste0("  Age range: ", min(pheno_65765$age, na.rm = TRUE),
               "–", max(pheno_65765$age, na.rm = TRUE), " years"))

# ── 4. Download GSE40279 — Hannum Methylation Clock ──────────────────────────

message("\n=== Downloading GSE40279 (DNA methylation clock reference, n=656) ===")
message("Source: Hannum et al. (2013) Mol Cell. DOI: 10.1016/j.molcel.2012.10.016")

# Note: GSE40279 expression data is very large (450K probes × 656 samples).
# For BioClockR we only need the phenotype (metadata) for the comparison:
# participant chronological age vs. Hannum epigenetic age.
# We do NOT load the full expression matrix.

gse40279_pheno_path <- file.path(geo_cache_dir, "gse40279_pheno.rds")

if (!file.exists(gse40279_pheno_path)) {
  
  # Download only phenotype data (soft file) — much smaller
  gse40279 <- getGEO(
    GEO       = "GSE40279",
    destdir   = geo_cache_dir,
    GSEMatrix = TRUE,
    getGPL    = FALSE   # Skip platform annotation (very large)
  )
  
  if (is.list(gse40279)) {
    gse40279 <- gse40279[[1]]
  }
  
  # Extract phenotype only
  pheno_40279 <- pData(gse40279) %>%
    as_tibble(rownames = "sample_id") %>%
    select(sample_id, geo_accession, contains("age"), contains("sex"), contains("gender"))
  
  message(paste0("  ✓ GSE40279 phenotype: ", nrow(pheno_40279), " samples"))
  
  saveRDS(pheno_40279, gse40279_pheno_path)
  message("  ✓ Saved: ", gse40279_pheno_path)
  
} else {
  message("  Loading from cache: ", gse40279_pheno_path)
  pheno_40279 <- readRDS(gse40279_pheno_path)
}

# ── 5. Summary ────────────────────────────────────────────────────────────────

message("\n=== GEO Download Summary ===")
message(paste0("GSE65765: ", ncol(exprs(gse65765)), " blood transcriptome samples"))
message(paste0("GSE40279: ", nrow(pheno_40279), " methylation clock reference samples"))
message("\n✓ All GEO data ready for Phase 4 validation")
message("Next step: Run analysis/phase4_geo_validation.Rmd")
