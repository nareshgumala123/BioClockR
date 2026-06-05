# =============================================================================
# FILE:    02_download_nhanes_mortality.R
# PROJECT: BioClockR — Biological Age Estimation
# PHASE:   Phase 3 — Survival Validation
# AUTHOR:  Naresh Gumala, N2 Cloud Tech (Full-time Data Analyst)
# DATE:    May 2024
#
# PURPOSE:
#   Download the CDC NHANES Public-Use Linked Mortality Files for the
#   2015-2016 and 2017-2020 cohorts. These files link NHANES participants
#   to National Death Index (NDI) records through December 31, 2019.
#   Used to validate whether biological age acceleration (from BioClockR)
#   predicts all-cause mortality.
#
# DATA SOURCE:
#   CDC NCHS — NHANES Linked Mortality Files (Public Use)
#   URL: https://www.cdc.gov/nchs/data-linkage/mortality-public.htm
#   Format: Fixed-width ASCII text (.dat)
#   No registration required. Public use data.
#
# OUTPUT:
#   data-raw/nhanes_mortality_2015.rds
#   data-raw/nhanes_mortality_2017.rds
#   data-raw/nhanes_mortality_combined.rds
# =============================================================================

library(tidyverse)
library(readr)

# ── 1. File metadata ──────────────────────────────────────────────────────────
# CDC publishes these as fixed-width .dat files with a published data layout.
# Layout: https://ftp.cdc.gov/pub/health_statistics/nchs/datalinkage/linked_mortality/

mortality_urls <- list(
  nhanes_2015_2016 = paste0(
    "https://ftp.cdc.gov/pub/health_statistics/nchs/datalinkage/",
    "linked_mortality/NHANES_2015_2016_MORT_2019_PUBLIC.dat"
  ),
  nhanes_2017_2020 = paste0(
    "https://ftp.cdc.gov/pub/health_statistics/nchs/datalinkage/",
    "linked_mortality/NHANES_2017_2020_MORT_2019_PUBLIC.dat"
  )
)

# ── 2. Column positions from CDC published layout ─────────────────────────────
# Documented at: https://ftp.cdc.gov/pub/health_statistics/nchs/
#                datalinkage/linked_mortality/Linked_Mortality_Analytic_Guidelines.pdf

parse_mortality_dat <- function(filepath) {
  
  message(paste0("Parsing: ", filepath))
  
  # Read fixed-width file using published CDC layout
  mort_raw <- read_fwf(
    filepath,
    fwf_cols(
      seqn            = c(1, 14),    # NHANES respondent ID
      eligstat        = c(15, 15),   # Eligibility status (1 = eligible, 2 = under-age, 3 = deceased prior)
      mortstat        = c(16, 16),   # Vital status (0 = assumed alive, 1 = deceased)
      permth_int      = c(17, 21),   # Person-months of follow-up (interview to Dec 31, 2019)
      permth_exam     = c(22, 26),   # Person-months from exam date
      ucod_leading    = c(27, 29),   # Underlying cause of death (leading cause, ICD-10 recoded)
      diabetes        = c(30, 30),   # Diabetes on death certificate (1=yes, 0=no, blank=NA)
      hyperten        = c(31, 31)    # Hypertension on death certificate
    ),
    col_types = cols(.default = col_character()),
    skip = 0
  )
  
  # Convert types and compute useful fields
  mort_clean <- mort_raw %>%
    mutate(
      seqn        = as.numeric(seqn),
      eligstat    = as.numeric(eligstat),
      mortstat    = as.numeric(mortstat),
      permth_int  = as.numeric(permth_int),
      permth_exam = as.numeric(permth_exam),
      
      # Convert person-months to person-years
      follow_up_yrs = permth_exam / 12,
      
      # Binary outcome: 1 = died, 0 = censored (alive at end of follow-up)
      died = mortstat,
      
      # Cause of death grouping (ICD-10 based CDC coding)
      cod_category = case_when(
        ucod_leading %in% as.character(1)  ~ "heart_disease",
        ucod_leading %in% as.character(2)  ~ "cerebrovascular",
        ucod_leading %in% as.character(3)  ~ "malignant_neoplasm",
        ucod_leading %in% as.character(4)  ~ "chronic_lower_resp",
        ucod_leading %in% as.character(6)  ~ "diabetes",
        ucod_leading %in% as.character(9)  ~ "nephritis",
        ucod_leading %in% as.character(10) ~ "influenza_pneumonia",
        is.na(ucod_leading) | ucod_leading == "" ~ NA_character_,
        TRUE ~ "other"
      )
    ) %>%
    # Keep only eligible participants
    filter(eligstat == 1) %>%
    # Remove observations with missing follow-up time
    filter(!is.na(follow_up_yrs)) %>%
    select(seqn, died, follow_up_yrs, cod_category, diabetes, hyperten)
  
  message(paste0("  ✓ Parsed ", nrow(mort_clean), " eligible participants"))
  message(paste0("  Deaths: ", sum(mort_clean$died, na.rm = TRUE),
                 " (", round(mean(mort_clean$died, na.rm = TRUE) * 100, 1), "%)"))
  message(paste0("  Median follow-up: ", round(median(mort_clean$follow_up_yrs, na.rm = TRUE), 1), " years"))
  
  return(mort_clean)
}

# ── 3. Download and parse files ───────────────────────────────────────────────

message("=== Downloading NHANES 2015-2016 Mortality File ===")
# Note: CDC files require direct download; if URL fails, download manually from:
# https://www.cdc.gov/nchs/data-linkage/mortality-public.htm

# For reproducibility, we provide both URL download and local file path options
download_or_load <- function(url, local_path, parse_fn) {
  if (!file.exists(local_path)) {
    message(paste0("Downloading from: ", url))
    tryCatch({
      download.file(url, local_path, mode = "wb")
    }, error = function(e) {
      stop(paste0(
        "Download failed. Please manually download from:\n",
        "https://www.cdc.gov/nchs/data-linkage/mortality-public.htm\n",
        "and save as: ", local_path
      ))
    })
  } else {
    message(paste0("Loading from local cache: ", local_path))
  }
  parse_fn(local_path)
}

mort_2015 <- download_or_load(
  url        = mortality_urls$nhanes_2015_2016,
  local_path = "data-raw/nhanes_mortality_2015_raw.dat",
  parse_fn   = parse_mortality_dat
)

message("\n=== Downloading NHANES 2017-2020 Mortality File ===")
mort_2017 <- download_or_load(
  url        = mortality_urls$nhanes_2017_2020,
  local_path = "data-raw/nhanes_mortality_2017_raw.dat",
  parse_fn   = parse_mortality_dat
)

# ── 4. Combine both cycles ────────────────────────────────────────────────────

mort_combined <- bind_rows(
  mort_2015 %>% mutate(cycle = "2015-2016"),
  mort_2017 %>% mutate(cycle = "2017-2020")
)

message(paste0("\nCombined mortality dataset: ", nrow(mort_combined), " participants"))

# ── 5. Save outputs ───────────────────────────────────────────────────────────

saveRDS(mort_2015,    "data-raw/nhanes_mortality_2015.rds")
saveRDS(mort_2017,    "data-raw/nhanes_mortality_2017.rds")
saveRDS(mort_combined,"data-raw/nhanes_mortality_combined.rds")

message("✓ Mortality files saved to data-raw/")
message("Next step: Run analysis/phase3_survival_validation.Rmd")
