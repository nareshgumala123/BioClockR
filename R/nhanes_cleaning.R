<<<<<<< HEAD
# =============================================================================
# FILE:    R/nhanes_cleaning.R
# PROJECT: BioClockR — Biological Age Estimation
# PHASE:   Phase 1 — Data Cleaning and QC
# AUTHOR:  Naresh Gumala, N2 Cloud Tech (Intern)
# DATE:    February 2024
#
# PURPOSE:
#   Quality control and cleaning of the merged NHANES dataset.
#   Applies inclusion/exclusion criteria for the BioClockR analysis cohort.
#
# INCLUSION CRITERIA (based on Levine et al. 2018):
#   - Age 20–84 years (NHANES age cap is 80 for public data; 84 for restricted)
#   - Non-pregnant at time of exam
#   - Complete data on all 9 PhenoAge biomarkers
#   - Not missing survey weight
# =============================================================================

#' Clean NHANES Combined Dataset for BioClockR Analysis
#'
#' Applies inclusion/exclusion criteria, handles outliers, and prepares
#' the analysis-ready NHANES dataset for biological age computation.
#'
#' @param nhanes_raw A data frame as produced by data-raw/01_download_nhanes.R.
#' @param age_min Minimum age to include. Default 20.
#' @param age_max Maximum age to include. Default 84.
#' @param winsorise_biomarkers Logical. If TRUE, winsorise biomarkers at
#'   0.5th and 99.5th percentile. Default TRUE.
#'
#' @return A cleaned data frame with one row per NHANES participant.
#'
#' @examples
#' \dontrun{
#'   nhanes_raw <- readRDS("data-raw/nhanes_combined_raw.rds")
#'   nhanes_clean <- clean_nhanes(nhanes_raw)
#' }
#'
#' @export
clean_nhanes <- function(nhanes_raw,
                         age_min = 20,
                         age_max = 84,
                         winsorise_biomarkers = TRUE) {
  
  # The 9 PhenoAge biomarkers that must all be present
  phenoage_vars <- c(
    "albumin_gdl", "creatinine_umoll", "glucose_mmoll", "crp_log",
    "lymphocyte_pct", "mcv_fl", "rdw_pct", "alkphos_ul", "wbc_1000ul"
  )
  
  message("=== BioClockR: NHANES Cleaning Pipeline ===")
  message(paste0("Input: ", nrow(nhanes_raw), " participants"))
  
  df <- nhanes_raw
  
  # ── Step 1: Age filter ─────────────────────────────────────────────────────
  n_before <- nrow(df)
  df <- df %>% filter(age_years >= age_min & age_years <= age_max)
  message(paste0("After age filter (", age_min, "-", age_max, " yrs): ",
                 nrow(df), " (removed ", n_before - nrow(df), ")"))
  
  # ── Step 2: Remove missing survey weight ──────────────────────────────────
  n_before <- nrow(df)
  df <- df %>% filter(!is.na(weight) & weight > 0)
  message(paste0("After removing missing weights: ",
                 nrow(df), " (removed ", n_before - nrow(df), ")"))
  
  # ── Step 3: Complete case on all 9 PhenoAge biomarkers ───────────────────
  n_before <- nrow(df)
  df <- df %>% filter(complete.cases(df[, phenoage_vars]))
  message(paste0("After complete-case filter (9 biomarkers): ",
                 nrow(df), " (removed ", n_before - nrow(df), ")"))
  
  # ── Step 4: Physiologically implausible value exclusion ──────────────────
  # Reference ranges from NHANES analytic notes and clinical standards
  n_before <- nrow(df)
  
  df <- df %>%
    filter(
      albumin_gdl      >= 1.0  & albumin_gdl      <= 6.0,    # g/dL
      creatinine_umoll >= 20   & creatinine_umoll  <= 1500,   # µmol/L
      glucose_mmoll    >= 2.0  & glucose_mmoll     <= 35.0,   # mmol/L
      lymphocyte_pct   >= 1.0  & lymphocyte_pct    <= 95.0,   # %
      mcv_fl           >= 50   & mcv_fl             <= 130,    # fL
      rdw_pct          >= 9.0  & rdw_pct            <= 30.0,   # %
      alkphos_ul       >= 10   & alkphos_ul         <= 500,    # U/L
      wbc_1000ul       >= 1.0  & wbc_1000ul         <= 30.0    # 1000 cells/µL
    )
  
  message(paste0("After physiological range exclusions: ",
                 nrow(df), " (removed ", n_before - nrow(df), ")"))
  
  # ── Step 5: Optional winsorisation ──────────────────────────────────────
  if (winsorise_biomarkers) {
    
    winsorise <- function(x, low_pct = 0.005, high_pct = 0.995) {
      q_low  <- quantile(x, probs = low_pct,  na.rm = TRUE)
      q_high <- quantile(x, probs = high_pct, na.rm = TRUE)
      pmax(pmin(x, q_high), q_low)
    }
    
    df <- df %>%
      mutate(across(all_of(phenoage_vars), winsorise))
    
    message("Winsorisation applied at 0.5th and 99.5th percentiles")
  }
  
  # ── Step 6: Re-label categorical variables ────────────────────────────────
  df <- df %>%
    mutate(
      sex_label = case_when(
        sex == 1 ~ "Male",
        sex == 2 ~ "Female",
        TRUE     ~ NA_character_
      ),
      race_label = case_when(
        race_eth == 1 ~ "Mexican American",
        race_eth == 2 ~ "Other Hispanic",
        race_eth == 3 ~ "Non-Hispanic White",
        race_eth == 4 ~ "Non-Hispanic Black",
        race_eth == 6 ~ "Non-Hispanic Asian",
        race_eth == 7 ~ "Other/Multi",
        TRUE          ~ NA_character_
      ),
      income_group = case_when(
        pir <  1.3 ~ "Low (<1.3 PIR)",
        pir <  3.5 ~ "Middle (1.3-3.5 PIR)",
        pir >= 3.5 ~ "High (>3.5 PIR)",
        TRUE       ~ NA_character_
      )
    )
  
  # ── Step 7: Final summary ─────────────────────────────────────────────────
  message("\n=== Final Clean Dataset Summary ===")
  message(paste0("Total participants: ", nrow(df)))
  message(paste0("Age range: ", min(df$age_years), "–", max(df$age_years), " years"))
  message(paste0("Mean age (unweighted): ", round(mean(df$age_years), 1)))
  message(paste0("Sex: ", sum(df$sex == 1, na.rm = TRUE), " Male, ",
                 sum(df$sex == 2, na.rm = TRUE), " Female"))
  message(paste0("Cycles: ",
                 paste(table(df$nhanes_cycle), collapse = "; ")))
  
  return(df)
}


#' Summarise Biomarker Distributions in Cleaned NHANES Dataset
#'
#' Returns a summary table of mean, SD, median, and missingness
#' for all 9 PhenoAge biomarkers.
#'
#' @param nhanes_clean A cleaned NHANES data frame from \code{clean_nhanes()}.
#' @return A tibble with one row per biomarker.
#'
#' @export
summarise_biomarkers <- function(nhanes_clean) {
  
  biomarker_vars <- c(
    "albumin_gdl", "creatinine_umoll", "glucose_mmoll", "crp_log",
    "lymphocyte_pct", "mcv_fl", "rdw_pct", "alkphos_ul", "wbc_1000ul"
  )
  
  biomarker_labels <- c(
    "Albumin (g/dL)", "Creatinine (µmol/L)", "Glucose (mmol/L)",
    "CRP log(mg/L+1)", "Lymphocyte (%)", "MCV (fL)",
    "RDW (%)", "Alkaline Phosphatase (U/L)", "WBC (1000/µL)"
  )
  
  summary_tbl <- nhanes_clean %>%
    select(all_of(biomarker_vars)) %>%
    summarise(across(
      everything(),
      list(
        mean   = ~round(mean(.x, na.rm = TRUE), 2),
        sd     = ~round(sd(.x, na.rm = TRUE), 2),
        median = ~round(median(.x, na.rm = TRUE), 2),
        p5     = ~round(quantile(.x, 0.05, na.rm = TRUE), 2),
        p95    = ~round(quantile(.x, 0.95, na.rm = TRUE), 2),
        n_miss = ~sum(is.na(.x))
      )
    )) %>%
    pivot_longer(
      everything(),
      names_to  = c("biomarker", ".value"),
      names_sep = "_(?=[^_]+$)"
    ) %>%
    mutate(biomarker = biomarker_labels[match(biomarker, biomarker_vars)])
  
  return(summary_tbl)
}
=======
# nhanes_cleaning.R
# Purpose: Merge NHANES 2015-2020 cycles, extract PhenoAge biomarkers, handle missing values

library(dplyr)

# ── Load raw data ──────────────────────────────────────────────────────────

nhanes_2015 <- readRDS("data-raw/nhanes_raw/nhanes_2015.rds")
nhanes_2017 <- readRDS("data-raw/nhanes_raw/nhanes_2017.rds")
nhanes_2019 <- readRDS("data-raw/nhanes_raw/nhanes_2019.rds")

# ── Function to clean one cycle ────────────────────────────────────────────

clean_cycle <- function(cycle, cycle_name) {
  
  # Merge all modules by SEQN (unique participant ID)
  df <- cycle$demo %>%
    left_join(cycle$cbc,  by = "SEQN") %>%
    left_join(cycle$bmp,  by = "SEQN") %>%
    left_join(cycle$crp,  by = "SEQN") %>%
    left_join(cycle$gluc, by = "SEQN") %>%
    left_join(cycle$ghb,  by = "SEQN")
  
  # Select and rename only the columns we need
  df <- df %>%
    select(
      SEQN,                    # Participant ID
      age         = RIDAGEYR,  # Age in years
      sex         = RIAGENDR,  # 1=Male, 2=Female
      race        = RIDRETH3,  # Race/ethnicity
      albumin     = LBDSALSI,  # Albumin g/L
      creatinine  = LBDSCRSI,  # Creatinine umol/L
      glucose     = LBDGLUSI,  # Glucose mmol/L
      crp         = LBXHSCRP,  # CRP mg/L
      lymphocyte  = LBDLYMNO,  # Lymphocyte number
      mcv         = LBXMCVSI,  # MCV fL
      rdw         = LBXRDW,    # RDW %
      alkphos     = LBXSAPSI,  # Alkaline phosphatase U/L
      wbc         = LBXWBCSI,  # WBC count
      weight      = WTMEC2YR   # Survey weight
    ) %>%
    mutate(
      cycle   = cycle_name,
      crp_log = log(crp + 1),
      sex     = as.character(sex)
    )
  
  return(df)
}

# ── Clean all three cycles ─────────────────────────────────────────────────

df_2015 <- clean_cycle(nhanes_2015, "2015-2016")
df_2017 <- clean_cycle(nhanes_2017, "2017-2018")
df_2019 <- clean_cycle(nhanes_2019, "2019-2020")

# ── Stack all cycles together ──────────────────────────────────────────────

nhanes_clean <- bind_rows(df_2015, df_2017, df_2019) %>%
  filter(age >= 20) %>%
  filter(!is.na(albumin)    &
           !is.na(creatinine) &
           !is.na(glucose)    &
           !is.na(crp)        &
           !is.na(lymphocyte) &
           !is.na(mcv)        &
           !is.na(rdw)        &
           !is.na(alkphos)    &
           !is.na(wbc))

# ── Save cleaned data ──────────────────────────────────────────────────────

dir.create("data-raw/nhanes_clean", showWarnings = FALSE)
saveRDS(nhanes_clean, "data-raw/nhanes_clean/nhanes_clean.rds")

message("✅ Cleaning complete. Rows: ", nrow(nhanes_clean))
# bioclock_model.R
# Purpose: Compute PhenoAge biological age for each NHANES participant
# Reference: Levine et al. 2018 doi:10.18632/aging.101414

library(dplyr)

# ── Load clean data ────────────────────────────────────────────────────────

nhanes_clean <- readRDS("data-raw/nhanes_clean/nhanes_clean.rds")
compute_phenoage <- function(df) {
  
  # Convert units to match Levine et al. 2018 formula
  glucose_mgdl    <- df$glucose    * 18.018  # mmol/L to mg/dL
  creatinine_mgdl <- df$creatinine / 88.42   # umol/L to mg/dL
  
  # Step 1 — Linear combination of 9 biomarkers
  xb <- -19.9067 +
    (-0.0336 * df$albumin)      +
    (0.0095  * creatinine_mgdl) +
    (0.1953  * glucose_mgdl)    +
    (0.0954  * df$crp_log)      +
    (0.0120  * df$lymphocyte)   +
    (0.0268  * df$mcv)          +
    (0.3306  * df$rdw)          +
    (0.00188 * df$alkphos)      +
    (0.0554  * df$wbc)
  
  # Step 2 — Convert to biological age
  pheno_age <- 141.50 +
    (log(0.00553 * log(1 - exp(xb) / (1 - exp(exp(xb))))) / 0.090165)
  
  return(pheno_age)
}

# ── Compute biological age and age acceleration ────────────────────────────
nhanes_model <- nhanes_clean %>%
  mutate(
    bio_age   = compute_phenoage(.),
    age_accel = bio_age - age
  ) %>%
  filter(!is.na(bio_age))

# ── Quick sanity check ─────────────────────────────────────────────────────

cat("── PhenoAge Summary ──────────────────────────────────────\n")
cat("Chronological age - Mean:", round(mean(nhanes_model$age), 1),
    " SD:", round(sd(nhanes_model$age), 1), "\n")
cat("Biological age    - Mean:", round(mean(nhanes_model$bio_age, na.rm=TRUE), 1),
    " SD:", round(sd(nhanes_model$bio_age, na.rm=TRUE), 1), "\n")
cat("Age acceleration  - Mean:", round(mean(nhanes_model$age_accel, na.rm=TRUE), 1),
    " SD:", round(sd(nhanes_model$age_accel, na.rm=TRUE), 1), "\n")
cat("─────────────────────────────────────────────────────────\n")

# ── Save model output ──────────────────────────────────────────────────────

dir.create("data-raw/model_output", showWarnings = FALSE)
saveRDS(nhanes_model, "data-raw/model_output/nhanes_model.rds")

message("✅ PhenoAge computation complete. Rows: ", nrow(nhanes_model))
# bioclock_model.R
# Purpose: Compute PhenoAge biological age for each NHANES participant
# Reference: Levine et al. 2018 doi:10.18632/aging.101414

library(dplyr)

# ── Load clean data ────────────────────────────────────────────────────────

nhanes_clean <- readRDS("data-raw/nhanes_clean/nhanes_clean.rds")

# ── PhenoAge Formula (Levine et al. 2018) ─────────────────────────────────

compute_phenoage <- function(df) {
  
  glucose_mgdl    <- df$glucose    * 18.018
  creatinine_mgdl <- df$creatinine / 88.42
  
  xb <- -19.9067 +
    (-0.0336 * df$albumin)      +
    (0.0095  * creatinine_mgdl) +
    (0.0954  * glucose_mgdl)    +
    (0.0120  * df$crp_log)      +
    (0.0120  * df$lymphocyte)   +
    (0.0268  * df$mcv)          +
    (0.3306  * df$rdw)          +
    (0.00188 * df$alkphos)      +
    (0.0554  * df$wbc)
  
  pheno_age <- 141.50 +
    (log(0.00553 * log(1 - exp(xb) / (1 - exp(exp(xb))))) / 0.090165)
  
  return(pheno_age)
}

# ── Compute biological age and age acceleration ────────────────────────────

nhanes_model <- nhanes_clean %>%
  mutate(
    bio_age   = compute_phenoage(.),
    age_accel = bio_age - age
  ) %>%
  filter(!is.na(bio_age))

# ── Quick sanity check ─────────────────────────────────────────────────────

cat("── PhenoAge Summary ──────────────────────────────────────\n")
cat("Chronological age - Mean:", round(mean(nhanes_model$age), 1),
    " SD:", round(sd(nhanes_model$age), 1), "\n")
cat("Biological age    - Mean:", round(mean(nhanes_model$bio_age, na.rm=TRUE), 1),
    " SD:", round(sd(nhanes_model$bio_age, na.rm=TRUE), 1), "\n")
cat("Age acceleration  - Mean:", round(mean(nhanes_model$age_accel, na.rm=TRUE), 1),
    " SD:", round(sd(nhanes_model$age_accel, na.rm=TRUE), 1), "\n")
cat("─────────────────────────────────────────────────────────\n")

# ── Save model output ──────────────────────────────────────────────────────

dir.create("data-raw/model_output", showWarnings = FALSE)
saveRDS(nhanes_model, "data-raw/model_output/nhanes_model.rds")

message("✅ PhenoAge computation complete. Rows: ", nrow(nhanes_model))

>>>>>>> c54d41e11034a866cc14adae5142cccda7fa1603
