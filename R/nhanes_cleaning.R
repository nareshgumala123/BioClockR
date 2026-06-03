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

