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
      alkphos     = LBDSAPSI,  # Alkaline phosphatase U/L
      wbc         = LBXWBCSI,  # WBC count
      weight      = WTMEC2YR   # Survey weight
    ) %>%
    mutate(
      cycle = cycle_name,
      # Log transform CRP (highly skewed)
      crp_log = log(crp + 1),
      # Recode sex to readable labels
      sex = ifelse(sex == 1, "Male", "Female")
    )
  
  return(df)
}

# ── Clean all three cycles ─────────────────────────────────────────────────

df_2015 <- clean_cycle(nhanes_2015, "2015-2016")
df_2017 <- clean_cycle(nhanes_2017, "2017-2018")
df_2019 <- clean_cycle(nhanes_2019, "2019-2020")

# ── Stack all cycles together ──────────────────────────────────────────────

nhanes_clean <- bind_rows(df_2015, df_2017, df_2019) %>%
  # Keep only adults 20 and older
  filter(age >= 20) %>%
  # Remove rows missing any of the 9 PhenoAge biomarkers
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