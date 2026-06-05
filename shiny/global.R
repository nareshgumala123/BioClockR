# =============================================================================
# FILE:    shiny/global.R
# PROJECT: BioClockR — Biological Age Estimation
# PHASE:   Phase 5 — Shiny Application
# AUTHOR:  Naresh Gumala, N2 Cloud Tech (Full-time Data Analyst)
# DATE:    November 2024 – May 2026
#
# PURPOSE:
#   Global setup for BioClockR Shiny app.
#   Loads packages, pre-computes NHANES population reference data,
#   and defines shared helper functions used across app.R.
#
#   Population reference is used to compute the user's biological age
#   PERCENTILE — where do they fall relative to the NHANES population
#   of the same sex and age decade?
# =============================================================================

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(DT)
library(tidyverse)
library(survival)
library(dplyr)

# ── 1. Load BioClockR functions ───────────────────────────────────────────────
# When deployed, source all R/ functions directly
# (In production: install as package with library(BioClockR))

source_files <- list.files("../R", pattern = "\\.R$", full.names = TRUE)
invisible(lapply(source_files, source))

# ── 2. PhenoAge coefficients (self-contained reference) ───────────────────────
# Reproduced here for app self-containment — from Levine et al. 2018
PHENOAGE_COEFFICIENTS <- list(
  intercept   = -19.9067,
  b_albumin   = -0.0336,
  b_creat     =  0.0095,
  b_glucose   = -0.1953,
  b_crp       =  0.0954,
  b_lymph     = -0.0120,
  b_mcv       =  0.0268,
  b_rdw       =  0.3306,
  b_alkphos   =  0.00188,
  b_wbc       =  0.0554,
  gamma       =  0.090165,
  lambda      = -0.00553,
  age_const   =  141.50
)

# ── 3. NHANES Population Reference ────────────────────────────────────────────
# Pre-computed PhenoAge distribution by age decade and sex
# (run from the clean NHANES dataset — summarised here for app performance)
# Source: NHANES 2015–2020 analysis in BioClockR Phase 1–2

NHANES_REFERENCE <- tibble(
  age_decade  = rep(c("20-29","30-39","40-49","50-59","60-69","70-79"), 2),
  sex_label   = c(rep("Male", 6), rep("Female", 6)),
  mean_pheno  = c(27.3, 33.9, 40.8, 47.6, 55.1, 63.8,   # Male
                  26.1, 32.7, 39.6, 46.5, 54.0, 62.5),   # Female
  sd_pheno    = c(7.2, 7.8, 8.4, 9.1, 9.8, 10.2,
                  6.9, 7.4, 8.1, 8.7, 9.4, 9.9),
  n           = c(580, 620, 590, 610, 540, 420,
                  560, 600, 570, 630, 560, 440)
)

# ── 4. Reference survival curve data ──────────────────────────────────────────
# Simplified Gompertz survival curves based on US life tables (CDC 2020)
# Used to visualise the user's projected survival curve in the app
# Source: National Vital Statistics Reports, Vol. 71, No. 1 (2022)
# URL: https://www.cdc.gov/nchs/products/nvsr.htm

# 10-year all-cause mortality probability by age and acceleration category
SURVIVAL_REFERENCE <- tibble(
  age         = rep(seq(20, 80, by = 10), 3),
  accel_group = rep(c("Accelerated (+5yr)", "Average", "Decelerated (-5yr)"), each = 7),
  p_10yr_mort = c(
    # Accelerated aging (biological age 5yr > chronological)
    c(0.008, 0.018, 0.040, 0.095, 0.198, 0.372, 0.548),
    # Average
    c(0.005, 0.011, 0.025, 0.061, 0.134, 0.268, 0.425),
    # Decelerated (biologically younger)
    c(0.003, 0.007, 0.016, 0.039, 0.088, 0.183, 0.315)
  )
)

# ── 5. Input validation ranges ────────────────────────────────────────────────
BIOMARKER_RANGES <- list(
  albumin_gdl      = list(min = 1.5, max = 6.0, normal_low = 3.5, normal_high = 5.0, unit = "g/dL"),
  creatinine_umoll = list(min = 20,  max = 800,  normal_low = 44,  normal_high = 106, unit = "µmol/L"),
  glucose_mmoll    = list(min = 2.0, max = 30.0, normal_low = 3.9, normal_high = 5.6, unit = "mmol/L"),
  crp_mgl          = list(min = 0,   max = 200,  normal_low = 0,   normal_high = 3.0, unit = "mg/L"),
  lymphocyte_pct   = list(min = 1,   max = 90,   normal_low = 20,  normal_high = 40,  unit = "%"),
  mcv_fl           = list(min = 60,  max = 120,  normal_low = 80,  normal_high = 100, unit = "fL"),
  rdw_pct          = list(min = 9,   max = 25,   normal_low = 11.5,normal_high = 14.5,unit = "%"),
  alkphos_ul       = list(min = 10,  max = 400,  normal_low = 20,  normal_high = 140, unit = "U/L"),
  wbc_1000ul       = list(min = 1.0, max = 25.0, normal_low = 4.0, normal_high = 11.0,unit = "×10³/µL")
)

# ── 6. App theme colours ──────────────────────────────────────────────────────
APP_COLORS <- list(
  primary    = "#1F4E79",
  secondary  = "#2E75B6",
  accent_red = "#C00000",
  accent_grn = "#375623",
  neutral    = "#595959",
  bg_light   = "#EBF3FB"
)
