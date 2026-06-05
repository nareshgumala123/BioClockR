# =============================================================================
# FILE:    R/survival_validation.R
# PROJECT: BioClockR — Biological Age Estimation
# PHASE:   Phase 3 — Survival Validation
# AUTHOR:  Naresh Gumala, N2 Cloud Tech (Full-time Data Analyst)
# DATE:    May – August 2024
#
# PURPOSE:
#   Validate BioClockR biological age acceleration against real mortality
#   outcomes using the NHANES public-use linked mortality file.
#   Tests the core hypothesis:
#     "Higher biological age acceleration predicts higher all-cause mortality,
#      independent of chronological age, sex, race, BMI, and income."
#
# KEY REFERENCE:
#   Levine, M.E. et al. (2018). DOI: 10.18632/aging.101414 — original
#   survival validation of PhenoAge in NHANES 2007–2010.
#   We replicate and extend this with NHANES 2015–2020.
# =============================================================================

library(survival)
library(survminer)
library(broom)
library(survey)
library(tidyverse)


#' Merge NHANES Biomarker Data with Mortality Linkage File
#'
#' Joins the BioClockR analysis dataset (with PhenoAge and age acceleration)
#' with the NHANES-linked mortality file.
#'
#' @param nhanes_with_accel NHANES data frame with phenoage and age_accel_resid.
#' @param mortality_data Mortality data frame from 02_download_nhanes_mortality.R.
#'
#' @return Merged data frame with survival outcome columns added.
#'
#' @export
merge_nhanes_mortality <- function(nhanes_with_accel, mortality_data) {
  
  merged <- nhanes_with_accel %>%
    inner_join(
      mortality_data %>% select(seqn, died, follow_up_yrs, cod_category),
      by = c("SEQN" = "seqn")
    )
  
  message(paste0("Merged survival dataset: ", nrow(merged), " participants"))
  message(paste0("Deaths: ", sum(merged$died, na.rm = TRUE),
                 " (", round(mean(merged$died, na.rm = TRUE) * 100, 1), "%)"))
  message(paste0("Median follow-up: ",
                 round(median(merged$follow_up_yrs, na.rm = TRUE), 1), " years"))
  
  return(merged)
}


#' Kaplan-Meier Survival Curves by Age Acceleration Quartile
#'
#' Stratifies NHANES participants into quartiles of biological age
#' acceleration and computes Kaplan-Meier survival curves.
#' The hypothesis: Q4 (highest acceleration) should have lowest survival.
#'
#' @param survival_df Merged NHANES-mortality data frame.
#' @return A survminer ggsurvplot object.
#'
#' @export
km_by_age_acceleration <- function(survival_df) {
  
  df <- survival_df %>%
    filter(!is.na(age_accel_resid) & !is.na(died) & !is.na(follow_up_yrs)) %>%
    mutate(
      accel_quartile = ntile(age_accel_resid, 4),
      accel_q_label  = case_when(
        accel_quartile == 1 ~ "Q1 — Biologically youngest",
        accel_quartile == 2 ~ "Q2",
        accel_quartile == 3 ~ "Q3",
        accel_quartile == 4 ~ "Q4 — Biologically oldest",
        TRUE ~ NA_character_
      )
    )
  
  # Kaplan-Meier fit
  km_fit <- survfit(
    Surv(follow_up_yrs, died) ~ accel_q_label,
    data = df
  )
  
  # Log-rank test
  log_rank_p <- surv_pvalue(km_fit, data = df)$pval
  
  # KM plot using survminer
  km_plot <- ggsurvplot(
    km_fit,
    data          = df,
    risk.table    = TRUE,
    pval          = TRUE,
    conf.int      = TRUE,
    xlim          = c(0, 15),
    xlab          = "Follow-up Time (years)",
    ylab          = "Survival Probability",
    title         = "All-Cause Survival by Biological Age Acceleration Quartile",
    subtitle      = "BioClockR — NHANES 2015–2020 linked to mortality through Dec 2019",
    legend.labs   = c("Q1 (Youngest)", "Q2", "Q3", "Q4 (Oldest)"),
    legend.title  = "Age Acceleration Quartile",
    palette       = c("#2166AC", "#4DAF4A", "#FF7F00", "#D6604D"),
    ggtheme       = theme_minimal(base_size = 12),
    caption       = "NHANES public-use linked mortality file. CDC/NCHS."
  )
  
  message(paste0("Log-rank test p-value: ", formatC(log_rank_p, digits = 3, format = "e")))
  
  return(km_plot)
}


#' Cox Proportional Hazards Model: Age Acceleration → All-Cause Mortality
#'
#' Tests whether biological age acceleration independently predicts
#' all-cause mortality after adjusting for:
#' - Chronological age
#' - Sex
#' - Race/ethnicity
#' - BMI
#' - Poverty income ratio (socioeconomic status)
#'
#' Runs in a NHANES survey-weighted Cox model using the survey package.
#'
#' @param survival_df Merged NHANES-mortality data frame.
#' @return A tibble of hazard ratios with 95% CIs and p-values.
#'
#' @export
cox_age_acceleration <- function(survival_df) {
  
  df <- survival_df %>%
    filter(
      !is.na(age_accel_resid) & !is.na(died) & !is.na(follow_up_yrs) &
      !is.na(age_years) & !is.na(sex) & !is.na(race_label) &
      !is.na(bmi) & !is.na(pir) &
      follow_up_yrs > 0
    ) %>%
    mutate(
      # Standardise age acceleration for 5-year HR interpretation
      age_accel_5yr = age_accel_resid / 5,
      sex_f         = factor(sex, levels = c(1, 2), labels = c("Male", "Female")),
      race_f        = factor(race_label)
    )
  
  message(paste0("Cox model sample: ", nrow(df), " participants"))
  
  # ── Model 1: Age acceleration only (unadjusted + age) ─────────────────────
  cox_m1 <- coxph(
    Surv(follow_up_yrs, died) ~ age_accel_5yr + age_years,
    data    = df,
    weights = weight
  )
  
  # ── Model 2: Fully adjusted ────────────────────────────────────────────────
  cox_m2 <- coxph(
    Surv(follow_up_yrs, died) ~ age_accel_5yr + age_years +
      sex_f + race_f + bmi + pir,
    data    = df,
    weights = weight
  )
  
  # ── Extract results using broom ────────────────────────────────────────────
  results_m1 <- tidy(cox_m1, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(model = "Model 1: Age-adjusted")
  
  results_m2 <- tidy(cox_m2, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(model = "Model 2: Fully adjusted")
  
  results <- bind_rows(results_m1, results_m2) %>%
    filter(term == "age_accel_5yr") %>%
    select(model, term, estimate, conf.low, conf.high, p.value) %>%
    rename(
      HR       = estimate,
      CI_low   = conf.low,
      CI_high  = conf.high,
      p_value  = p.value
    ) %>%
    mutate(
      interpretation = paste0(
        "HR = ", round(HR, 2),
        " (95% CI: ", round(CI_low, 2), "–", round(CI_high, 2), ")",
        " per 5-year biological age acceleration"
      )
    )
  
  message("\n=== Cox Model Results: Age Acceleration → All-Cause Mortality ===")
  message("Per 5-year biological age acceleration:")
  results %>%
    select(model, HR, CI_low, CI_high, p_value) %>%
    print()
  
  # ── PH assumption check ────────────────────────────────────────────────────
  ph_test <- cox.zph(cox_m2)
  message("\nProportional Hazards Assumption (Schoenfeld residuals):")
  print(ph_test)
  
  return(list(
    model_1      = cox_m1,
    model_2      = cox_m2,
    hr_table     = results,
    ph_test      = ph_test
  ))
}


#' Forest Plot of Cox Hazard Ratios
#'
#' Visualises hazard ratios from the adjusted Cox model.
#'
#' @param cox_results Output from \code{cox_age_acceleration()}.
#' @return A ggplot2 forest plot.
#'
#' @export
plot_cox_forest <- function(cox_results) {
  
  hr_data <- tidy(
    cox_results$model_2,
    exponentiate = TRUE,
    conf.int     = TRUE
  ) %>%
    filter(!grepl("race_f|Intercept", term)) %>%
    mutate(
      term_label = case_when(
        term == "age_accel_5yr" ~ "Bio. Age Acceleration\n(per 5 yrs)",
        term == "age_years"     ~ "Chronological Age\n(per year)",
        term == "sex_fFemale"   ~ "Sex: Female vs. Male",
        term == "bmi"           ~ "BMI (per unit)",
        term == "pir"           ~ "Income (PIR per unit)",
        TRUE ~ term
      )
    )
  
  ggplot(hr_data, aes(x = estimate, y = reorder(term_label, estimate),
                      xmin = conf.low, xmax = conf.high)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
    geom_errorbarh(height = 0.25, color = "#1F4E79", linewidth = 0.9) +
    geom_point(aes(color = estimate > 1), size = 4) +
    scale_color_manual(values = c("TRUE" = "#C00000", "FALSE" = "#2E75B6"),
                       guide = "none") +
    scale_x_log10() +
    labs(
      title   = "Cox Proportional Hazards: All-Cause Mortality",
      subtitle = "Hazard Ratios — fully adjusted model (NHANES 2015-2020)",
      x       = "Hazard Ratio (log scale, 95% CI)",
      y       = NULL,
      caption = "Weighted Cox regression. NHANES linked mortality file."
    ) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold", color = "#1F4E79"))
}
