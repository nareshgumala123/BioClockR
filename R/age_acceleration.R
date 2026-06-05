# =============================================================================
# FILE:    R/age_acceleration.R
# PROJECT: BioClockR — Biological Age Estimation
# PHASE:   Phase 2–3
# AUTHOR:  Naresh Gumala, N2 Cloud Tech
# DATE:    April – June 2024
#
# PURPOSE:
#   Compute biological age acceleration and related population-level
#   statistics. Age acceleration is the key "actionable" output of
#   BioClockR — it tells you whether your body is aging faster or
#   slower than expected given your calendar age.
# =============================================================================

library(tidyverse)
library(survey)


#' Compute Age Acceleration
#'
#' Biological age acceleration = PhenoAge − chronological age.
#' Positive values mean biologically older than calendar age.
#' Negative values mean biologically younger.
#'
#' We also compute a \strong{residual-based} age acceleration (recommended for
#' population analyses) by regressing PhenoAge on chronological age and
#' using the residual. This removes the baseline age trend and better
#' captures health-related deviation from expected aging trajectory.
#'
#' @param df A data frame with columns \code{phenoage} and \code{age_years}.
#' @param method One of "simple" (phenoage - age) or "residual"
#'   (residual from OLS regression of phenoage ~ age). Default "residual".
#'
#' @return The input data frame with additional columns:
#'   \code{age_accel_simple}, \code{age_accel_resid}.
#'
#' @references
#'   Levine, M.E. et al. (2018). DOI: 10.18632/aging.101414
#'
#' @export
compute_age_acceleration <- function(df, method = "residual") {
  
  if (!"phenoage" %in% colnames(df)) {
    stop("Column 'phenoage' not found. Run compute_phenoage_df() first.")
  }
  if (!"age_years" %in% colnames(df)) {
    stop("Column 'age_years' not found.")
  }
  
  # Simple acceleration
  df <- df %>%
    mutate(age_accel_simple = phenoage - age_years)
  
  # Residual-based acceleration (recommended for group comparisons)
  # Regress PhenoAge on chronological age; residual = age-adjusted acceleration
  age_model <- lm(phenoage ~ age_years, data = df)
  df$age_accel_resid <- residuals(age_model)
  
  message(paste0(
    "Age acceleration computed.\n",
    "  Simple: mean = ", round(mean(df$age_accel_simple, na.rm = TRUE), 2),
    " yr, SD = ", round(sd(df$age_accel_simple, na.rm = TRUE), 2), "\n",
    "  Residual: mean ≈ 0 by construction, SD = ",
    round(sd(df$age_accel_resid, na.rm = TRUE), 2)
  ))
  
  return(df)
}


#' Stratify Age Acceleration by Demographic and Health Groups
#'
#' Computes weighted mean age acceleration (residual) by sex, race/ethnicity,
#' income group, and BMI category using NHANES survey design weights.
#'
#' @param df NHANES data frame with age_accel_resid, survey design variables.
#' @return A named list of summary tibbles, one per stratification variable.
#'
#' @export
stratify_age_acceleration <- function(df) {
  
  # Set up NHANES complex survey design
  # NHANES uses mobile examination center (MEC) weights for lab analyses
  svy_design <- svydesign(
    id      = ~psu,
    strata  = ~strata,
    weights = ~weight,
    data    = df,
    nest    = TRUE
  )
  
  # Helper: weighted mean + SE by group variable
  compute_group_means <- function(group_var) {
    formula_str <- paste0("~age_accel_resid")
    by_formula  <- as.formula(paste0("~", group_var))
    
    result <- svyby(
      formula  = ~age_accel_resid,
      by       = by_formula,
      design   = svy_design,
      FUN      = svymean,
      na.rm    = TRUE
    ) %>%
      as_tibble() %>%
      rename(
        group       = !!group_var,
        mean_accel  = age_accel_resid,
        se_accel    = se
      ) %>%
      mutate(
        ci_low   = mean_accel - 1.96 * se_accel,
        ci_high  = mean_accel + 1.96 * se_accel,
        group_var = group_var
      )
    
    return(result)
  }
  
  results <- list(
    by_sex    = compute_group_means("sex_label"),
    by_race   = compute_group_means("race_label"),
    by_income = compute_group_means("income_group")
  )
  
  # BMI category if available
  if ("bmi" %in% colnames(df)) {
    df_bmi <- df %>%
      filter(!is.na(bmi)) %>%
      mutate(bmi_cat = case_when(
        bmi < 18.5 ~ "Underweight",
        bmi < 25   ~ "Normal weight",
        bmi < 30   ~ "Overweight",
        bmi >= 30  ~ "Obese",
        TRUE       ~ NA_character_
      ))
    
    svy_bmi <- svydesign(
      id = ~psu, strata = ~strata, weights = ~weight,
      data = df_bmi, nest = TRUE
    )
    
    results$by_bmi <- svyby(
      formula = ~age_accel_resid,
      by      = ~bmi_cat,
      design  = svy_bmi,
      FUN     = svymean,
      na.rm   = TRUE
    ) %>%
      as_tibble() %>%
      rename(group = bmi_cat, mean_accel = age_accel_resid, se_accel = se) %>%
      mutate(ci_low = mean_accel - 1.96 * se_accel,
             ci_high = mean_accel + 1.96 * se_accel,
             group_var = "bmi_cat")
  }
  
  return(results)
}


#' Plot Age Acceleration Distribution
#'
#' Creates a ggplot2 density plot of biological age acceleration,
#' annotated with mean and SD.
#'
#' @param df Data frame with age_accel_resid column.
#' @param color Fill color. Default "#2E75B6".
#' @return A ggplot2 object.
#'
#' @export
plot_age_acceleration_dist <- function(df, color = "#2E75B6") {
  
  mean_accel <- mean(df$age_accel_resid, na.rm = TRUE)
  sd_accel   <- sd(df$age_accel_resid,   na.rm = TRUE)
  
  ggplot(df, aes(x = age_accel_resid)) +
    geom_density(fill = color, alpha = 0.6, color = "white") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +
    geom_vline(xintercept = mean_accel, linetype = "solid",
               color = "#C00000", linewidth = 0.8) +
    annotate("text",
             x     = mean_accel + 0.5,
             y     = Inf,
             label = paste0("Mean: ", round(mean_accel, 2), " yr"),
             vjust = 2, hjust = 0, color = "#C00000", size = 3.5) +
    labs(
      title    = "Distribution of Biological Age Acceleration",
      subtitle = "BioClockR — NHANES 2015–2020 (n ≈ 14,800)",
      x        = "Age Acceleration (years)\n[Positive = biologically older than calendar age]",
      y        = "Density",
      caption  = "Residual-based age acceleration (regression of PhenoAge on chronological age).\nLevine et al. (2018). DOI: 10.18632/aging.101414"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title    = element_text(face = "bold", color = "#1F4E79"),
      plot.subtitle = element_text(color = "grey40"),
      plot.caption  = element_text(color = "grey50", size = 9)
    )
}


#' Plot Age Acceleration by Group (Forest-Plot Style)
#'
#' @param stratification_results Output from \code{stratify_age_acceleration()}.
#' @param which_group One of "by_sex", "by_race", "by_income", "by_bmi".
#' @return A ggplot2 object.
#'
#' @export
plot_acceleration_by_group <- function(stratification_results, which_group = "by_race") {
  
  df_plot <- stratification_results[[which_group]]
  
  ggplot(df_plot, aes(x = mean_accel, y = reorder(group, mean_accel),
                      xmin = ci_low, xmax = ci_high)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
    geom_errorbarh(height = 0.2, color = "#1F4E79", linewidth = 0.8) +
    geom_point(size = 3, color = "#2E75B6") +
    labs(
      title   = paste0("Mean Biological Age Acceleration by ",
                       gsub("by_", "", which_group)),
      x       = "Mean Age Acceleration (years, 95% CI)",
      y       = NULL,
      caption = "Survey-weighted estimates. NHANES 2015-2020."
    ) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold", color = "#1F4E79"))
}
