library(dplyr)
library(survival)
library(survminer)

parse_mortality <- function(filepath) {
  lines <- readLines(filepath)
  lines <- lines[grepl("^[0-9]", lines)]
  data.frame(
    SEQN       = as.numeric(substr(lines, 1,  5)),
    eligstat   = as.numeric(substr(lines, 15, 15)),
    mortstat   = as.numeric(trimws(substr(lines, 16, 16))),
    permth_int = as.numeric(trimws(substr(lines, 43, 44))),
    permth_exm = as.numeric(trimws(substr(lines, 46, 47)))
  )
}

mort_2015 <- parse_mortality("data-raw/mortality/mortality_2015.dat")
mort_2017 <- parse_mortality("data-raw/mortality/mortality_2017.dat")
mortality <- bind_rows(mort_2015, mort_2017)

nhanes_model <- readRDS("data-raw/model_output/nhanes_model.rds")

df <- nhanes_model %>%
  inner_join(mortality, by = "SEQN") %>%
  filter(eligstat == 1) %>%
  mutate(
    dead = ifelse(mortstat == 1, 1, 0),
    months = ifelse(!is.na(permth_exm), permth_exm, permth_int),
    age_accel_group = ifelse(age_accel > 0, "Biologically Older", "Biologically Younger")
  ) %>%
  filter(!is.na(months) & months > 0)

cat("Total participants:", nrow(df), "
")
cat("Deaths:", sum(df$dead), "
")

cox_model <- coxph(
  Surv(months, dead) ~ age_accel + age + sex + race,
  data = df
)

print(summary(cox_model))

km_fit <- survfit(Surv(months, dead) ~ age_accel_group, data = df)

km_plot <- ggsurvplot(
  km_fit, data = df, pval = TRUE, conf.int = TRUE,
  palette = c("#E64B35", "#4DBBD5"),
  legend.labs = c("Biologically Older", "Biologically Younger"),
  title = "Survival by Biological Age Acceleration",
  xlab = "Months", ylab = "Survival Probability"
)

print(km_plot)

saveRDS(df, "data-raw/model_output/survival_data.rds")
saveRDS(cox_model, "data-raw/model_output/cox_model.rds")
message("✅ Survival analysis complete")
