# BioClockR

A clinical biological age estimator using NHANES public data and the PhenoAge formula.

## What is this?

BioClockR estimates your **biological age** from routine blood test values using the
PhenoAge formula (Levine et al. 2018). Unlike expensive DNA methylation clocks ($500+
per sample), BioClockR uses only standard lab values available from any routine blood panel.

## Data Sources

| Dataset | Use | Source |
|---|---|---|
| NHANES 2015-2020 | Reference population (n=6,806) | CDC |
| NHANES Linked Mortality | Survival validation (n=2,094) | NCHS |

## Methods

- Biological age computed using the PhenoAge formula (Levine et al. 2018)
- 9 biomarkers: albumin, creatinine, glucose, CRP, lymphocyte, MCV, RDW, alkaline phosphatase, WBC
- Survival validation using Cox proportional hazards model
- Kaplan-Meier curves comparing biologically older vs younger participants

## Results

- Mean biological age acceleration: +1.6 years in NHANES population
- Cox model concordance: 0.83
- Males have 65% higher mortality risk than females (HR=1.65, p=0.035)

## Repository Structure

```
BioClockR/
├── R/                        # Core R scripts
│   ├── nhanes_cleaning.R     # Data cleaning and merging
│   ├── bioclock_model.R      # PhenoAge computation
│   └── survival_validation.R # Cox model and Kaplan-Meier
├── analysis/
│   └── phase1_eda.Rmd        # Exploratory data analysis report
├── shiny/
│   └── app.R                 # Interactive biological age estimator
└── data-raw/                 # Download scripts (data not included)
```

## How to Run

```r
# Install dependencies
install.packages(c("nhanesA", "dplyr", "ggplot2", "survival", "survminer", "shiny"))

# Download data
source("data-raw/01_download_nhanes.R")

# Clean data
source("R/nhanes_cleaning.R")

# Compute biological age
source("R/bioclock_model.R")

# Run survival analysis
source("R/survival_validation.R")

# Launch Shiny app
shiny::runApp("shiny/app.R")
```

## Reference

Levine ME et al. (2018). An epigenetic biomarker of aging for lifespan and healthspan.
*Aging*. doi:10.18632/aging.101414

## Author

Naresh Gumala | Data Analyst | [GitHub](https://github.com/nareshgumala123)
