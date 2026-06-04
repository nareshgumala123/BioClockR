# BioClockR — Complete Project Documentation

> A Clinical Biological Age Estimator Using NHANES Public Data and the PhenoAge Formula
> **Author:** Naresh Gumala | Data Analyst | N2 Cloud Tech

---

## Table of Contents
1. [What is BioClockR?](#1-what-is-bioclockr)
2. [Data Sources](#2-data-sources)
3. [The PhenoAge Formula](#3-the-phenoage-formula)
4. [Project Structure](#4-project-structure)
5. [The Four Analysis Phases](#5-the-four-analysis-phases)
6. [The Shiny App](#6-the-shiny-app)
7. [Key Results](#7-key-results)
8. [Interview Q&A](#8-interview-qa)
9. [Technical Skills Demonstrated](#9-technical-skills-demonstrated)
10. [How to Run the Project](#10-how-to-run-the-project)
11. [Reference](#11-reference)

---

## 1. What is BioClockR?

BioClockR is an R-based data science project that estimates a person's **biological age**
using routine clinical blood test values. Unlike expensive DNA methylation clocks that cost
$500+ per sample, BioClockR uses only standard lab values available from any routine blood panel.

The core idea is simple: two people can be the same chronological age (say, 50 years old)
but have very different biological ages based on how their body is actually aging at the
cellular and systemic level. BioClockR quantifies this difference.

The project implements the published **PhenoAge formula** by Levine et al. (2018), validates
it on a large national dataset (NHANES), tests it against real mortality outcomes, and wraps
it in an interactive Shiny web app that anyone can use by entering their lab values.

### Why This Project Matters

Most biological age tools require expensive laboratory tests. BioClockR demonstrates that you
can estimate biological age for free using routine clinical data. This has practical applications for:

- Health-tech startups building wellness platforms
- Insurance companies assessing health risk
- Clinical researchers studying population aging
- Anyone curious about their own biological age

---

## 2. Data Sources

| Dataset | What It Contains | How We Use It | Source |
|---|---|---|---|
| NHANES 2015-2020 | Blood test results, demographics for ~30,000 Americans | Build and test the biological age model (n=6,806) | CDC.gov |
| NHANES Linked Mortality | Death records linked to NHANES participants | Validate biological age against real survival outcomes (n=2,094) | NCHS/CDC |
| GEO Gene Expression | Blood transcriptomics aging data | Cross-validate aging signatures against inflammatory pathways | NCBI GEO |

### What is NHANES?

NHANES stands for **National Health and Nutrition Examination Survey**. It is run by the CDC
and surveys thousands of Americans every two years. Participants get physical exams and blood
tests. All data is made publicly available for researchers.

We downloaded 6 modules per survey cycle:
- **Demographics** (age, sex, race)
- **Complete Blood Count (CBC)**
- **Metabolic Panel** (albumin, creatinine)
- **CRP** (inflammation marker)
- **Fasting Glucose**
- **HbA1c**

---

## 3. The PhenoAge Formula

The heart of BioClockR is the PhenoAge formula, published by Morgan Levine and colleagues
in 2018 in the journal *Aging* (doi:10.18632/aging.101414).

### The 9 Biomarkers

| Biomarker | What It Measures | Units |
|---|---|---|
| Albumin | Liver and nutritional health | g/dL |
| Creatinine | Kidney function | mg/dL |
| Glucose | Blood sugar / metabolic stress | mg/dL |
| CRP (log) | Systemic inflammation | log(mg/L + 1) |
| Lymphocyte count | Immune system health | 1000 cells/uL |
| MCV | Red blood cell size | fL |
| RDW | Red blood cell size variation | % |
| Alkaline Phosphatase | Liver and bone health | U/L |
| WBC count | White blood cells / immune response | 1000 cells/uL |

### Step 1 — Compute linear score (xb)

```r
xb <- -19.9067 +
  (-0.0336 * albumin_gdl)      +
  ( 0.0095 * creatinine_mgdl)  +
  ( 0.0954 * glucose_mgdl)     +
  ( 0.0120 * crp_log)          +
  (-0.0120 * lymphocyte)       +
  ( 0.0268 * mcv)              +
  ( 0.3306 * rdw)              +
  ( 0.00188 * alkphos)         +
  ( 0.0554 * wbc)
```

### Step 2 — Convert to biological age

```r
biological_age <- 141.50 + (log(-0.00553 * log(1 - exp(xb))) / 0.090165)
```

### Unit Conversions Applied
- Albumin: g/L → g/dL (divide by 10)
- Creatinine: umol/L → mg/dL (divide by 88.42)
- Glucose: mmol/L → mg/dL (multiply by 18.018)

---

## 4. Project Structure

```
BioClockR/
├── .github/workflows/
│   └── r-check.yml              # GitHub Actions CI
├── R/
│   ├── nhanes_cleaning.R        # Data cleaning and merging
│   ├── bioclock_model.R         # PhenoAge computation
│   └── survival_validation.R   # Cox model and Kaplan-Meier
├── analysis/
│   ├── phase1_eda.Rmd           # Exploratory data analysis
│   ├── phase2_model_building.Rmd
│   ├── phase3_seer_validation.Rmd
│   └── phase4_geo_validation.Rmd
├── shiny/
│   └── app.R                    # Interactive biological age estimator
├── docs/
│   └── PROJECT_DOCUMENTATION.md # This file
├── data-raw/
│   └── 01_download_nhanes.R     # Data download scripts
├── DESCRIPTION                  # R package metadata
└── README.md
```

---

## 5. The Four Analysis Phases

### Phase 1 — Exploratory Data Analysis

After downloading and cleaning NHANES data, the EDA report produces four key visualizations:

- Biological age vs chronological age scatter plot
- Age acceleration by sex
- Age acceleration by race/ethnicity
- Distribution of age acceleration across the population

**Key finding:** Mean biological age acceleration in the NHANES population is **+1.6 years**.

### Phase 2 — Model Building Report

Formally documents the model with:

- Distribution of all 9 biomarkers
- How each biomarker changes with age
- Biological age acceleration by glucose status (normal, pre-diabetic, diabetic)
- Summary statistics by sex

**Key finding:** Diabetic participants show significantly higher biological age acceleration.

### Phase 3 — Survival Validation

Links NHANES participants to death records from the NCHS Linked Mortality File.

- **Survival cohort:** 2,094 participants, 73 deaths, 3.5% death rate
- **Cox Proportional Hazards Model:** Age (HR=1.08/year, p<0.001) and sex (males 65% higher risk, p=0.035) are significant predictors
- **Concordance index:** 0.83 — excellent discrimination
- **Kaplan-Meier curves:** Biologically older vs younger survival comparison

**Honest finding:** Biological age acceleration itself was not statistically significant (p=0.68)
in this dataset due to the short (~4 year) follow-up period. This is a defensible, transparent result.

### Phase 4 — Gene Expression Aging Validation

Uses NHANES biomarkers as proxies for known aging gene expression pathways:

- **Inflammaging Score:** CRP + WBC as proxies for TNF-alpha/IL-6 pathway activity
- **Metabolic Aging Score:** Glucose + RDW as proxies for metabolic stress pathways
- **Combined Aging Score:** Variation across race/ethnicity groups

---

## 6. The Shiny App

### How to Run

```r
setwd("C:/Users/Naresh Gumala/Documents/GitHub/BioClockR")
shiny::runApp("shiny/app.R")
```

### What the App Does

1. User enters 9 lab values plus chronological age
2. Clicks **Calculate Biological Age**
3. App displays estimated biological age in years
4. App shows whether user is biologically older or younger than calendar age
5. App shows a histogram of the NHANES population with the user's value marked

---

## 7. Key Results

| Metric | Value | Interpretation |
|---|---|---|
| Sample size (clean) | 6,806 participants | Adults 20+ with complete biomarker data |
| Mean chronological age | 50.8 years | Average age of participants |
| Mean biological age | 52.4 years | Participants are biologically older on average |
| Mean age acceleration | +1.6 years | Americans age ~1.6 years faster biologically |
| Survival cohort | 2,094 participants | Linked to mortality records |
| Deaths | 73 (3.5%) | Over ~4 years follow-up |
| Cox concordance | 0.83 | Excellent survival discrimination |
| Male mortality HR | 1.65 (p=0.035) | Males 65% higher mortality risk |
| Age HR per year | 1.08 (p<0.001) | Each year of age = 8% higher mortality risk |

---

## 8. Interview Q&A

**Q: What data did you use?**
> NHANES 2015-2020 from CDC — nationally representative survey of ~30,000 Americans with routine blood tests. Also used NHANES Linked Mortality File for survival validation. All data is free and publicly available.

**Q: Is the formula yours?**
> No — I implemented and validated the PhenoAge formula by Levine et al. (2018). What is novel is the R package, Shiny app, NHANES validation, and survival analysis.

**Q: What is biological age acceleration?**
> The difference between biological age and chronological age. Positive = aging faster than average. Negative = aging more slowly.

**Q: Why does biological age not significantly predict mortality in your Cox model?**
> The follow-up period is only ~4 years, which is too short. Longer follow-up studies (UK Biobank, 10+ years) consistently show biological age as a significant mortality predictor. I report this honestly.

**Q: Why does this matter for health-tech?**
> DNA methylation clocks cost $500/sample. BioClockR uses routine blood work costing $20-30. For large clinical trials or wellness platforms, this makes biological age estimation scalable and affordable.

**Q: What R packages did you use?**
> nhanesA, dplyr, ggplot2, survival, survminer, shiny, rmarkdown, tidyr, BiocManager (GEOquery, limma)

---

## 9. Technical Skills Demonstrated

| Skill | How It Shows in BioClockR |
|---|---|
| R programming | All analysis in R: cleaning, modeling, visualization, Shiny |
| Data wrangling (dplyr) | Merging 6 NHANES modules across 3 cycles, unit conversions |
| Statistical modeling | Implementing published formula, Cox regression |
| Data visualization (ggplot2) | 15+ professional plots across 4 reports |
| Survival analysis | Cox model, Kaplan-Meier, hazard ratios, concordance |
| Public health data | NHANES survey design, CDC data linkage files |
| R Markdown | 4 reproducible HTML reports |
| Shiny app development | Interactive web app with reactive outputs |
| Git / GitHub | Version controlled with meaningful commits |
| R package structure | DESCRIPTION, organized R/ folder, GitHub Actions CI |
| Scientific communication | Honest null results, proper citations, reproducible methods |

---

## 10. How to Run the Project

```r
# Step 1 - Install dependencies
install.packages(c("nhanesA", "dplyr", "ggplot2", "survival",
                   "survminer", "shiny", "tidyr", "rmarkdown"))

# Step 2 - Download data from CDC
source("data-raw/01_download_nhanes.R")

# Step 3 - Clean data
source("R/nhanes_cleaning.R")

# Step 4 - Compute biological age
source("R/bioclock_model.R")

# Step 5 - Run survival analysis
source("R/survival_validation.R")

# Step 6 - Render reports
rmarkdown::render("analysis/phase1_eda.Rmd")
rmarkdown::render("analysis/phase2_model_building.Rmd")
rmarkdown::render("analysis/phase3_seer_validation.Rmd")
rmarkdown::render("analysis/phase4_geo_validation.Rmd")

# Step 7 - Launch Shiny app
shiny::runApp("shiny/app.R")
```

---

## 11. Reference

Levine ME et al. (2018). An epigenetic biomarker of aging for lifespan and healthspan.
*Aging (Albany NY)*. doi:10.18632/aging.101414

NHANES: https://www.cdc.gov/nchs/nhanes/

NHANES Linked Mortality: https://www.cdc.gov/nchs/data-linkage/mortality-public.htm

---

*BioClockR — Built by Naresh Gumala | github.com/nareshgumala123/BioClockR*
