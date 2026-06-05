# BioClockR

<!-- badges -->
![R-CMD-check](https://github.com/nareshgumala/BioClockR/workflows/R-CMD-check/badge.svg)
![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)
![NHANES](https://img.shields.io/badge/Data-NHANES%202015--2020-blue)
![Version](https://img.shields.io/badge/version-1.0.0-green)

---

## Overview

**BioClockR** is an open-source R package and Shiny web application that estimates
**biological age** from routine clinical blood test values — entirely free, using only
publicly available data.

Most biological age clocks (e.g., Horvath's epigenetic clock) require expensive
DNA methylation arrays (~$500 per sample), making them inaccessible for small clinics,
health-tech startups, or individual researchers.

BioClockR implements the **PhenoAge algorithm** (Levine et al., 2018) using only
9 standard blood biomarkers routinely collected in any clinical setting, validated
against **real population-level mortality data** from the NHANES–linked mortality file
and **gene expression aging signatures** from GEO.

---

## Scientific Foundation

This project is built on and validates:

| Reference | What we use from it |
|-----------|-------------------|
| Levine et al. (2018). *Aging*, 10(4). DOI: 10.18632/aging.101414 | PhenoAge formula and biomarker coefficients |
| NHANES 2015–2020. CDC/NCHS. cdc.gov/nchs/nhanes | Training population (n ≈ 14,800) |
| NHANES Public-Use Linked Mortality File. CDC. cdc.gov/nchs/data-linkage/mortality-public.htm | Survival outcome validation |
| GEO GSE65765. NCBI GEO. ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE65765 | Gene expression aging validation (n = 1,202) |
| GEO GSE40279. NCBI GEO. ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE40279 | Horvath methylation reference comparison |

---

## What BioClockR Does

1. **Computes biological age** from 9 blood biomarkers using the PhenoAge formula
2. **Computes age acceleration** (biological age minus chronological age)
3. **Validates** age acceleration against real all-cause mortality (Cox regression, NHANES mortality linkage)
4. **Cross-validates** against blood gene expression aging signatures (GEO GSE65765)
5. **Provides a Shiny app** where anyone can enter their own lab values and get their biological age estimate with population percentile

---

## Biomarkers Required

| Biomarker | Unit | NHANES Variable |
|-----------|------|----------------|
| Albumin | g/dL | LBDSALSI |
| Creatinine | µmol/L | LBDSCRLSI |
| Glucose (fasting) | mmol/L | LBDGLUSI |
| C-Reactive Protein (log) | mg/L | LBXCRP |
| Lymphocyte % | % | LBDLYMNO |
| Mean Corpuscular Volume (MCV) | fL | LBXMCVSI |
| Red Cell Distribution Width (RDW) | % | LBXRDW |
| Alkaline Phosphatase | U/L | LBXSAPSI |
| White Blood Cell Count | 1000 cells/µL | LBXWBCSI |

---

## Project Timeline

This project was developed over 2.5 years at N2 Cloud Tech:

| Phase | Period | Focus |
|-------|--------|-------|
| Phase 1 | Jan 2024 – Mar 2024 (Intern) | NHANES data acquisition and EDA |
| Phase 2 | Mar 2024 – May 2024 (Intern) | PhenoAge model implementation |
| Phase 3 | May 2024 – Aug 2024 (Full-time) | Survival validation (NHANES mortality linkage) |
| Phase 4 | Aug 2024 – Nov 2024 | Gene expression cross-validation (GEO) |
| Phase 5 | Nov 2024 – May 2026 | R package, Shiny app, final polishing |

---

## Installation

```r
# Install from GitHub
# install.packages("devtools")
devtools::install_github("nareshgumala/BioClockR")
```

---

## Quick Start

```r
library(BioClockR)

# Compute biological age for one person
my_labs <- data.frame(
  albumin_gdl      = 4.2,
  creatinine_umoll = 75,
  glucose_mmoll    = 5.1,
  crp_mgl          = 0.8,
  lymphocyte_pct   = 28,
  mcv_fl           = 90,
  rdw_pct          = 13.0,
  alkphos_ul       = 65,
  wbc_1000ul       = 6.2
)

result <- compute_phenoage(my_labs)
print(result)
# Biological age: 34.2 years

accel <- age_acceleration(bio_age = result, chron_age = 32)
print(accel)
# Age acceleration: +2.2 years (biologically older than calendar age)
```

---

## Running the Shiny App

```r
library(BioClockR)
launch_bioclock_app()
```

Or visit the deployed app at: **https://nareshgumala.shinyapps.io/BioClockR**

---

## Repository Structure

```
BioClockR/
├── README.md
├── DESCRIPTION
├── NAMESPACE
├── LICENSE
├── .github/
│   └── workflows/
│       └── r-check.yml
├── data-raw/
│   ├── 01_download_nhanes.R
│   ├── 02_download_nhanes_mortality.R
│   └── 03_download_geo.R
├── R/
│   ├── nhanes_cleaning.R
│   ├── biomarker_selection.R
│   ├── bioclock_model.R
│   ├── age_acceleration.R
│   ├── survival_validation.R
│   └── geo_validation.R
├── analysis/
│   ├── phase1_eda.Rmd
│   ├── phase2_model_building.Rmd
│   ├── phase3_survival_validation.Rmd
│   ├── phase4_geo_validation.Rmd
│   └── phase5_shiny_final.Rmd
├── shiny/
│   ├── app.R
│   └── global.R
└── tests/
    └── testthat/
        └── test_bioclock_model.R
```

---

## License

MIT License. See LICENSE file.

---

## Author

**Naresh Gumala**
Data Analyst, N2 Cloud Tech
MS Biological Sciences | B.Pharm
GitHub: github.com/nareshgumala

---

## Citation

If you use BioClockR, please cite:

> Gumala, N. (2026). BioClockR: An open-source R package for biological age
> estimation using routine clinical biomarkers. N2 Cloud Tech.
> github.com/nareshgumala/BioClockR

And the underlying PhenoAge method:

> Levine, M.E. et al. (2018). An epigenetic biomarker of aging for lifespan and
> healthspan. *Aging*, 10(4), 573-591. DOI: 10.18632/aging.101414
