# 01_download_nhanes.R
# Purpose: Download NHANES 2015-2020 lab modules needed for BioClockR
# Data source: CDC NHANES - https://www.cdc.gov/nchs/nhanes/index.htm

if (!requireNamespace("nhanesA", quietly = TRUE)) {
  install.packages("nhanesA")
}

library(nhanesA)
nhanesOptions(use.db = FALSE)

# ── 2015–2016 ──────────────────────────────────────────────────────────────

demo_15 <- nhanes("DEMO_I")
cbc_15  <- nhanes("CBC_I")
bmp_15  <- nhanes("BIOPRO_I")
crp_15  <- nhanes("HSCRP_I")
gluc_15 <- nhanes("GLU_I")
ghb_15  <- nhanes("GHB_I")

# ── 2017–2018 ──────────────────────────────────────────────────────────────

demo_17 <- nhanes("DEMO_J")
cbc_17  <- nhanes("CBC_J")
bmp_17  <- nhanes("BIOPRO_J")
crp_17  <- nhanes("HSCRP_J")
gluc_17 <- nhanes("GLU_J")
ghb_17  <- nhanes("GHB_J")

# ── 2019–2020 ──────────────────────────────────────────────────────────────

demo_19 <- nhanes("DEMO_L")
cbc_19  <- nhanes("CBC_L")
bmp_19  <- nhanes("BIOPRO_L")
crp_19  <- nhanes("HSCRP_L")
gluc_19 <- nhanes("GLU_L")
ghb_19  <- nhanes("GHB_L")

# ── Save raw downloads ─────────────────────────────────────────────────────

dir.create("data-raw/nhanes_raw", showWarnings = FALSE)

saveRDS(list(demo=demo_15, cbc=cbc_15, bmp=bmp_15,
             crp=crp_15,  gluc=gluc_15, ghb=ghb_15),
        "data-raw/nhanes_raw/nhanes_2015.rds")

saveRDS(list(demo=demo_17, cbc=cbc_17, bmp=bmp_17,
             crp=crp_17,  gluc=gluc_17, ghb=ghb_17),
        "data-raw/nhanes_raw/nhanes_2017.rds")

saveRDS(list(demo=demo_19, cbc=cbc_19, bmp=bmp_19,
             crp=crp_19,  gluc=gluc_19, ghb=ghb_19),
        "data-raw/nhanes_raw/nhanes_2019.rds")

message("✅ NHANES download complete. Files saved to data-raw/nhanes_raw/")

