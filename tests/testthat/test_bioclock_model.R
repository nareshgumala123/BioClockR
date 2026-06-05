library(testthat)

test_that("compute_phenoage returns a numeric value", {
  result <- compute_phenoage(
    albumin_gdl=4.2, creatinine_umoll=75, glucose_mmoll=5.1,
    crp_log=log(0.8+1), lymphocyte_pct=28, mcv_fl=90,
    rdw_pct=13.0, alkphos_ul=65, wbc_1000ul=6.2
  )
  expect_true(is.numeric(result))
  expect_false(is.na(result))
})

test_that("healthy 30-year-old gets biological age < 35", {
  bio_age <- compute_phenoage(
    albumin_gdl=4.5, creatinine_umoll=70, glucose_mmoll=4.8,
    crp_log=log(0.5+1), lymphocyte_pct=32, mcv_fl=88,
    rdw_pct=12.5, alkphos_ul=60, wbc_1000ul=5.8
  )
  expect_lt(bio_age, 35)
})

test_that("elevated biomarkers produce higher biological age", {
  healthy   <- compute_phenoage(4.5,70,4.8,log(0.5+1),32,88,12.5,60,5.8)
  unhealthy <- compute_phenoage(3.2,200,9.0,log(15+1),15,98,18.0,200,12.0)
  expect_gt(unhealthy, healthy)
})
