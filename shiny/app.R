
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(DT)
library(tidyverse)

# PhenoAge function
compute_phenoage <- function(albumin_gdl, creatinine_umoll, glucose_mmoll,
                              crp_log, lymphocyte_pct, mcv_fl, rdw_pct,
                              alkphos_ul, wbc_1000ul) {
  xb <- -4.0107 +
    (-0.0336 * albumin_gdl) +
    (0.0095  * creatinine_umoll) +
    (-0.1953 * glucose_mmoll) +
    (0.0954  * crp_log) +
    (-0.0120 * lymphocyte_pct) +
    (0.0268  * mcv_fl) +
    (0.3306  * rdw_pct) +
    (0.00188 * alkphos_ul) +
    (0.0554  * wbc_1000ul)
  mort_score <- 1 - exp(-0.00553 * exp(xb) / 0.090165)
  phenoage   <- log(-log(1 - mort_score) / 0.00553) / 0.090165
  return(round(phenoage, 1))
}

# UI
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "BioClockR"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Calculator",  tabName = "calc",    icon = icon("calculator")),
      menuItem("About",       tabName = "about",   icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "calc",
        fluidRow(
          box(title="Enter Your Blood Test Values", status="primary",
              solidHeader=TRUE, width=5,
              numericInput("albumin",    "Albumin (g/dL) — normal 3.5-5.0",        4.2, min=1.5, max=6.0,   step=0.1),
              numericInput("creatinine", "Creatinine (µmol/L) — normal 44-106",    75,  min=20,  max=1200,  step=1),
              numericInput("glucose",    "Glucose (mmol/L) — normal 3.9-5.6",      5.1, min=1.0, max=40.0,  step=0.1),
              numericInput("alkphos",    "Alkaline Phosphatase (U/L) — normal 20-140", 65, min=10, max=1000, step=1),
              numericInput("crp",        "CRP (mg/L) — normal <3.0",               0.8, min=0,   max=200,   step=0.1),
              numericInput("lymphocyte", "Lymphocyte (%) — normal 20-40",          28,  min=1,   max=99,    step=0.5),
              numericInput("mcv",        "MCV (fL) — normal 80-100",               90,  min=50,  max=150,   step=0.5),
              numericInput("rdw",        "RDW (%) — normal 11.5-14.5",             13,  min=9,   max=30,    step=0.1),
              numericInput("wbc",        "WBC (x10³/µL) — normal 4-11",            6.2, min=1,   max=50,    step=0.1),
              numericInput("chron_age",  "Your Chronological Age (years)",          35,  min=20,  max=84,    step=1),
              selectInput("sex", "Sex", choices=c("Male","Female")),
              actionButton("compute", "Compute My Biological Age",
                           class="btn-primary btn-lg btn-block")
          ),
          column(width=7,
            box(title="Your Results", status="info", solidHeader=TRUE, width=12,
              uiOutput("result_display")
            ),
            box(title="What This Means", status="warning", solidHeader=FALSE, width=12,
              uiOutput("interpretation")
            ),
            box(title="Biomarker Status", status="primary", solidHeader=FALSE, width=12,
              DTOutput("biomarker_table")
            )
          )
        )
      ),
      tabItem(tabName = "about",
        box(title="About BioClockR", status="primary", solidHeader=TRUE, width=8,
          h4("What is BioClockR?"),
          p("BioClockR estimates your biological age from routine blood test values
             using the PhenoAge algorithm (Levine et al., 2018)."),
          h4("Data Sources"),
          tags$ul(
            tags$li("NHANES 2015-2020 — CDC/NCHS"),
            tags$li("NHANES Linked Mortality File — CDC/NCHS"),
            tags$li("GEO GSE30272 — Brain aging gene expression")
          ),
          h4("Reference"),
          p("Levine ME et al. (2018). An epigenetic biomarker of aging.
             Aging, 10(4), 573-591. DOI: 10.18632/aging.101414"),
          h4("Developer"),
          p("Naresh Gumala — Data Analyst, N2 Cloud Tech (Jan 2024 - Present)")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {

  result <- eventReactive(input$compute, {
    bio_age  <- compute_phenoage(
      albumin_gdl      = input$albumin,
      creatinine_umoll = input$creatinine,
      glucose_mmoll    = input$glucose,
      crp_log          = log(input$crp + 1),
      lymphocyte_pct   = input$lymphocyte,
      mcv_fl           = input$mcv,
      rdw_pct          = input$rdw,
      alkphos_ul       = input$alkphos,
      wbc_1000ul       = input$wbc
    )
    accel <- bio_age - input$chron_age
    list(bio_age=bio_age, accel=round(accel,1), chron_age=input$chron_age)
  })

  output$result_display <- renderUI({
    req(result())
    r <- result()
    color <- if (r$accel > 2) "#C00000" else if (r$accel < -2) "#375623" else "#1F4E79"
    symbol <- if (r$accel > 0) "+" else ""
    tagList(
      tags$div(style=paste0("font-size:52px; font-weight:bold; color:#1F4E79; text-align:center;"),
               paste0(r$bio_age, " years")),
      tags$div(style=paste0("font-size:28px; font-weight:bold; color:", color, "; text-align:center;"),
               paste0("Age Acceleration: ", symbol, r$accel, " years"))
    )
  })

  output$interpretation <- renderUI({
    req(result())
    r <- result()
    text <- if (r$accel > 5) {
      "⚠️ Your biological age is significantly higher than your calendar age. Consider discussing these lab values with your physician."
    } else if (r$accel > 2) {
      "Your biological age is somewhat higher than your calendar age. Lifestyle factors like diet, exercise and sleep can influence biological aging."
    } else if (r$accel < -2) {
      "✅ Your biological age is lower than your calendar age. Your body appears to be aging more slowly than average."
    } else {
      "Your biological age is close to your calendar age — consistent with average aging."
    }
    tags$p(text, style="font-size:14px; color:#595959;")
  })

  output$biomarker_table <- renderDT({
    req(input$compute)
    tibble(
      Biomarker = c("Albumin","Creatinine","Glucose","CRP",
                    "Lymphocyte%","MCV","RDW","Alkaline Phos.","WBC"),
      `Your Value` = c(
        paste(input$albumin,    "g/dL"),
        paste(input$creatinine, "µmol/L"),
        paste(input$glucose,    "mmol/L"),
        paste(input$crp,        "mg/L"),
        paste(input$lymphocyte, "%"),
        paste(input$mcv,        "fL"),
        paste(input$rdw,        "%"),
        paste(input$alkphos,    "U/L"),
        paste(input$wbc,        "x10³/µL")
      ),
      `Normal Range` = c(
        "3.5-5.0","44-106","3.9-5.6","<3.0",
        "20-40","80-100","11.5-14.5","20-140","4-11"
      ),
      Status = c(
        ifelse(input$albumin>=3.5    & input$albumin<=5.0,    "✅ Normal","⚠️ Abnormal"),
        ifelse(input$creatinine>=44  & input$creatinine<=106, "✅ Normal","⚠️ Abnormal"),
        ifelse(input$glucose>=3.9    & input$glucose<=5.6,    "✅ Normal","⚠️ Abnormal"),
        ifelse(input$crp<3.0,                                 "✅ Normal","⚠️ Elevated"),
        ifelse(input$lymphocyte>=20  & input$lymphocyte<=40,  "✅ Normal","⚠️ Abnormal"),
        ifelse(input$mcv>=80         & input$mcv<=100,        "✅ Normal","⚠️ Abnormal"),
        ifelse(input$rdw>=11.5       & input$rdw<=14.5,       "✅ Normal","⚠️ Abnormal"),
        ifelse(input$alkphos>=20     & input$alkphos<=140,    "✅ Normal","⚠️ Abnormal"),
        ifelse(input$wbc>=4          & input$wbc<=11,         "✅ Normal","⚠️ Abnormal")
      )
    ) %>%
      datatable(options=list(pageLength=9, dom="t", ordering=FALSE), rownames=FALSE)
  })
}

shinyApp(ui=ui, server=server)

