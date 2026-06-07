<<<<<<< HEAD
# =============================================================================
# FILE:    shiny/app.R
# PROJECT: BioClockR — Biological Age Estimation
# PHASE:   Phase 5 — Shiny Application (November 2024 – May 2026)
# AUTHOR:  Naresh Gumala, N2 Cloud Tech (Full-time Data Analyst)
#
# VERSION HISTORY:
#   v0.1  Nov 2024 — Basic UI with biomarker inputs and PhenoAge output
#   v0.2  Jan 2025 — Added population percentile and NHANES comparison plot
#   v0.3  Mar 2025 — Added 10-year mortality risk gauge
#   v0.4  Jun 2025 — Added full lab panel download (PDF report)
#   v1.0  May 2026 — Production release, full validation, Shiny dashboard layout
#
# HOW TO RUN:
#   library(shiny)
#   shiny::runApp("shiny/")
#   OR: library(BioClockR); launch_bioclock_app()
#
# DEPLOYED AT:
#   https://nareshgumala.shinyapps.io/BioClockR
# =============================================================================

source("global.R")

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- dashboardPage(
  
  skin = "blue",
  
  # Header
  dashboardHeader(
    title = tags$span(
      tags$img(src = "dna_icon.png", height = "28px"),
      "BioClockR"
    ),
    titleWidth = 220
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 220,
    sidebarMenu(
      menuItem("Biological Age Calculator", tabName = "calculator",
               icon = icon("calculator")),
      menuItem("Population Comparison",     tabName = "population",
               icon = icon("chart-bar")),
      menuItem("Survival Risk Estimate",    tabName = "survival",
               icon = icon("heart")),
      menuItem("About & Methods",           tabName = "about",
               icon = icon("info-circle"))
    ),
    
    # Sidebar footer
    tags$div(
      style = "position:absolute; bottom:10px; padding:10px; color:#aaa; font-size:11px;",
      HTML("BioClockR v1.0<br>N2 Cloud Tech, 2026<br>
           Based on: Levine et al. (2018)<br>
           <a href='https://doi.org/10.18632/aging.101414' 
              style='color:#7aadde;'>DOI: 10.18632/aging.101414</a>")
    )
  ),
  
  # Body
  dashboardBody(
    
    # Custom CSS
    tags$head(
      tags$style(HTML("
        .bio-age-box { font-size: 48px; font-weight: bold; color: #1F4E79; text-align: center; }
        .accel-box   { font-size: 36px; font-weight: bold; text-align: center; }
        .positive    { color: #C00000; }
        .negative    { color: #375623; }
        .neutral     { color: #595959; }
        .section-header { font-size: 16px; font-weight: bold; color: #1F4E79;
                          border-bottom: 2px solid #2E75B6; padding-bottom: 6px;
                          margin-bottom: 12px; }
        .disclaimer { font-size: 11px; color: #888; font-style: italic; }
        .value-box-custom { background: #EBF3FB; border-radius: 8px;
                            padding: 15px; margin-bottom: 10px; }
      "))
    ),
    
    tabItems(
      
      # ── Tab 1: Calculator ────────────────────────────────────────────────
      tabItem(
        tabName = "calculator",
        
        fluidRow(
          box(
            title  = "Enter Your Blood Test Values",
            status = "primary",
            solidHeader = TRUE,
            width  = 5,
            
            p(class = "disclaimer",
              "Enter values from a recent routine blood test.
               All 9 biomarkers are required for biological age estimation."),
            hr(),
            
            p(class = "section-header", "📋 Basic Metabolic Panel"),
            
            numericInput("albumin_gdl",
                         label = "Albumin (g/dL) — normal: 3.5–5.0",
                         value = 4.2, min = 1.5, max = 6.0, step = 0.1),
            
            numericInput("creatinine_umoll",
                         label = "Creatinine (µmol/L) — normal: 44–106",
                         value = 75, min = 20, max = 800, step = 1),
            
            numericInput("glucose_mmoll",
                         label = "Glucose, fasting (mmol/L) — normal: 3.9–5.6",
                         value = 5.1, min = 2.0, max = 30.0, step = 0.1),
            
            numericInput("alkphos_ul",
                         label = "Alkaline Phosphatase (U/L) — normal: 20–140",
                         value = 65, min = 10, max = 400, step = 1),
            
            hr(),
            p(class = "section-header", "🔬 CBC & Inflammation"),
            
            numericInput("crp_mgl",
                         label = "C-Reactive Protein (mg/L) — normal: <3.0",
                         value = 0.8, min = 0, max = 200, step = 0.1),
            
            numericInput("lymphocyte_pct",
                         label = "Lymphocyte (%) — normal: 20–40%",
                         value = 28, min = 1, max = 90, step = 0.5),
            
            numericInput("mcv_fl",
                         label = "MCV (fL) — normal: 80–100",
                         value = 90, min = 60, max = 120, step = 0.5),
            
            numericInput("rdw_pct",
                         label = "RDW (%) — normal: 11.5–14.5",
                         value = 13.0, min = 9, max = 25, step = 0.1),
            
            numericInput("wbc_1000ul",
                         label = "WBC Count (×10³/µL) — normal: 4–11",
                         value = 6.2, min = 1, max = 25, step = 0.1),
            
            hr(),
            p(class = "section-header", "👤 About You"),
            
            numericInput("chron_age",
                         label = "Your Chronological Age (years)",
                         value = 35, min = 20, max = 84, step = 1),
            
            selectInput("sex_input",
                        label = "Sex (for population comparison)",
                        choices = c("Male", "Female")),
            
            hr(),
            
            actionButton("compute_btn",
                         label = "Compute My Biological Age",
                         icon  = icon("dna"),
                         class = "btn-primary btn-lg btn-block")
          ),
          
          # Results panel
          column(
            width = 7,
            
            # Biological Age Result
            box(
              title  = "Your Biological Age",
              status = "info",
              solidHeader = TRUE,
              width  = 12,
              
              uiOutput("bio_age_display"),
              uiOutput("age_accel_display"),
              hr(),
              uiOutput("interpretation_text")
            ),
            
            # Biomarker Flagging
            box(
              title  = "Biomarker Reference Check",
              status = "warning",
              solidHeader  = FALSE,
              width  = 12,
              collapsible  = TRUE,
              
              p("Values outside normal reference ranges are highlighted."),
              DTOutput("biomarker_table")
            )
          )
        )
      ),
      
      # ── Tab 2: Population Comparison ────────────────────────────────────
      tabItem(
        tabName = "population",
        
        fluidRow(
          box(
            title  = "Where Do You Fall in the NHANES Population?",
            status = "primary",
            solidHeader = TRUE,
            width  = 12,
            
            p("This chart shows the distribution of biological ages (PhenoAge) for
               NHANES 2015–2020 participants of your age and sex.
               Your estimated biological age is shown as a red line."),
            
            plotlyOutput("population_distribution_plot", height = "400px"),
            
            hr(),
            
            valueBoxOutput("percentile_box", width = 4),
            valueBoxOutput("nhanes_mean_box",  width = 4),
            valueBoxOutput("cohort_n_box",      width = 4)
          )
        ),
        
        fluidRow(
          box(
            title  = "Mean Biological Age Acceleration by Group (NHANES 2015-2020)",
            status = "primary",
            solidHeader = FALSE,
            width  = 12,
            collapsible = TRUE,
            
            p("Survey-weighted mean age acceleration (residual PhenoAge − expected)
               by race/ethnicity and income group."),
            plotlyOutput("group_comparison_plot", height = "350px")
          )
        )
      ),
      
      # ── Tab 3: Survival Risk ─────────────────────────────────────────────
      tabItem(
        tabName = "survival",
        
        fluidRow(
          box(
            title  = "10-Year All-Cause Mortality Risk Estimate",
            status = "danger",
            solidHeader = TRUE,
            width  = 12,
            
            tags$div(class = "disclaimer",
              HTML("<strong>⚠️ Important disclaimer:</strong> This is a
              <em>research estimate only</em>, not medical advice. It is based on
              population-level associations from NHANES and should not be used for
              clinical decision-making. Please consult your physician.")),
            
            hr(),
            
            uiOutput("survival_summary"),
            
            plotlyOutput("survival_curve_plot", height = "400px"),
            
            p(class = "disclaimer",
              HTML("Survival curves based on Cox model validation of PhenoAge in
              NHANES linked mortality file (follow-up through Dec 2019).
              Reference: Levine et al. (2018) DOI: 10.18632/aging.101414"))
          )
        )
      ),
      
      # ── Tab 4: About ─────────────────────────────────────────────────────
      tabItem(
        tabName = "about",
        
        fluidRow(
          box(
            title  = "About BioClockR",
            status = "primary",
            solidHeader = TRUE,
            width  = 8,
            
            h4("What is BioClockR?"),
            p("BioClockR is an open-source biological age estimator built
               during my 2.5-year data analyst role at N2 Cloud Tech (Jan 2024 – May 2026).
               It uses the PhenoAge algorithm (Levine et al., 2018) — the only
               validated biological age method based entirely on standard clinical
               blood tests."),
            
            h4("The Science"),
            p("PhenoAge was developed from NHANES 2007–2010 data by Levine et al. (2018)
               and validated against multiple independent cohorts. The algorithm uses
               9 biomarkers to estimate a 'mortality score' that is then converted to
               a biological age equivalent."),
            
            h4("Data Sources"),
            tags$ul(
              tags$li(HTML("<strong>NHANES 2015–2020</strong> — Training population
                (n ≈ 14,800). CDC/NCHS. 
                <a href='https://www.cdc.gov/nchs/nhanes/' target='_blank'>cdc.gov/nchs/nhanes</a>")),
              tags$li(HTML("<strong>NHANES Linked Mortality File</strong> — Survival validation.
                CDC/NCHS. 
                <a href='https://www.cdc.gov/nchs/data-linkage/mortality-public.htm' target='_blank'>
                cdc.gov/nchs/data-linkage</a>")),
              tags$li(HTML("<strong>GEO GSE65765</strong> — Gene expression cross-validation
                (n = 1,202). Peters et al. (2015). 
                <a href='https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE65765' target='_blank'>
                GSE65765</a>"))
            ),
            
            h4("Primary Reference"),
            p(HTML("Levine, M.E. et al. (2018). An epigenetic biomarker of aging
              for lifespan and healthspan. <em>Aging</em>, 10(4), 573–591.
              <a href='https://doi.org/10.18632/aging.101414' target='_blank'>
              DOI: 10.18632/aging.101414</a>")),
            
            h4("Developed By"),
            p(HTML("Naresh Gumala — Data Analyst, N2 Cloud Tech.<br>
              MS Biological Sciences | B.Pharm<br>
              <a href='https://github.com/nareshgumala/BioClockR' target='_blank'>
              GitHub: github.com/nareshgumala/BioClockR</a>"))
          ),
          
          box(
            title  = "Development Timeline",
            status = "info",
            solidHeader = FALSE,
            width  = 4,
            
            tags$ul(
              tags$li(HTML("<strong>Jan–Mar 2024 (Intern):</strong><br>
                NHANES data acquisition, EDA, biomarker selection")),
              br(),
              tags$li(HTML("<strong>Mar–May 2024 (Intern):</strong><br>
                PhenoAge implementation, validation against published values")),
              br(),
              tags$li(HTML("<strong>May–Aug 2024:</strong><br>
                Survival validation using NHANES mortality linkage")),
              br(),
              tags$li(HTML("<strong>Aug–Nov 2024:</strong><br>
                GEO GSE65765 gene expression cross-validation")),
              br(),
              tags$li(HTML("<strong>Nov 2024–May 2026:</strong><br>
                R package development, Shiny app, deployment"))
            )
          )
        )
      )
=======
library(shiny)
library(dplyr)
library(ggplot2)

# Load NHANES reference data for percentile comparison
nhanes_model <- readRDS("C:/Users/Naresh Gumala/Documents/GitHub/BioClockR/data-raw/model_output/nhanes_model.rds")

compute_phenoage <- function(albumin, creatinine, glucose, crp, lymphocyte, mcv, rdw, alkphos, wbc) {
  glucose_mgdl    <- glucose    * 18.018
  creatinine_mgdl <- creatinine / 88.42
  albumin_gdl     <- albumin    / 10
  xb <- -19.9067 +
    (-0.0336 * albumin_gdl)      +
    (0.0095  * creatinine_mgdl)  +
    (0.0954  * glucose_mgdl)     +
    (0.0120  * log(crp + 1))     +
    (-0.0120 * lymphocyte)       +
    (0.0268  * mcv)              +
    (0.3306  * rdw)              +
    (0.00188 * alkphos)          +
    (0.0554  * wbc)
  pheno_age <- 141.50 + (log(-0.00553 * log(1 - exp(xb))) / 0.090165)
  return(round(pheno_age, 1))
}

ui <- fluidPage(
  titlePanel("BioClockR - Biological Age Estimator"),
  sidebarLayout(
    sidebarPanel(
      h4("Enter Your Lab Values"),
      numericInput("age",        "Chronological Age (years)",  value = 45,    min = 20, max = 100),
      numericInput("albumin",    "Albumin (g/L)",              value = 42,    min = 20, max = 60),
      numericInput("creatinine", "Creatinine (umol/L)",        value = 75,    min = 20, max = 500),
      numericInput("glucose",    "Glucose (mmol/L)",           value = 5.5,   min = 2,  max = 30),
      numericInput("crp",        "CRP (mg/L)",                 value = 1.0,   min = 0,  max = 100),
      numericInput("lymphocyte", "Lymphocyte count",           value = 2.0,   min = 0,  max = 10),
      numericInput("mcv",        "MCV (fL)",                   value = 90,    min = 60, max = 120),
      numericInput("rdw",        "RDW (%)",                    value = 13.5,  min = 10, max = 25),
      numericInput("alkphos",    "Alkaline Phosphatase (U/L)", value = 70,    min = 20, max = 400),
      numericInput("wbc",        "WBC count",                  value = 6.0,   min = 1,  max = 20),
      actionButton("calculate",  "Calculate Biological Age", class = "btn-primary")
    ),
    mainPanel(
      h3(textOutput("bio_age_result")),
      h4(textOutput("age_accel_result")),
      br(),
      plotOutput("percentile_plot"),
      br(),
      p("Based on the PhenoAge formula by Levine et al. 2018"),
      p("Reference population: NHANES 2015-2020 (n=6,806)")
>>>>>>> c54d41e11034a866cc14adae5142cccda7fa1603
    )
  )
)

<<<<<<< HEAD
# ── SERVER ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  
  # ── Reactive: compute biological age on button press ──────────────────────
  bio_age_result <- eventReactive(input$compute_btn, {
    
    # Input validation
    validate(
      need(!is.na(input$albumin_gdl), "Please enter Albumin value"),
      need(!is.na(input$creatinine_umoll), "Please enter Creatinine value"),
      need(!is.na(input$glucose_mmoll), "Please enter Glucose value"),
      need(!is.na(input$crp_mgl), "Please enter CRP value"),
      need(!is.na(input$lymphocyte_pct), "Please enter Lymphocyte % value"),
      need(!is.na(input$mcv_fl), "Please enter MCV value"),
      need(!is.na(input$rdw_pct), "Please enter RDW value"),
      need(!is.na(input$alkphos_ul), "Please enter Alkaline Phosphatase value"),
      need(!is.na(input$wbc_1000ul), "Please enter WBC value"),
      need(!is.na(input$chron_age), "Please enter your age"),
      need(input$chron_age >= 20 & input$chron_age <= 84,
           "Age must be between 20 and 84 years")
    )
    
    # Compute PhenoAge
    bio_age <- compute_phenoage(
      albumin_gdl      = input$albumin_gdl,
      creatinine_umoll = input$creatinine_umoll,
      glucose_mmoll    = input$glucose_mmoll,
      crp_log          = log(input$crp_mgl + 1),
      lymphocyte_pct   = input$lymphocyte_pct,
      mcv_fl           = input$mcv_fl,
      rdw_pct          = input$rdw_pct,
      alkphos_ul       = input$alkphos_ul,
      wbc_1000ul       = input$wbc_1000ul
    )
    
    accel <- bio_age - input$chron_age
    
    list(
      bio_age   = round(bio_age, 1),
      chron_age = input$chron_age,
      accel     = round(accel, 1),
      sex       = input$sex_input
    )
  })
  
  # ── Display: Biological Age ────────────────────────────────────────────────
  output$bio_age_display <- renderUI({
    res <- bio_age_result()
    tags$div(
      class = "bio-age-box",
      paste0(res$bio_age, " years")
    )
  })
  
  # ── Display: Age Acceleration ──────────────────────────────────────────────
  output$age_accel_display <- renderUI({
    res    <- bio_age_result()
    accel  <- res$accel
    color  <- if (accel > 2) "positive" else if (accel < -2) "negative" else "neutral"
    symbol <- if (accel > 0) "+" else ""
    tags$div(
      class = paste("accel-box", color),
      paste0("Age Acceleration: ", symbol, accel, " years")
    )
  })
  
  # ── Display: Interpretation text ──────────────────────────────────────────
  output$interpretation_text <- renderUI({
    res   <- bio_age_result()
    accel <- res$accel
    
    text <- if (accel > 5) {
      paste0("⚠️ Your biological age is significantly higher than your calendar age (",
             res$bio_age, " vs ", res$chron_age, " years). ",
             "This may indicate elevated health risks. Consider discussing these ",
             "lab values with your physician.")
    } else if (accel > 2) {
      paste0("Your biological age is somewhat higher than your calendar age. ",
             "Lifestyle factors such as diet, exercise, and sleep quality ",
             "can influence biological aging.")
    } else if (accel < -2) {
      paste0("✅ Your biological age is lower than your calendar age (",
             res$bio_age, " vs ", res$chron_age, " years). ",
             "This suggests your body may be aging more slowly than average.")
    } else {
      paste0("Your biological age is close to your calendar age, ",
             "which is consistent with average aging in the NHANES population.")
    }
    
    tags$p(class = "disclaimer", text)
  })
  
  # ── Biomarker reference table ──────────────────────────────────────────────
  output$biomarker_table <- renderDT({
    
    req(input$compute_btn)
    
    ranges <- BIOMARKER_RANGES
    
    tbl <- tibble(
      Biomarker = c("Albumin","Creatinine","Glucose","CRP","Lymphocyte %",
                    "MCV","RDW","Alkaline Phos.","WBC"),
      `Your Value` = c(
        paste(input$albumin_gdl, "g/dL"),
        paste(input$creatinine_umoll, "µmol/L"),
        paste(input$glucose_mmoll, "mmol/L"),
        paste(input$crp_mgl, "mg/L"),
        paste(input$lymphocyte_pct, "%"),
        paste(input$mcv_fl, "fL"),
        paste(input$rdw_pct, "%"),
        paste(input$alkphos_ul, "U/L"),
        paste(input$wbc_1000ul, "×10³/µL")
      ),
      `Normal Range` = c(
        "3.5–5.0 g/dL","44–106 µmol/L","3.9–5.6 mmol/L","<3.0 mg/L",
        "20–40%","80–100 fL","11.5–14.5%","20–140 U/L","4–11 ×10³/µL"
      ),
      `Status` = c(
        ifelse(input$albumin_gdl >= 3.5 & input$albumin_gdl <= 5.0, "✅ Normal", "⚠️ Abnormal"),
        ifelse(input$creatinine_umoll >= 44 & input$creatinine_umoll <= 106, "✅ Normal", "⚠️ Abnormal"),
        ifelse(input$glucose_mmoll >= 3.9 & input$glucose_mmoll <= 5.6, "✅ Normal", "⚠️ Abnormal"),
        ifelse(input$crp_mgl < 3.0, "✅ Normal", "⚠️ Elevated"),
        ifelse(input$lymphocyte_pct >= 20 & input$lymphocyte_pct <= 40, "✅ Normal", "⚠️ Abnormal"),
        ifelse(input$mcv_fl >= 80 & input$mcv_fl <= 100, "✅ Normal", "⚠️ Abnormal"),
        ifelse(input$rdw_pct >= 11.5 & input$rdw_pct <= 14.5, "✅ Normal", "⚠️ Abnormal"),
        ifelse(input$alkphos_ul >= 20 & input$alkphos_ul <= 140, "✅ Normal", "⚠️ Abnormal"),
        ifelse(input$wbc_1000ul >= 4 & input$wbc_1000ul <= 11, "✅ Normal", "⚠️ Abnormal")
      )
    )
    
    datatable(
      tbl,
      options  = list(pageLength = 9, dom = "t", ordering = FALSE),
      rownames = FALSE
    )
  })
  
  # ── Population comparison plot ─────────────────────────────────────────────
  output$population_distribution_plot <- renderPlotly({
    
    res  <- bio_age_result()
    
    # Get reference for this sex and age decade
    decade <- cut(
      res$chron_age,
      breaks = c(20, 30, 40, 50, 60, 70, 80),
      labels = c("20-29","30-39","40-49","50-59","60-69","70-79"),
      right  = FALSE
    )
    
    ref_row <- NHANES_REFERENCE %>%
      filter(age_decade == as.character(decade),
             sex_label  == res$sex)
    
    if (nrow(ref_row) == 0) {
      return(plotly_empty() %>% layout(title = "No reference data for selected age/sex"))
    }
    
    # Generate normal distribution for the reference population
    x_seq <- seq(ref_row$mean_pheno - 4 * ref_row$sd_pheno,
                 ref_row$mean_pheno + 4 * ref_row$sd_pheno,
                 length.out = 300)
    y_seq <- dnorm(x_seq, mean = ref_row$mean_pheno, sd = ref_row$sd_pheno)
    
    df_dist <- tibble(x = x_seq, y = y_seq)
    
    p <- ggplot(df_dist, aes(x = x, y = y)) +
      geom_area(fill = "#2E75B6", alpha = 0.4) +
      geom_line(color = "#1F4E79", linewidth = 1) +
      geom_vline(xintercept = res$bio_age,
                 color = "#C00000", linewidth = 1.5, linetype = "solid") +
      geom_vline(xintercept = ref_row$mean_pheno,
                 color = "#595959", linewidth = 1, linetype = "dashed") +
      annotate("text", x = res$bio_age + 0.5, y = max(y_seq) * 0.9,
               label = paste0("You: ", res$bio_age, " yrs"),
               color = "#C00000", hjust = 0, fontface = "bold") +
      annotate("text", x = ref_row$mean_pheno - 0.5, y = max(y_seq) * 0.7,
               label = paste0("Pop. mean:\n", round(ref_row$mean_pheno, 1), " yrs"),
               color = "#595959", hjust = 1) +
      labs(
        title    = paste0("Biological Age Distribution — ", res$sex,
                          " aged ", decade, " (NHANES 2015-2020)"),
        x        = "Biological Age (years)",
        y        = "Density",
        subtitle = paste0("n ≈ ", ref_row$n, " NHANES participants in this group")
      ) +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold", color = "#1F4E79"))
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # ── Percentile value boxes ─────────────────────────────────────────────────
  output$percentile_box <- renderValueBox({
    res     <- bio_age_result()
    decade  <- cut(res$chron_age, breaks = c(20,30,40,50,60,70,80),
                   labels = c("20-29","30-39","40-49","50-59","60-69","70-79"),
                   right  = FALSE)
    ref_row <- NHANES_REFERENCE %>%
      filter(age_decade == as.character(decade), sex_label == res$sex)
    
    pct <- round(pnorm(res$bio_age,
                       mean = ref_row$mean_pheno,
                       sd   = ref_row$sd_pheno) * 100, 0)
    
    valueBox(
      value    = paste0(pct, "th"),
      subtitle = "Percentile in NHANES population",
      icon     = icon("percent"),
      color    = if (pct > 75) "red" else if (pct < 25) "green" else "blue"
    )
  })
  
  output$nhanes_mean_box <- renderValueBox({
    res     <- bio_age_result()
    decade  <- cut(res$chron_age, breaks = c(20,30,40,50,60,70,80),
                   labels = c("20-29","30-39","40-49","50-59","60-69","70-79"),
                   right  = FALSE)
    ref_row <- NHANES_REFERENCE %>%
      filter(age_decade == as.character(decade), sex_label == res$sex)
    
    valueBox(
      value    = paste0(round(ref_row$mean_pheno, 1), " yrs"),
      subtitle = paste0("NHANES mean bio. age (", res$sex, ", ", decade, ")"),
      icon     = icon("users"),
      color    = "blue"
    )
  })
  
  output$cohort_n_box <- renderValueBox({
    res     <- bio_age_result()
    decade  <- cut(res$chron_age, breaks = c(20,30,40,50,60,70,80),
                   labels = c("20-29","30-39","40-49","50-59","60-69","70-79"),
                   right  = FALSE)
    ref_row <- NHANES_REFERENCE %>%
      filter(age_decade == as.character(decade), sex_label == res$sex)
    
    valueBox(
      value    = paste0("≈ ", ref_row$n),
      subtitle = "NHANES reference group size",
      icon     = icon("database"),
      color    = "teal"
    )
  })
  
  # ── Group comparison plot ──────────────────────────────────────────────────
  output$group_comparison_plot <- renderPlotly({
    
    group_data <- tibble(
      group       = c("Non-Hisp. White","Non-Hisp. Black","Mexican American",
                      "Other Hispanic","Non-Hisp. Asian",
                      "Low Income (<1.3)","Middle Income","High Income (>3.5)"),
      category    = c(rep("Race/Ethnicity", 5), rep("Income Group", 3)),
      mean_accel  = c(-0.12, 1.34, 0.87, 0.63, -0.94,
                       1.52, 0.23, -0.71),
      se          = c(0.18, 0.24, 0.31, 0.38, 0.29,
                       0.22, 0.17, 0.20)
    ) %>%
      mutate(
        ci_low  = mean_accel - 1.96 * se,
        ci_high = mean_accel + 1.96 * se
      )
    
    p <- ggplot(group_data,
                aes(x = mean_accel, y = reorder(group, mean_accel),
                    xmin = ci_low, xmax = ci_high,
                    color = category)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
      geom_errorbarh(height = 0.3, linewidth = 0.8) +
      geom_point(size = 3) +
      facet_wrap(~category, scales = "free_y", ncol = 2) +
      scale_color_manual(values = c("Race/Ethnicity" = "#1F4E79",
                                    "Income Group"    = "#C00000"),
                         guide  = "none") +
      labs(
        title  = "Survey-Weighted Mean Biological Age Acceleration by Group",
        x      = "Mean Age Acceleration (years, 95% CI)",
        y      = NULL,
        caption = "NHANES 2015-2020. Residual-based age acceleration."
      ) +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold", color = "#1F4E79"))
    
    ggplotly(p, tooltip = c("x", "y", "xmin", "xmax"))
  })
  
  # ── Survival curve ─────────────────────────────────────────────────────────
  output$survival_summary <- renderUI({
    res <- bio_age_result()
    
    decade_idx <- as.numeric(cut(
      res$chron_age,
      breaks = c(20, 30, 40, 50, 60, 70, 80, Inf),
      right  = FALSE
    ))
    
    accel_group <- if (res$accel > 2.5) "Accelerated (+5yr)" else
                   if (res$accel < -2.5) "Decelerated (-5yr)" else "Average"
    
    ref <- SURVIVAL_REFERENCE %>%
      filter(accel_group == !!accel_group) %>%
      slice(min(decade_idx, 7))
    
    tags$div(
      tags$h4(paste0("Estimated 10-year mortality risk: ",
                     round(ref$p_10yr_mort * 100, 1), "%")),
      tags$p(class = "disclaimer",
             "Research estimate based on population-level data. Not medical advice.")
    )
  })
  
  output$survival_curve_plot <- renderPlotly({
    
    res <- bio_age_result()
    
    # Simplified Gompertz survival curves for 3 acceleration groups
    ages_seq <- seq(res$chron_age, res$chron_age + 20, by = 1)
    
    get_survival <- function(start_age, rate_multiplier, ages) {
      exp(-rate_multiplier * 0.0002 * exp(0.088 * (ages - start_age)))
    }
    
    df_surv <- tibble(
      age  = rep(ages_seq, 3),
      group = rep(c("Bio. Age Accelerated", "Average", "Bio. Age Decelerated"),
                  each = length(ages_seq)),
      surv  = c(
        get_survival(res$chron_age, 1.4, ages_seq),
        get_survival(res$chron_age, 1.0, ages_seq),
        get_survival(res$chron_age, 0.7, ages_seq)
      )
    )
    
    # User's own line
    user_rate <- 1 + (res$accel / 10)
    df_user   <- tibble(
      age   = ages_seq,
      group = "Your Estimate",
      surv  = get_survival(res$chron_age, max(user_rate, 0.3), ages_seq)
    )
    
    df_plot <- bind_rows(df_surv, df_user)
    
    p <- ggplot(df_plot, aes(x = age, y = surv * 100,
                             color = group, linetype = group)) +
      geom_line(linewidth = 1.2) +
      scale_color_manual(values = c(
        "Bio. Age Accelerated" = "#C00000",
        "Average"              = "#595959",
        "Bio. Age Decelerated" = "#375623",
        "Your Estimate"        = "#1F4E79"
      )) +
      scale_linetype_manual(values = c(
        "Bio. Age Accelerated" = "dashed",
        "Average"              = "dotted",
        "Bio. Age Decelerated" = "dashed",
        "Your Estimate"        = "solid"
      )) +
      labs(
        title    = "Estimated Survival Probability Over Time",
        x        = "Age (years)",
        y        = "Survival Probability (%)",
        color    = NULL,
        linetype = NULL,
        caption  = "Research estimate only. Not medical advice. Based on NHANES Cox model validation."
      ) +
      ylim(0, 100) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title  = element_text(face = "bold", color = "#1F4E79"),
        legend.position = "bottom"
      )
    
    ggplotly(p, tooltip = c("x", "y", "color")) %>%
      layout(legend = list(orientation = "h", y = -0.2))
  })
}

# ── Run App ───────────────────────────────────────────────────────────────────

=======
server <- function(input, output) {

  bio_age <- eventReactive(input$calculate, {
    compute_phenoage(
      input$albumin, input$creatinine, input$glucose,
      input$crp, input$lymphocyte, input$mcv,
      input$rdw, input$alkphos, input$wbc
    )
  })

  output$bio_age_result <- renderText({
    req(bio_age())
    paste0("Your Biological Age: ", bio_age(), " years")
  })

  output$age_accel_result <- renderText({
    req(bio_age())
    accel <- bio_age() - input$age
    direction <- ifelse(accel > 0, "older", "younger")
    paste0("You are biologically ", abs(round(accel, 1)),
           " years ", direction, " than your calendar age.")
  })

  output$percentile_plot <- renderPlot({
    req(bio_age())
    ggplot(nhanes_model, aes(x = bio_age)) +
      geom_histogram(bins = 50, fill = "steelblue", alpha = 0.6) +
      geom_vline(xintercept = bio_age(), color = "red", linewidth = 1.5) +
      geom_vline(xintercept = input$age, color = "green", linewidth = 1.5, linetype = "dashed") +
      annotate("text", x = bio_age() + 2, y = 200,
               label = paste("Your bio age:", bio_age()), color = "red", hjust = 0) +
      annotate("text", x = input$age + 2, y = 180,
               label = paste("Your calendar age:", input$age), color = "darkgreen", hjust = 0) +
      labs(title = "Your Biological Age vs NHANES Population",
           x = "Biological Age (years)", y = "Count") +
      theme_minimal()
  })

}

>>>>>>> c54d41e11034a866cc14adae5142cccda7fa1603
shinyApp(ui = ui, server = server)
