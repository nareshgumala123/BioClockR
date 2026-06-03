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
    )
  )
)

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

shinyApp(ui = ui, server = server)
