library(shiny)
library(ggplot2)
library(gridExtra)
library(brms)
library(tidyverse)
library(bayesplot)

# Load preprocessed objects
df <- readRDS("framingham_cleaned.rds")
heart_model <- readRDS("heart_model.rds")
y_pred_mean <- readRDS("predicted_probabilities.rds")
y_pred <- readRDS("binary_predictions.rds")
cont_plots <- readRDS("cont_plots.rds")
bin_plots_counts <- readRDS("bin_plots_counts.rds")
top_features_summary <- readRDS("top_feature_summary.rds")

# UI
ui <- fluidPage(
  titlePanel("Bayesian Heart Disease Predictor - Framingham Study"),
  tabsetPanel(
    tabPanel("Background & Exploratory Analysis",
             h3("Framingham Heart Study"),
             p("The Framingham Heart Study is one of the most influential epidemiological studies in medical history. Initiated in 1948 in Framingham, Massachusetts, this multi-generational study has followed thousands of participants to identify risk factors associated with cardiovascular disease (CVD). It was the first to establish connections between high blood pressure, cholesterol, smoking, and heart disease, shaping how we understand and manage cardiac risk today."),
             p("This app leverages a subset of Framingham data to predict the probability of developing coronary heart disease (CHD) using Bayesian logistic regression. We rely on clinically validated features like cholesterol levels, blood pressure, BMI, and smoking status to estimate personalized risk."),
             p("The dataset includes the following predictors:"),
             tags$ul(
               tags$li(strong("AGE:"), " Age of the participant (years)"),
               tags$li(strong("SEX:"), " Biological sex (0 = Female, 1 = Male)"),
               tags$li(strong("TOTCHOL:"), " Total cholesterol level (mg/dL)"),
               tags$li(strong("SYSBP / DIABP:"), " Systolic and Diastolic Blood Pressure (mmHg)"),
               tags$li(strong("CURSMOKE / CIGPDAY:"), " Smoking status and number of cigarettes smoked per day"),
               tags$li(strong("BMI:"), " Body Mass Index (weight/height²)"),
               tags$li(strong("GLUCOSE:"), " Glucose level (mg/dL), an indicator of diabetes risk"),
               tags$li(strong("BPMEDS:"), " Whether the participant is on blood pressure medication"),
               tags$li(strong("HEARTRTE:"), " Resting heart rate (beats per minute)"),
               tags$li(strong("PREVCHD / PREVMI / PREVAP / PREVSTRK / PREVHYP:"), 
                       " History of coronary heart disease, myocardial infarction, angina, stroke, or hypertension"),
               br(),
               p("The target variable is ", strong("ANYCHD"), ", indicating whether the participant developed coronary heart disease during the study period.")
             ),
             
             h4("Histograms by CHD Status (Continuous Variables)"),
             fluidRow(plotOutput("contPlot1")),
             fluidRow(plotOutput("contPlot2")),
             fluidRow(plotOutput("contPlot3")),
             fluidRow(plotOutput("contPlot4")),
             
             h4("Binary Variable Distributions by CHD Status"),
             fluidRow(plotOutput("binPlot1")),
             fluidRow(plotOutput("binPlot2")),
             fluidRow(plotOutput("binPlot3"))
    ),
    
    tabPanel("Your CHD Risk Prediction",
             sidebarLayout(
               sidebarPanel(
                 h4("Enter Your Info"),
                 numericInput("AGE", "Age", value = 50, min = 20, max = 100),
                 numericInput("TOTCHOL", "Total Cholesterol", 200),
                 numericInput("SYSBP", "Systolic BP", 120),
                 numericInput("DIABP", "Diastolic BP", 80),
                 numericInput("CIGPDAY", "Cigarettes per Day", 0),
                 numericInput("BMI", "Body Mass Index", 25),
                 numericInput("HEARTRTE", "Heart Rate", 70),
                 numericInput("GLUCOSE", "Glucose", 90),
                 selectInput("SEX", "Sex", choices = c("Female" = 0, "Male" = 1)),
                 selectInput("CURSMOKE", "Current Smoker", choices = c("No" = 0, "Yes" = 1)),
                 selectInput("DIABETES", "Diabetes", choices = c("No" = 0, "Yes" = 1)),
                 selectInput("BPMEDS", "On BP Meds", choices = c("No" = 0, "Yes" = 1)),
                 selectInput("PREVCHD", "Prior CHD", choices = c("No" = 0, "Yes" = 1)),
                 selectInput("PREVAP", "Prior Angina", choices = c("No" = 0, "Yes" = 1)),
                 selectInput("PREVMI", "Prior MI", choices = c("No" = 0, "Yes" = 1)),
                 selectInput("PREVSTRK", "Prior Stroke", choices = c("No" = 0, "Yes" = 1)),
                 selectInput("PREVHYP", "Prior Hypertension", choices = c("No" = 0, "Yes" = 1)),
                 actionButton("predict_btn", "Predict CHD Risk")
               ),
               mainPanel(
                 h4("Prediction Output"),
                 textOutput("predMean"),
                 textOutput("ciText"),
                 plotOutput("predHist")
               )
             )
    ),
    
    tabPanel("Feature Insights",
             h4("Top Features Most Correlated with CHD Risk"),
             verbatimTextOutput("topFeatures")
    )
  )
)

# Server
server <- function(input, output) {
  # Continuous histograms (8 total)
  output$contPlot1 <- renderPlot({ grid.arrange(grobs = cont_plots[1:2], ncol = 2) })
  output$contPlot2 <- renderPlot({ grid.arrange(grobs = cont_plots[3:4], ncol = 2) })
  output$contPlot3 <- renderPlot({ grid.arrange(grobs = cont_plots[5:6], ncol = 2) })
  output$contPlot4 <- renderPlot({ grid.arrange(grobs = cont_plots[7:8], ncol = 2) })
  
  # Binary bar plots (9 total)
  output$binPlot1 <- renderPlot({ grid.arrange(grobs = bin_plots_counts[1:3], ncol = 3) })
  output$binPlot2 <- renderPlot({ grid.arrange(grobs = bin_plots_counts[4:6], ncol = 3) })
  output$binPlot3 <- renderPlot({ grid.arrange(grobs = bin_plots_counts[7:9], ncol = 3) })
  
  # Prediction
  observeEvent(input$predict_btn, {
    new_data <- tibble(
      AGE = scale(input$AGE, center = attr(df$AGE, "scaled:center"), scale = attr(df$AGE, "scaled:scale")),
      TOTCHOL = scale(input$TOTCHOL, center = attr(df$TOTCHOL, "scaled:center"), scale = attr(df$TOTCHOL, "scaled:scale")),
      SYSBP = scale(input$SYSBP, center = attr(df$SYSBP, "scaled:center"), scale = attr(df$SYSBP, "scaled:scale")),
      DIABP = scale(input$DIABP, center = attr(df$DIABP, "scaled:center"), scale = attr(df$DIABP, "scaled:scale")),
      CIGPDAY = scale(input$CIGPDAY, center = attr(df$CIGPDAY, "scaled:center"), scale = attr(df$CIGPDAY, "scaled:scale")),
      BMI = scale(input$BMI, center = attr(df$BMI, "scaled:center"), scale = attr(df$BMI, "scaled:scale")),
      HEARTRTE = scale(input$HEARTRTE, center = attr(df$HEARTRTE, "scaled:center"), scale = attr(df$HEARTRTE, "scaled:scale")),
      GLUCOSE = scale(input$GLUCOSE, center = attr(df$GLUCOSE, "scaled:center"), scale = attr(df$GLUCOSE, "scaled:scale")),
      SEX = as.numeric(input$SEX),
      CURSMOKE = as.numeric(input$CURSMOKE),
      DIABETES = as.numeric(input$DIABETES),
      BPMEDS = as.numeric(input$BPMEDS),
      PREVCHD = as.numeric(input$PREVCHD),
      PREVAP = as.numeric(input$PREVAP),
      PREVMI = as.numeric(input$PREVMI),
      PREVSTRK = as.numeric(input$PREVSTRK),
      PREVHYP = as.numeric(input$PREVHYP)
    )
    
    post_pred <- posterior_epred(heart_model, newdata = new_data)
    mean_prob <- round(mean(post_pred), 3)
    ci <- round(quantile(post_pred, probs = c(0.025, 0.975)), 3)
    
    output$predMean <- renderText({
      paste("Your predicted probability of developing CHD is:", mean_prob)
    })
    
    output$ciText <- renderText({
      paste("95% Credible Interval:", paste0("[", ci[1], ", ", ci[2], "]"))
    })
    
    output$predHist <- renderPlot({
      df_post <- data.frame(prob = as.numeric(post_pred))
      ggplot(df_post, aes(x = prob)) +
        geom_histogram(fill = "skyblue", color = "white", bins = 30) +
        geom_vline(xintercept = mean_prob, color = "blue", size = 1.2) +
        geom_vline(xintercept = ci, linetype = "dashed", color = "red") +
        labs(
          title = "Posterior Probability of CHD",
          x = "Predicted CHD Risk",
          y = "Frequency",
          subtitle = paste("Mean =", mean_prob, ", 95% CI = [", ci[1], ", ", ci[2], "]")
        ) +
        theme_minimal(base_size = 14)
    })
  })
  
  output$topFeatures <- renderPrint({
    cat(top_features_summary, sep = "\n")
  })

}

shinyApp(ui = ui, server = server)
