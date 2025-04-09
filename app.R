## Shiny App for Bayesian Heart Disease Prediction

library(shiny)
library(brms)
library(tidyverse)
library(bayesplot)
library(GGally)

# Load precomputed model and data
heart_model <- readRDS("heart_model.rds")
df <- readRDS("framingham_cleaned.rds")

numeric_vars <- c("AGE", "TOTCHOL", "GLUCOSE", "DIABP", "CIGPDAY", "BMI", "HEARTRTE")

# UI
ui <- fluidPage(
  titlePanel("Bayesian Heart Disease Prediction App"),
  tabsetPanel(
    tabPanel("Overview",
             h3("Project Background"),
             p("This app demonstrates a Bayesian logistic regression model built on the Framingham Heart Study dataset to predict the probability of coronary heart disease (CHD). The model uses the 'brms' package and incorporates prior uncertainty."),
             h4("Exploratory Data Analysis"),
             selectInput("edaVar", "Select Variable:", choices = numeric_vars, selected = "AGE"),
             radioButtons("plotType", "Plot Type:", choices = c("Boxplot" = "box", "Histogram" = "hist"), inline = TRUE),
             plotOutput("edaPlot"),
             h4("Correlation Matrix"),
             plotOutput("corPlot")
    ),
    tabPanel("Patient Prediction",
             sidebarLayout(
               sidebarPanel(
                 numericInput("AGE", "Age", 50, min = 30, max = 100),
                 selectInput("SEX", "Sex", choices = c("Male" = 1, "Female" = 0)),
                 numericInput("TOTCHOL", "Total Cholesterol", 200, min = 100, max = 400),
                 numericInput("GLUCOSE", "Glucose", 90, min = 50, max = 200),
                 numericInput("DIABP", "Diastolic BP", 80, min = 50, max = 120),
                 selectInput("CURSMOKE", "Current Smoker", choices = c("Yes" = 1, "No" = 0)),
                 numericInput("CIGPDAY", "Cigarettes per Day", 0, min = 0, max = 50),
                 selectInput("BPMEDS", "On BP Meds", choices = c("Yes" = 1, "No" = 0)),
                 numericInput("BMI", "BMI", 25, min = 10, max = 50),
                 numericInput("HEARTRTE", "Heart Rate", 70, min = 40, max = 120),
                 actionButton("predictBtn", "Predict")
               ),
               mainPanel(
                 h4("Posterior Probability of ANYCHD = 1"),
                 verbatimTextOutput("predProb"),
                 plotOutput("posteriorHist")
               )
             )
    ),
    tabPanel("Model Summary",
             h4("Posterior Summary"),
             verbatimTextOutput("modelSummary"),
             h4("Model Performance Metrics"),
             verbatimTextOutput("perfMetrics")
    )
  )
)

# Server
server <- function(input, output) {
  
  output$modelSummary <- renderPrint({
    summary(heart_model)
  })
  
  output$corPlot <- renderPlot({
    ggcorr(df %>% select(all_of(numeric_vars)), label = TRUE)
  })
  
  output$edaPlot <- renderPlot({
    var <- input$edaVar
    type <- input$plotType
    if (type == "box") {
      ggplot(df, aes(x = factor(ANYCHD), y = .data[[var]])) +
        geom_boxplot(fill = "lightblue") +
        labs(title = paste(var, "by Heart Disease Outcome"), x = "ANYCHD", y = var)
    } else {
      ggplot(df, aes(x = .data[[var]], fill = factor(ANYCHD))) +
        geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
        labs(title = paste("Distribution of", var, "by ANYCHD"), x = var, y = "Count", fill = "ANYCHD") +
        scale_fill_manual(values = c("skyblue", "tomato"))
    }
  })
  
  output$perfMetrics <- renderPrint({
    probs <- posterior_linpred(heart_model, transform = TRUE)
    mean_probs <- colMeans(probs)
    y_pred <- ifelse(mean_probs > 0.5, 1, 0)
    y_true <- df$ANYCHD
    
    conf_matrix <- table(Predicted = y_pred, Actual = y_true)
    TP <- conf_matrix["1", "1"]
    TN <- conf_matrix["0", "0"]
    FP <- conf_matrix["1", "0"]
    FN <- conf_matrix["0", "1"]
    
    precision <- TP / (TP + FP)
    recall <- TP / (TP + FN)
    f1 <- 2 * (precision * recall) / (precision + recall)
    accuracy <- mean(y_pred == y_true)
    
    cat("Accuracy:", round(accuracy, 3), "\n")
    cat("Precision:", round(precision, 3), "\n")
    cat("Recall:", round(recall, 3), "\n")
    cat("F1 Score:", round(f1, 3), "\n")
  })
  
  observeEvent(input$predictBtn, {
    new_patient <- tibble(
      AGE = input$AGE,
      SEX = as.numeric(input$SEX),
      TOTCHOL = input$TOTCHOL,
      GLUCOSE = input$GLUCOSE,
      DIABP = input$DIABP,
      CURSMOKE = as.numeric(input$CURSMOKE),
      CIGPDAY = input$CIGPDAY,
      BPMEDS = as.numeric(input$BPMEDS),
      BMI = input$BMI,
      HEARTRTE = input$HEARTRTE
    )
    
    preds <- posterior_predict(heart_model, newdata = new_patient)
    pred_mean <- mean(preds)
    
    output$predProb <- renderPrint({
      cat("Estimated probability of ANYCHD = 1:", round(pred_mean, 3))
    })
    
    output$posteriorHist <- renderPlot({
      hist(preds, breaks = 20, main = "Posterior Predictive Distribution", xlab = "ANYCHD = 1 Probability", col = "skyblue")
    })
  })
}

shinyApp(ui = ui, server = server)
