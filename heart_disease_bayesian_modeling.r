## Krina Patel, Aditya Vikrant
## ML2 Final Project - Heart Disease Prediction using Bayesian Modeling

# Load libraries
library(tidyverse)
library(brms)
library(bayesplot)
library(GGally)
set.seed(42)

# Load and clean dataset
df <- read_csv("/Users/krinapatel/Desktop/northeastern/Year_4/ML2/Project/ml2_final_project/Framingham Dataset.csv")

# Remove columns with >50% missing values
missing_percent <- colMeans(is.na(df)) * 100
cols_to_drop <- names(missing_percent[missing_percent > 50])
df <- df %>% select(-all_of(cols_to_drop))

# Impute missing values
numeric_vars_to_impute <- c("TOTCHOL", "GLUCOSE", "BMI")
df <- df %>% mutate(across(all_of(numeric_vars_to_impute), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

df <- df %>% mutate(
  BPMEDS = ifelse(is.na(BPMEDS), as.numeric(names(sort(table(BPMEDS), decreasing = TRUE)[1])), BPMEDS),
  CIGPDAY = ifelse(is.na(CIGPDAY) & CURSMOKE == 0, 0, CIGPDAY),
  CIGPDAY = ifelse(is.na(CIGPDAY), median(CIGPDAY, na.rm = TRUE), CIGPDAY)
)

# Drop rows with NA in HEARTRTE and remove 'educ'
df <- df %>% drop_na(HEARTRTE) %>% select(-educ)

# Drop known leakage variables (except ANYCHD)
drop_cols <- c('RANDID', 'TIME', 'PERIOD', 'DEATH', 'STROKE', 'CVD', 'HYPERTEN',
               'MI_FCHD', 'HOSPMI', 'ANGINA', 'TIMEAP', 'TIMEMI', 'TIMEMIFC',
               'TIMECHD', 'TIMESTRK', 'TIMECVD', 'TIMEDTH', 'TIMEHYP')
df <- df %>% select(-any_of(drop_cols))

# Define priors
priors <- default_prior(
  ANYCHD ~ AGE + SEX + TOTCHOL + GLUCOSE + DIABP + CURSMOKE + CIGPDAY + BPMEDS + BMI + HEARTRTE,
  data = df,
  family = bernoulli()
)

# Fit Bayesian Logistic Regression Model
heart_model <- brm(
  formula = ANYCHD ~ AGE + SEX + TOTCHOL + GLUCOSE + DIABP + CURSMOKE + 
    CIGPDAY + BPMEDS + BMI + HEARTRTE,
  data = df,
  family = bernoulli(link = "logit"),
  prior = priors,
  chains = 4,
  iter = 1000,
  warmup = 180
)

# Model Diagnostics
summary(heart_model)
plot(heart_model)
pp_check(heart_model)
bayes_R2(heart_model)

# Prediction for All Observations
probs <- posterior_linpred(heart_model, transform = TRUE)
mean_probs <- colMeans(probs)
y_pred <- ifelse(mean_probs > 0.5, 1, 0)
y_true <- df$ANYCHD


# Confusion Matrix & Performance Metrics
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

# Prediction for Specific Patients (1 & 2) 
posterior_preds <- posterior_predict(heart_model, newdata = df[1:2, ])
posterior_probs <- colMeans(posterior_preds)

cat("Patient 1 P(ANYCHD = 1):", round(posterior_probs[1], 3), "\n")
cat("Patient 2 P(ANYCHD = 1):", round(posterior_probs[2], 3), "\n")

# Use posterior_linpred to get predicted probabilities (smooth)
linpred_probs <- posterior_linpred(heart_model, newdata = df[1:2, ], transform = TRUE)

# For Patient 1
dens1 <- density(linpred_probs[, 1])
plot(dens1,
     main = "Posterior Probability: Patient 1",
     xlab = "P(ANYCHD = 1)",
     col = "blue", lwd = 2,
     ylim = c(0, max(dens1$y) * 1.1))  # Add some headroom
polygon(dens1, col = rgb(0, 0, 1, 0.3), border = NA)


# For Patient 2
dens2 <- density(linpred_probs[, 2])
plot(dens2,
     main = "Posterior Probability: Patient 2",
     xlab = "P(ANYCHD = 1)",
     col = "red", lwd = 2,
     ylim = c(0, max(dens2$y) * 1.1))
polygon(dens2, col = rgb(1, 0, 0, 0.3), border = NA)


# Save Model for Shiny App
saveRDS(heart_model, "heart_model.rds")
saveRDS(df, "framingham_cleaned.rds")
