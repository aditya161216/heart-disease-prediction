## Krina Patel, Aditya Vikrant
## ML2 Final Project - Heart Disease Prediction using Bayesian Modeling

# Load libraries
library(tidyverse)
library(brms)
library(bayesplot)
library(GGally)
set.seed(42)
library(gridExtra)

# Load and clean dataset
df <- read_csv("/Users/krinapatel/Desktop/northeastern/Year_4/ML2/Project/ml2_final_project/Framingham Dataset.csv")

# Check it out
head(df)

# Drop known leakage variables (except ANYCHD)
drop_cols <- c('RANDID', 'TIME', 'PERIOD', 'DEATH', 'STROKE', 'CVD', 'HYPERTEN',
               'MI_FCHD', 'HOSPMI', 'ANGINA', 'TIMEAP', 'TIMEMI', 'TIMEMIFC',
               'TIMECHD', 'TIMESTRK', 'TIMECVD', 'TIMEDTH', 'TIMEHYP')
df <- df %>% select(-any_of(drop_cols))

# Remove 'educ' column
df <- df %>% select(-educ)

# Print number of rows before removing missing values
cat("Original number of rows:", nrow(df), "\n")

# Count how many rows have missing values
na_rows <- sum(!complete.cases(df))
cat("Rows with missing values:", na_rows, "\n")

# Print number and percent of missing values per column
missing_summary <- df %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "Column", values_to = "Missing") %>%
  mutate(Percent = round((Missing / nrow(df)) * 100, 2)) %>%
  arrange(desc(Missing))

print(missing_summary)

# Drop LDLC and HDLC since they have 74% missing values
df <- df %>% select(-LDLC, -HDLC)

# Custom mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Impute missing rows for GLUCOSE, BPMEDS, TOTCHOL, CIGPDAY, BMI, & HEARTRATE
df <- df %>%
  mutate(
    # use median for glucose
    GLUCOSE = ifelse(is.na(GLUCOSE), median(GLUCOSE, na.rm = TRUE), GLUCOSE),
    # use mode for bp medicine
    BPMEDS = ifelse(is.na(BPMEDS), Mode(BPMEDS), BPMEDS),
    # use median for total cholestorol
    TOTCHOL = ifelse(is.na(TOTCHOL), median(TOTCHOL, na.rm = TRUE), TOTCHOL),
    # if current smoke is 0, make cig per day = 0
    CIGPDAY = ifelse(is.na(CIGPDAY) & CURSMOKE == 0, 0, CIGPDAY),
    # otherwise cigarettes per day is median
    CIGPDAY = ifelse(is.na(CIGPDAY), median(CIGPDAY, na.rm = TRUE), CIGPDAY),
    # use median for bmi
    BMI = ifelse(is.na(BMI), median(BMI, na.rm = TRUE), BMI),
    # use median for heartrate
    HEARTRTE = ifelse(is.na(HEARTRTE), median(HEARTRTE, na.rm = TRUE), HEARTRTE)
  )

# Count how many rows have missing values
na_rows <- sum(!complete.cases(df))
cat("Rows with missing values:", na_rows, "\n")

# Print number of rows after cleaning
cat("Remaining after removal:", nrow(df), "\n")

# Look at the data
head(df)
summary(df)

# Recode SEX (0 = Female, 1 = Male)
df <- df %>%
  mutate(SEX = ifelse(SEX == 1, 1, 0))

# Define variables
binary_vars <- c("SEX", "CURSMOKE", "DIABETES", "BPMEDS", "PREVCHD", 
                 "PREVAP", "PREVMI", "PREVSTRK", "PREVHYP")

# Identify continuous columns to scale (excluding binary indicators like SEX, CURSMOKE, etc.)
continuous_vars <- c("TOTCHOL", "AGE", "SYSBP", "DIABP", "CIGPDAY", "BMI", "HEARTRTE", "GLUCOSE")

df$ANYCHD <- factor(df$ANYCHD, levels = c(0, 1), labels = c("No CHD", "CHD"))

# Correct: overlapping histograms
cont_plots <- map(continuous_vars, function(var) {
  ggplot(df, aes_string(x = var, fill = "ANYCHD")) +
    geom_histogram(position = "identity", alpha = 0.5, bins = 30, color = "black") +
    theme_minimal() +
    labs(title = paste("Histogram of", var, "by CHD Status"),
         x = var,
         fill = "CHD Status") +
    scale_fill_manual(values = c("skyblue", "tomato"))
})

# Show all continuous variable plots in a grid
gridExtra::grid.arrange(grobs = cont_plots, ncol = 2)

# Fix CHD labels
df$ANYCHD <- factor(df$ANYCHD, levels = c("No CHD", "CHD"))

# Check if both levels are present
table(df$ANYCHD)

# Binary bar plots with CHD status fill
bin_plots_counts <- map(binary_vars, function(var) {
  ggplot(df, aes(x = .data[[var]], fill = ANYCHD)) +
    geom_bar(position = "dodge", color = "black") +
    scale_fill_manual(values = c("steelblue", "tomato"), drop = FALSE) +
    labs(
      title = paste("CHD Status by", var),
      x = var,
      y = "Count",
      fill = "CHD Status"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "right"
    )
})

# Display grid
gridExtra::grid.arrange(grobs = bin_plots_counts, ncol = 3)

# Standardize these columns
df <- df %>%
  mutate(across(all_of(continuous_vars), scale))

# Check Scaled Variables
head(df)

# Define priors based on prior beliefs, from other studies
# Gender, BMJ Global Health Journal: https://gh.bmj.com/content/2/2/e000298 
# Total Cholesterol, National Library of Medicine: https://pubmed.ncbi.nlm.nih.gov/27016614/ 
# Age, National Library of Medicine: https://pmc.ncbi.nlm.nih.gov/articles/PMC6616540/#:~:text=Abstract,and%20gender%20on%20heart%20disease
# Systolic Blood Pressure, https://pubmed.ncbi.nlm.nih.gov/9797168/ 
# Diastolic Blood Pressure, https://pmc.ncbi.nlm.nih.gov/articles/PMC5654652/#:~:text=In%20multivariable%2Dadjusted%20analyses%2C%20compared,hazard%20ratio%201.48%20%5B95%25%20confidence 
# Smoking, https://www.cdc.gov/tobacco/about/cigarettes-and-cardiovascular-disease.html 
# BMI, https://pubmed.ncbi.nlm.nih.gov/19454737/ 
# Diabetes, New England Journal of Medicine: https://www.nejm.org/doi/full/10.1056/NEJMoa1803180 
# Blood Pressure Medicine, https://pubmed.ncbi.nlm.nih.gov/29367388/ 
# Heart rate, European Society of Cardiology: https://www.escardio.org/Journals/E-Journal-of-Cardiology-Practice/Volume-5/Heart-Rate-as-marker-of-Cardiovascular-Risk-Title-Heart-Rate-as-marker-of-Car  
# Glucose, National Library of Medicine: https://pubmed.ncbi.nlm.nih.gov/16157837/ 
# Prevalent Coronory Heart Disease, https://www.ahajournals.org/doi/10.1161/circulationaha.106.174516 
# Prevalent Angina Pectoris, https://www.hopkinsmedicine.org/health/conditions-and-diseases/angina-pectoris#:~:text=Angina%20pectoris%20is%20chest%20pain,coronary%20artery%20disease%20(CAD). 
# Prevalent Myocardial Infarction, https://bmccardiovascdisord.biomedcentral.com/articles/10.1186/s12872-023-03231-w 
# Prevalent Stroke, https://www.heart.org/en/news/2020/01/09/after-stroke-an-astounding-risk-of-heart-problems 
# Prevalent Hypertensive, https://www.ncbi.nlm.nih.gov/books/NBK539800/#:~:text=Hypertension%20increases%20the%20workload%20on,can%20progress%20to%20heart%20failure. 

prior_custom <- c(
  # SEX (0 = Female, 1 = Male): Men have higher CHD risk, so we expect a positive association belief
  set_prior("normal(1, 5)", class = "b", coef = "SEX"),
  
  # TOTCHOL: Higher total cholesterol increases CHD risk
  set_prior("normal(1, 5)", class = "b", coef = "TOTCHOL"), 
  
  # AGE: CHD risk increases substantially with age
  set_prior("normal(1, 5)", class = "b", coef = "AGE"), 
  
  # SYSBP: Higher systolic blood pressure is a strong predictor of heart disease
  set_prior("normal(1, 5)", class = "b", coef = "SYSBP"), 
  
  # DIABP: Both very low and very high diastolic blood pressure are associated with increased CHD risk; We are uncertain whether the effect is positive or negative in a linear model.
  set_prior("normal(0, 5)", class = "b", coef = "DIABP"),
  
  # CURSMOKE: Smoking status (1 = smoker), associated with vascular damage and CHD
  set_prior("normal(1, 5)", class = "b", coef = "CURSMOKE"), 
  
  # CIGPDAY: Dose-response relationship — more cigarettes per day increases CHD risk
  set_prior("normal(1, 5)", class = "b", coef = "CIGPDAY"), 
  
  # BMI: Higher BMI linked with obesity-related cardiovascular issues
  set_prior("normal(1, 5)", class = "b", coef = "BMI"), 
  
  # DIABETES: Diabetics are at significantly increased risk for heart disease
  set_prior("normal(1, 5)", class = "b", coef = "DIABETES"), 
  
  # BPMEDS: Taking blood pressure meds implies elevated BP but might reduce risk — set neutral
  set_prior("normal(0, 5)", class = "b", coef = "BPMEDS"), 
  
  # HEARTRTE: Elevated resting heart rate linked to cardiovascular mortality
  set_prior("normal(0, 5)", class = "b", coef = "HEARTRTE"), 
  
  # GLUCOSE: Hyperglycemia is a risk factor even in non-diabetics
  set_prior("normal(1, 5)", class = "b", coef = "GLUCOSE"), 
  
  # PREVCHD: Prior CHD is the strongest predictor of future CHD events
  set_prior("normal(1.5, 5)", class = "b", coef = "PREVCHD"), 
  
  # PREVAP: Angina Pectoris is a marker of underlying CHD — strong positive prior
  set_prior("normal(1.5, 5)", class = "b", coef = "PREVAP"), 
  
  # PREVMI: Prior myocardial infarction almost guarantees increased risk
  set_prior("normal(2, 5)", class = "b", coef = "PREVMI"), 
  
  # PREVSTRK: Stroke indicates vascular pathology, increasing CHD risk
  set_prior("normal(1, 5)", class = "b", coef = "PREVSTRK"), 
  
  # PREVHYP: Prior hypertension, even if treated, still associated with CHD
  set_prior("normal(1, 5)", class = "b", coef = "PREVHYP"), 
  
  # Intercept 
  set_prior("normal(0, 10)", class = "Intercept")
)

# Fit Bayesian Logistic Regression Model
heart_model <- brm(
  formula = ANYCHD ~ .,
  data = df,
  family = bernoulli(link = "logit"),
  prior = prior_custom,
  chains = 4,
  iter = 2000,
  warmup = 360
)

# Model Diagnostics

# Get overall summary
summary(heart_model)

# Extract summary
coef_summary <- as.data.frame(fixef(heart_model))

# Generate interpretation as a character vector (all features)
posterior_lines <- sapply(rownames(coef_summary), function(feature) {
  est <- round(coef_summary[feature, "Estimate"], 3)
  lower <- round(coef_summary[feature, "Q2.5"], 3)
  upper <- round(coef_summary[feature, "Q97.5"], 3)
  
  if (lower > 0) {
    paste0(feature, " has a significant POSITIVE effect on CHD risk (95% CI: ", lower, " to ", upper, ")")
  } else if (upper < 0) {
    paste0(feature, " has a significant NEGATIVE effect on CHD risk (95% CI: ", lower, " to ", upper, ")")
  } else {
    paste0(feature, " has NO clear effect — the 95% CI includes 0 (", lower, " to ", upper, ")")
  }
})

# Also generate ranked top features that have significant effects
coef_summary_tbl <- coef_summary %>%
  rownames_to_column("Feature") %>%
  filter(Feature != "Intercept") %>%
  mutate(Significant = Q2.5 > 0 | Q97.5 < 0) %>%
  filter(Significant) %>%
  mutate(Direction = ifelse(Estimate > 0, "Positive", "Negative")) %>%
  arrange(desc(abs(Estimate)))

top_features_summary <- apply(coef_summary_tbl, 1, function(row) {
  paste0(row["Feature"], " (", row["Direction"], " effect, β = ",
         round(as.numeric(row["Estimate"]), 3),
         ", 95% CI: [", round(as.numeric(row["Q2.5"]), 3),
         ", ", round(as.numeric(row["Q97.5"]), 3), "])")
})

# Print full posterior interpretation
cat(posterior_lines, sep = "\n")

# Print top feature summary
cat("\n\nTop Correlated Features with CHD Risk:\n")
cat(top_features_summary, sep = "\n")

# Plot the posterior distributions/chains to assess convergence
plot(heart_model)

# Compares the actual observed data to the predictions-- how well the model captures the patterns in the data
pp_check(heart_model, type = "bars")

# Evaluate Bayesian R-squared
r2 <- bayes_R2(heart_model)
r2_estimate <- round(r2[1, "Estimate"], 3)

cat("Interpretation: The Bayesian R² value is", r2_estimate, ", meaning the model explains about", r2_estimate * 100, "% of the variance in CHD outcomes. While this suggests some predictive power, the relatively low value indicates that much of the variability in heart disease risk remains unexplained by the current model.")

# Generate posterior predictive samples
post_preds <- posterior_predict(heart_model)

# Posterior predictive distribution for the first observation
patient1_post_preds <- post_preds[, 1]
hist(patient1_post_preds,
     main = "Posterior Predictive Distribution for Patient 1",
     xlab = "Predicted ANYCHD Outcome",
     col = "lightblue",
     border = "white")

# Posterior predictive mean for Patient 1
cat("Posterior Predictive Mean for Patient 1:", round(mean(patient1_post_preds), 3), "\n")

# Use posterior_epred to get probabilities instead of 0/1 draws
patient1_probs <- posterior_epred(heart_model, newdata = df[1, , drop = FALSE])[,1]

# Compute mean and 95% credible interval
mean_prob <- mean(patient1_probs)
ci_low <- quantile(patient1_probs, 0.025)
ci_high <- quantile(patient1_probs, 0.975)

# Plot the probability distribution
ggplot(data.frame(prob = patient1_probs), aes(x = prob)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "white") +
  geom_vline(xintercept = mean_prob, color = "darkblue", size = 1.2) +
  geom_vline(xintercept = ci_low, color = "red", linetype = "dashed") +
  geom_vline(xintercept = ci_high, color = "red", linetype = "dashed") +
  labs(
    title = "Posterior Probability of CHD for Patient 1",
    x = "Predicted CHD Risk (%)",
    y = "Frequency",
    subtitle = paste0("Mean = ", round(mean_prob, 3),
                      ", 95% CI = [", round(ci_low, 3), ", ", round(ci_high, 3), "]")
  ) +
  theme_minimal()

# Generate predicted means across all patients
y_pred_mean <- colMeans(post_preds)

# Save raw ANYCHD as numeric for correlation and confusion matrix
df$ANYCHD_raw <- ifelse(df$ANYCHD == "CHD", 1, 0)

# Correlation between predicted and actual values
correlation <- cor(y_pred_mean, df$ANYCHD_raw)
cat("Correlation between posterior predictive means and actual values:", round(correlation, 3), "\n")

# Turn into binary predictions using 0.5 threshold
y_pred <- ifelse(y_pred_mean > 0.5, 1, 0)

# Confusion Matrix & Performance Metrics
conf_matrix <- table(Predicted = y_pred, Actual = df$ANYCHD_raw)

print(conf_matrix)

TP <- conf_matrix["1", "1"]
TN <- conf_matrix["0", "0"]
FP <- conf_matrix["1", "0"]
FN <- conf_matrix["0", "1"]

# Metrics
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1 <- 2 * (precision * recall) / (precision + recall)
accuracy <- mean(y_pred == df$ANYCHD_raw)  # <-- fixed here

# Print results with interpretations
cat("Model Performance Summary:\n")
cat("Accuracy:", round(accuracy, 3), "- The model predicted correctly for", round(accuracy, 3),"of cases.\n")
cat("Precision:", round(precision, 3), "- When the model predicted CHD, it was correct", round(precision, 3),"of the time.\n")
cat("Recall:", round(recall, 3), "- Of those who actually had CHD, the model only identified", round(recall, 3), "\n")
cat("F1 Score:", round(f1, 3), "- The balance between precision and recall is moderate (F1 = ", round(f1, 3), ").\n")

cat("Interpretation:\n")
cat("This model is highly precise but has poor recall — it misses many true cases of CHD.\n")
cat("While the overall accuracy seems high, this is likely due to class imbalance.\n")
cat("Consider lowering the threshold, adding predictors, or using sampling techniques to improve recall.\n")

# Save files for Shiny
saveRDS(heart_model, "heart_model.rds")
saveRDS(df, "framingham_cleaned.rds")
saveRDS(y_pred_mean, "predicted_probabilities.rds")
saveRDS(y_pred, "binary_predictions.rds")
saveRDS(cont_plots, "cont_plots.rds")
saveRDS(bin_plots_counts, "bin_plots_counts.rds")
saveRDS(top_features_summary, "top_feature_summary.rds")
