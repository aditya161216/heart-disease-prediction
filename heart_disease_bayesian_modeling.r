# Krina Patel, Aditya Vikrant
# ML2 Final Project Bayesian Modeling- Framingham Heart Disease Dataset

# Load necessary libraries
library(tidyverse)
library(mvnfast)

# Load dataset
df <- read_csv("/Users/krinapatel/Desktop/northeastern/Year_4/ML2/Project/ml2_final_project/Framingham Dataset.csv")

# Drop columns with >50% missing
missing_percent <- colSums(is.na(df)) / nrow(df) * 100
cols_to_drop <- names(missing_percent[missing_percent > 50])
df <- df %>% select(-all_of(cols_to_drop))

# Replace NA in numeric columns with median
num_cols <- c("TOTCHOL", "GLUCOSE", "BMI")
df <- df %>%
  mutate(across(all_of(num_cols), ~replace_na(., median(., na.rm = TRUE))))

# Replace NA in categorical columns with mode
cat_cols <- c("BPMEDS")
df <- df %>%
  mutate(across(all_of(cat_cols), ~replace_na(., as.numeric(names(sort(table(.), decreasing = TRUE)[1])))))

# Modify CIGPDAY
df <- df %>%
  mutate(CIGPDAY = case_when(
    is.na(CIGPDAY) & CURSMOKE == 0 ~ 0,
    TRUE ~ CIGPDAY
  ))
df$CIGPDAY[is.na(df$CIGPDAY)] <- median(df$CIGPDAY, na.rm = TRUE)

# Drop 'educ' and rows missing 'HEARTRTE'
df <- df %>%
  select(-educ) %>%
  filter(!is.na(HEARTRTE))

# Drop leakage columns EXCEPT ANYCHD
drop_leakage_cols <- c('RANDID', 'TIME', 'PERIOD', 'DEATH', 'STROKE', 'CVD', 'HYPERTEN', 'MI_FCHD',
                       'HOSPMI', 'ANGINA', 'TIMEAP', 'TIMEMI', 'TIMEMIFC', 'TIMECHD', 'TIMESTRK',
                       'TIMECVD', 'TIMEDTH', 'TIMEHYP')
df <- df %>% select(-all_of(drop_leakage_cols))  # KEEP ANYCHD!

# Set up binary target and features
y <- df$ANYCHD
X <- cbind(
  intercept = 1,
  AGE = scale(df$AGE),
  SEX = df$SEX,
  TOTCHOL = scale(df$TOTCHOL),
  GLUCOSE = scale(df$GLUCOSE),
  DIABP = scale(df$DIABP),
  CURSMOKE = df$CURSMOKE,
  CIGPDAY = scale(df$CIGPDAY),
  BPMEDS = df$BPMEDS,
  BMI = scale(df$BMI),
  HEARTRTE = scale(df$HEARTRTE)
)

# Approximate Bayesian logistic regression using a pseudo-likelihood (linear approximation)
# Treat binary y as "noisy continuous" outcome for now (probit-like)
n <- nrow(X)
p <- ncol(X)

# OLS estimate
w_hat <- solve(t(X) %*% X) %*% t(X) %*% y
resid <- y - X %*% w_hat
s2 <- as.numeric(t(resid) %*% resid / (n - p))

# Posterior sampling
rinvchisq <- function(n, df, scale) {
  (df * scale) / rchisq(n, df)
}
sigma2_tilde <- rinvchisq(1000, df = n - p, scale = s2)

w_tilde <- matrix(0, nrow = 1000, ncol = p)
XtX_inv <- solve(t(X) %*% X)
for (i in 1:1000) {
  w_tilde[i, ] <- rmvn(1, mu = w_hat, sigma = XtX_inv * sigma2_tilde[i])
}

# Show 3 histograms at a time
par(mfrow = c(1, 3))  # 1 row, 3 columns

# First batch
hist(w_tilde[, 1], main = "Posterior of Intercept", xlab = "Intercept")
hist(w_tilde[, 2], main = "Posterior of AGE", xlab = "AGE")
hist(w_tilde[, 3], main = "Posterior of SEX", xlab = "SEX")

# Next batch
par(mfrow = c(1, 3))
hist(w_tilde[, 4], main = "Posterior of TOTCHOL", xlab = "TOTCHOL")
hist(w_tilde[, 5], main = "Posterior of GLUCOSE", xlab = "GLUCOSE")
hist(w_tilde[, 6], main = "Posterior of DIABP", xlab = "DIABP")

# Next batch
par(mfrow = c(1, 3))
hist(w_tilde[, 7], main = "Posterior of CURSMOKE", xlab = "CURSMOKE")
hist(w_tilde[, 8], main = "Posterior of CIGPDAY", xlab = "CIGPDAY")
hist(w_tilde[, 9], main = "Posterior of BPMEDS", xlab = "BPMEDS")

# Last batch (2 plots)
par(mfrow = c(1, 2))
hist(w_tilde[, 10], main = "Posterior of BMI", xlab = "BMI")
hist(w_tilde[, 11], main = "Posterior of HEARTRTE", xlab = "HEARTRTE")

# Reset to single plot
par(mfrow = c(1, 1))

# Patient 1

# Posterior predictive
hist(y_post_pred, main = "Posterior Predictive Prob for Patient 1", xlab = "P(ANYCHD = 1)")

# Predictive distribution for Patient 1
x0 <- X[1, ]
y_post_pred <- numeric(1000)
for (i in 1:1000) {
  mu <- t(w_tilde[i, ]) %*% x0
  y_post_pred[i] <- 1 / (1 + exp(-mu))  # apply logistic function manually
}

cat("Posterior mean probability of heart disease for Patient 1:", round(mean(y_post_pred), 3), "\n")

# Patient 2

# Posterior predictive
hist(y_post_pred, main = "Posterior Predictive Prob for Patient 2", xlab = "P(ANYCHD = 2)")

# Predictive distribution for Patient 2
x0 <- X[2, ]
y_post_pred <- numeric(1000)
for (i in 1:1000) {
  mu <- t(w_tilde[i, ]) %*% x0
  y_post_pred[i] <- 1 / (1 + exp(-mu))  # apply logistic function manually
}

cat("Posterior mean probability of heart disease for Patient 2:", round(mean(y_post_pred), 3), "\n")
