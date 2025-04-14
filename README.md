# ml2_final_project

# 🫀 Heart Disease Risk Prediction Using Bayesian Modeling & MLP

This project applies two machine learning approaches—Bayesian logistic regression and a Multilayer Perceptron (MLP)—to predict the likelihood of coronary heart disease (CHD) using data from the Framingham Heart Study.

## Authors
Krina Patel, Aditya Vikrant  

---

## Overview
Coronary heart disease (CHD) is the leading cause of death in the U.S. But, early prediction can enable preventative interventions. This project explores predictive modeling using both probabilistic and neural approaches:

- **Bayesian Logistic Regression** using the `brms` package in R
- **Multilayer Perceptron (MLP)** implemented manually in Python

The models were trained on a cleaned version of the Framingham dataset, accessed via Kaggle.

## Dataset
- **Source**: [Framingham Heart Study Dataset on Kaggle](https://www.kaggle.com/datasets/shreyjain601/framingham-heart-study)
- **Rows**: 11,627
- **Target Variable**: `ANYCHD` (1 = CHD, 0 = No CHD)
- **Features**: 17 predictor columns kept including demographics, vitals, cholesterol, glucose, smoking behavior, and prior heart history (original dataset includes additional leaky/incomplete columns)

## Project Structure
```
heart-disease-prediction/
├── app.R                               # Shiny app for CHD risk visualization
├── heart_disease_mlp_ml2.ipynb         # Python script for manual MLP
├── heart_disease_bayesian_modeling.R   # R script for Bayesian logistic regression using brms
├── Framingham Dataset.csv              # Input dataset
├── README.md                           # This file
```

## Requirements
### R Packages
- brms
- tidyverse
- bayesplot
- ggplot2
- shinydashboard (for `app.R`)

### Python Packages
- numpy
- pandas
- matplotlib

> **Note**: The MLP was implemented from scratch and does **not** use scikit-learn or TensorFlow.

## How to Run app.R

### 1. Bayesian Modeling in R
```r
# Open R or RStudio and run:
source("heart_disease_bayesian_modeling.R")
```

### 2. Shiny App for Risk Prediction
```r
# In RStudio:
shiny::runApp("app.R")
```
This will open an interactive browser window to enter user vitals and view posterior CHD risk distributions.

### 3. Multilayer Perceptron in Python
```bash
python3 mlp_model.py
```
The script will train the model and print out training metrics, accuracy, and warnings regarding class imbalance.

## Results Summary
- **Bayesian Model**: Accuracy = 83.6%, Precision = 0.696, Recall = 0.241
- **MLP Model**: Accuracy = 72.8%, but failed to predict positive CHD cases

## Limitations
- Class imbalance heavily impacted recall for both models
- MLP lacked positive case detection
- Bayesian model preferred for interpretability and clinical relevance

## Future Work
- Use SMOTE or class-weighted loss to handle imbalance
- Apply hyperparameter tuning and cross-validation
- Incorporate SHAP/LIME for MLP interpretability
- Explore hierarchical Bayesian models for subgroup analysis

---

## Contact
For questions or collaboration:
- **Krina Patel**: [LinkedIn](https://www.linkedin.com/in/patelkrina100)
- **Aditya Vikrant**: [LinkedIn](#)

