__Project Title:__ 

Analyzing the Impact of Profit-Sharing and Savings Plans on Employee Wages

__Description:__

This project investigates the causal relationship between profit-sharing and savings plans and employee wages using propensity score matching in R.

__Data:__

Dataset: ECOMOSS 2006

Source: Insee

__Methodology:__

* Data Import and Cleaning:
  Import the data using the haven package.
  Clean and prepare the data by removing missing values, creating new variables, and transforming variables into appropriate data types.
* Propensity Score Modeling:
  Estimate propensity scores using logistic regression based on observed covariates.
  Use the matchit package for matching treated and control units based on their propensity scores.
* Treatment Effect Estimation:
  Compare the wages of matched treated and control groups using linear regression.
  Assess the statistical significance and magnitude of the treatment effect.

__Code Structure:__

data_cleaning.R: Functions for data cleaning and preparation.

propensity_score_matching.R: Functions for propensity score estimation and matching.

treatment_effect_analysis.R: Functions for estimating the treatment effect and conducting statistical tests.

main.R: Main script that runs the analysis and generates output.
