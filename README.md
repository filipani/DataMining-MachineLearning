#Data Analysis Projects in R

This repository contains two data analysis projects developed during my bachelor's degree coursework.

The goal of these projects was to apply statistical modeling and machine learning techniques to real-world datasets.

The repository includes:

-Cycling activity analysis using statistical modeling

-Diabetes prediction using multiple machine learning algorithms

Both projects involve data preprocessing, model development, diagnostic analysis, and model evaluation.

##Project 1 — Cycling Activity Analysis
###Objective

The goal of this project is to explain the duration of cycling activities (Time) using several physiological and activity-related predictors.

The analysis focuses on identifying which variables significantly influence the duration of a cycling activity and building a robust statistical model.

###Dataset

The dataset contains information on cycling activities, including:

-Distance

-Calories burned

-Average and maximum heart rate

-Average and maximum speed

-Power metrics

-Activity type

The response variable Time was converted from string format to numeric seconds for modeling.

###Data Preparation

Several preprocessing steps were performed:

-Conversion of variables to appropriate numeric or factor types

-Transformation of time format into seconds

-Identification and visualization of missing data

-Removal of variables with more than 25% missing values

-Missing value imputation using MICE (Predictive Mean Matching)

Exploratory analysis included:

-correlation matrices

-pairwise variable plots

-multicollinearity diagnostics (VIF / tolerance)

###Statistical Modeling

Multiple regression models were tested to explain cycling duration.

Diagnostic tests revealed several issues typical of real-world data:

-non-normal residuals

-heteroscedasticity

-non-linearity

-presence of outliers

###Model Improvements

To address these issues, several techniques were applied:

-Box-Cox transformation to identify a suitable transformation of the response variable

-transformation of predictors (e.g., log transformations)

-exploration of non-linear relationships using Generalized Additive Models (GAM)

-identification and removal of influential observations using Cook's distance

-robust regression estimation

Model selection was performed using AIC-based stepwise selection.

The final model explains cycling duration using a subset of predictors while improving the statistical assumptions of the model.

##Project 2 — Diabetes Prediction
###Objective

The goal of this project is to build predictive models capable of identifying whether a patient is diagnosed with diabetes using demographic, lifestyle, and health indicators.

The project emphasizes model comparison and evaluation.

###Dataset

The dataset includes several predictors related to health and lifestyle, such as:

-Age

-BMI

-Blood pressure

-Cholesterol levels

-Physical activity

-Diet score

-Smoking and alcohol consumption

-Family history of diabetes

The target variable is:

diagnosed_diabetes (Yes / No)

Observations labeled Pre-Diabetes were removed to focus on clear diagnostic outcomes.

###Data Preparation

Preprocessing steps included:

-conversion of categorical variables into factors

-removal of variables directly used for diagnosis to avoid data leakage

dataset splitting into:

training set

validation set

scoring dataset

Outliers were detected using Cook's distance.

###Feature Selection

Important predictors were identified using the Boruta algorithm, which is based on Random Forest feature importance.

This step helped reduce dimensionality and improve model interpretability.

###Machine Learning Models

Several machine learning models were trained and compared using 10-fold cross-validation and ROC AUC as the main evaluation metric.

Models tested include:

-Logistic Regression (Elastic Net)

-Decision Trees

-k-Nearest Neighbors

-Neural Networks

-Partial Least Squares

-Naive Bayes

-Bagging

-Random Forest

-Gradient Boosting

The caret framework was used for training, preprocessing, hyperparameter tuning, and performance evaluation.

###Model Evaluation

Model performance was evaluated on a validation dataset using:

-ROC curves

-gain and lift charts

-confusion matrices

Predicted probabilities were also adjusted to reflect the true population prevalence of diabetes (~11%).

###Decision Threshold Optimization

Instead of using the default classification threshold (0.5), a custom threshold selection procedure was implemented to prioritize high recall (sensitivity).

This is particularly important in medical screening scenarios where false negatives are more critical than false positives.

The optimal threshold was selected using validation data and can optionally be assessed with bootstrap resampling.

###Tools and Libraries

The analysis was implemented in R using several libraries, including:

caret

Boruta

mice

corrplot

gam

pROC

rpart

randomForest

gbm

dplyr

###Datasets and Outputs

Both datasets used in the analyses are included in this repository.

Cycling dataset used for the statistical modeling of cycling activity duration

Diabetes dataset used for the machine learning classification task

In addition to the scripts, the repository also contains the full outputs generated by the code, including:

-statistical summaries

-model results

-diagnostic outputs

-plots and graphs produced during the analysis

These outputs were originally produced for the university exam and are provided as raw results (plain outputs) without extensive commenting. They can still be opened and explored to inspect the results of the analyses and the graphical outputs generated by the scripts.
