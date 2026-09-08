# Ontario Housing Affordability Analysis

## Overview

This project examines housing affordability across Ontario municipalities using demographic, socioeconomic, immigration, household composition, and housing market indicators.

The analysis integrates multiple municipal-level datasets to:

- Measure housing affordability across communities.
- Identify the key factors associated with housing affordability challenges.
- Compare predictive performance of multiple machine learning models.
- Visualize affordability patterns across Ontario municipalities through spatial mapping.

---

## Project Objectives

The primary objectives of this project are to:

1. Develop municipal-level housing affordability indicators.
2. Identify the most influential factors affecting affordability.
3. Compare predictive performance of:
   - Linear Regression
   - Random Forest
   - Gradient Boosting
4. Visualize affordability patterns through GIS-based mapping.
5. Support evidence-based housing policy and planning decisions.

---

## Data Sources Used

The project combines several municipal-level datasets from publicly available sources.

### Housing Data

- Housing affordability indicators
- Shelter cost estimates
- Housing structure composition
- Apartment, detached, and missing-middle housing shares

### Demographic Data

- Age distribution
- Household size composition
- Household type composition

### Income Data

- Median and household income indicators
- Monthly shelter cost estimates

### Immigration Data

- Immigrant population share
- Immigration period composition
- Recent immigrant indicators

### Geographic Data

- Ontario municipal boundary shapefiles
- Municipal identifiers and spatial polygons

Potential data providers include:

- Statistics Canada Census Program
- Ontario Open Data Portal
- Ontario Ministry of Agriculture, Food and Agribusiness (OMAFA)
- Municipal boundary datasets

---

## Data Preparation Methods

### Data Cleaning

The following data preparation steps were performed:

- Missing value checking and validation
- Variable consistency checks
- Municipal name standardization
- Duplicate record verification
- Spatial identifier validation

### Feature Engineering

Additional analytical variables were developed including:

- Housing affordability ratios
- Housing structure composition shares
- Household composition percentages
- Immigration composition shares
- Age cohort percentages

### Spatial Data Preparation

Municipal boundary shapefiles were:

- Imported using the `sf` package
- Standardized using municipality identifiers
- Joined to affordability datasets
- Prepared for choropleth mapping and visualization

### Standardization

For coefficient comparison and interpretation:

- Predictor variables were standardized using z-score normalization
- Standardized coefficients were calculated for linear regression analysis

---

## Methodology

### 1. Linear Regression

Linear regression was used as a baseline statistical model to:

- Estimate relationships between predictors and affordability.
- Assess statistical significance.
- Measure standardized coefficient effects.

### 2. Random Forest

Random Forest models were developed to:

- Capture nonlinear relationships.
- Evaluate variable importance.
- Improve predictive performance.

Model settings:

- 1,000 trees
- Variable importance enabled

### 3. Gradient Boosting

Gradient Boosting models were used to:

- Model complex interactions.
- Optimize prediction accuracy.
- Evaluate cross-validation loss behavior.

Model settings:

- Gaussian distribution
- 10-fold cross-validation
- Learning rate (shrinkage) = 0.01
- Optimal tree selection using cross-validation error

---

## Model Evaluation

Models were evaluated using:

### Root Mean Squared Error (RMSE)

Measures prediction error magnitude.

### Mean Absolute Error (MAE)

Measures average prediction deviation.

### R-Squared (R²)

Measures explanatory power.

Performance metrics were calculated using an independent testing dataset.

---

## Key Outputs

### Model Performance Comparison

Comparison of:

- Linear Regression
- Random Forest
- Gradient Boosting

using:

- RMSE
- MAE
- R²

### Feature Importance Analysis

Linear Regression:

- Standardized coefficients

Random Forest:

- Percent increase in MSE (%IncMSE)

### Spatial Affordability Mapping

Ontario municipal affordability maps were developed using:

- Ontario municipal boundary shapefiles
- Affordability indicators
- Choropleth mapping techniques

---

## Repository Structure

```text
├── Data/
│   ├── Raw/
│   └── Processed/
│
├── Scripts/
│   ├── 01_train_models.R
│   ├── 02_evaluate_models.R
│   ├── 03_compare_models.R
│   ├── 04_feature_importance.R
│   └── 05_spatial_mapping.R
│
├── outputs/
│   ├── models/
│   ├── metrics/
│   ├── figures/
│   └── maps/
│
├── README.md
└── LICENSE
