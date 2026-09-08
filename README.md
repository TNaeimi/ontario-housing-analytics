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

### Geographic Data

- Ontario municipal boundary shapefiles
- Municipal identifiers and spatial polygons

Data Sources:

- Statistics Canada 
- Canada Mortgage and Housing Corporation (CMHC)
- Ontario GeoHub
---

## Data Preparation Methods

### Data Cleaning

The following data preparation steps were performed:

### Data Cleaning

Data cleaning and preprocessing were conducted to improve data quality, consistency, and analytical readiness prior to exploratory analysis and modelling.

The following steps were performed:

- Identified and handled missing and null values across all datasets.
- Filtered records with incomplete or unusable information where appropriate.
- Selected relevant variables required for analysis and modelling.
- Renamed fields to improve readability and consistency across datasets.
- Removed unnecessary characters, numeric codes, and redundant information from selected columns.
- Standardized text fields and categorical variables.
- Checked for duplicate records and data inconsistencies.
- Converted and standardized date fields originating from multiple formats, including:
  - Year-only formats (e.g., `2022`)
  - Month-Year formats (e.g., `Jan-2022`)
  - Character-based month representations (e.g., `January 2022`)
  - Mixed date formats from different source systems
- Created a consistent date structure to support time-series analysis and trend visualization.
- Validated municipality names and geographic identifiers to ensure consistency across datasets.
- Prepared cleaned datasets for exploratory analysis, feature engineering, predictive modelling, and spatial mapping.

### Feature Engineering

Additional analytical variables were developed including:

- Housing affordability ratios
- Age proportion
- houshold size proportion
- Houshold type proportion
- Imigrant Proportion
- Housing Structure Share
- Estimated Monthly Shelter Cost

### Spatial Data Preparation

Municipal boundary shapefiles were:

- Imported using the `sf` package
- Standardized using municipality identifiers
- Joined to affordability datasets
- Prepared for choropleth mapping and visualization

### Standardization

For coefficient comparison and interpretation:

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

Model settings:

- 1,000 trees
- Variable importance enabled

### 3. Gradient Boosting

Gradient Boosting models were used to:

- Model complex interactions.
- Evaluate cross-validation loss behavior.

Model settings:

- Gaussian distribution
- 10-fold cross-validation
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

### Explanatory Graphs

- Historical Trend of Housing Supply
- Historical Trend of Rental Marker

### Explanatory Graphs

- Ontario Housing Monitoring Dashboard: R Shiny

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
## Assumptions and Limitations

### Geographic Coverage

This analysis does not include all municipalities across Ontario. The study focuses primarily on municipalities located in Southern Ontario and selected municipalities from other regions where data were available and sufficiently complete for analysis. As a result, findings may not be fully representative of all Ontario municipalities.

### Temporal Coverage

The analysis was intended to use the most recent available data; however, data availability varied across datasets and municipalities. In several cases, complete and consistent data were not available for 2021 and later years. Consequently, 2020 data were used for many municipalities to maintain consistency and maximize geographic coverage.

### Data Availability and Consistency

Municipal-level datasets were obtained from multiple sources, including housing, demographic, income, and geographic datasets. Differences in data collection methods, reporting periods, and update frequencies may introduce inconsistencies across datasets.

### Municipality Name Standardization

A significant challenge during data integration involved inconsistencies in municipality naming conventions across data sources.

For example:

- CMHC datasets often used municipality names and market area definitions that differed from those used in Ontario municipal boundary datasets.
- Some municipalities included administrative suffixes (e.g., Township, Municipality, County) while others did not.
- Several municipalities were represented using alternative naming formats, abbreviations, or consolidated market areas.

As a result, substantial manual cleaning and recoding were required to standardize municipality names before the attribute data could be successfully joined to municipal boundary shapefiles. Although extensive validation was performed, there remains a possibility of minor matching inaccuracies.

### Modelling Assumptions

The predictive models developed in this project assume that historical relationships between housing affordability and the selected demographic, income, immigration, household composition, and housing characteristics remain reasonably stable during the study period.

Additionally:

- Linear Regression assumes linear relationships between predictors and affordability.
- Random Forest and Gradient Boosting models assume that historical patterns in the training data can be generalized to unseen municipalities within the study area.
- Model results should be interpreted as predictive and exploratory rather than causal.

### Spatial Analysis Limitations

Municipal boundaries were obtained from publicly available geographic data sources and joined to affordability indicators using standardized municipality identifiers and names. Any remaining discrepancies between geographic and attribute datasets may affect spatial visualization results.

### Interpretation of Results

The findings presented in this project should be interpreted as an analytical assessment of housing affordability patterns rather than a definitive measure of housing conditions. Housing affordability is influenced by many factors that may not be fully captured in the available datasets, including local policy decisions, housing quality, labour market conditions, and other socioeconomic factors.

## Repository Structure

```text
├── Data/
│   ├── Raw/
│   │   └── Shapefiles/
│   └── Processed/
│
├── Scripts/
│   ├── 01_DataCleaning.R
│   ├── 02_EDA_HistoricalSupplyTrends.R
│   ├── 03_EDA_HistoricalRentalMarket.R
│   ├── 04_EDA_Housing_Absorption.R
│   ├── 05_DataCleaningForModeling.R
│   ├── 06_EDA_AffordabilityRatio.R
│   ├── 07_FeatureEngineering.R
│   ├── 08_ModellingAndFeatureImportance.R
│   ├── 09_AffordabilityMapping.R
│   └── 10_HousingMonitoringDashboard.R
│
├── outputs/
│   ├── figures/
│   └── reports/
│
├── README.md
└── LICENSE
