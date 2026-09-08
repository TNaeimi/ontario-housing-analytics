###########################################################
# Install Packages and Load libraries 
###########################################################

install.packages(c(
  "tidyverse",
  "dplyr",
  "lubridate",
  "ggrepel",
  "ggplot2",
  "gt",
  "forcats",
  "tidymodels",
  "rsample",
  "yardstick",
  "randomForest",
  "caret",
  "gbm"
))


library(tidyverse)
library(dplyr)
library(lubridate)
library(ggrepel)
library(ggplot2)
library(gt)
library(forcats)
library(tidymodels)
library(rsample)
library(yardstick)
library(randomForest)
library(caret)
library(gbm)


### Setting up the data folder and loading data
dataFolder = "./Data/Processed/"


Housing_Integrated_df_06 <- read.csv(paste0(dataFolder, "Housing_Integrated_df_FE.csv"))


########################################################################
########################### Modelling######## ##########################
########################################################################

############################# Modelling

Housing_model_df <- Housing_Integrated_df_06 %>%
  select(
    # Target
    Affordability_Ratio,
    
    # Income & shelter cost
    Houhold_Mon_Inco,
    Estimated_Monthly_Shelter_Cost,
    
    # Age composition
    Pct_0_14,
    Pct_15_24,
    Pct_25_34,
    Pct_35_44,
    Pct_45_54,
    Pct_55_64,
    Pct_65_Plus,
    
    # Household size composition
    Share_One_Person,
    Share_Two_Person,
    Share_Three_Person,
    Share_Four_Person,
    Share_FivePlus_Person,
    
    # Household type composition
    Share_Couple_With_Children,
    Share_Couple_Without_Children,
    Share_Lone_Parent,
    Share_Multiple_Family,
    Share_One_Person_Household,
    Share_Other_Non_Family,
    
    # Immigration composition
    Share_Immigrant,
    Share_Pre2006,
    Share_2006_2015,
    Share_Recent_Immigrant,
    
    # Housing structure composition
    Apartment_Share,
    Detached_Share,
    Missing_Middle_Share
  )

write.csv(
  Housing_model_df,
  "./Data/Processed/Housing_model_df.csv",
  row.names = FALSE
)


################ splitting df to training and testing df ################
###### Train/Test Split
set.seed(123)

housing_split <- rsample::initial_split(
  Housing_model_df,
  prop = 0.80
)

housing_train <- rsample::training(housing_split)
housing_test  <- rsample::testing(housing_split)

####################### Linear Regression Model ####################### 
#######################################################################

lm_model <- lm(
  Affordability_Ratio ~ .,
  data = housing_train
)

summary(lm_model)

#######################  Linear Regression Predictions #################
lm_pred <- predict(
  lm_model,
  newdata = housing_test
)

lm_results <- housing_test %>%
  mutate(pred = lm_pred)

#######################  Valuation Metrics ############################

lm_rmse <- rmse(
  lm_results,
  truth = Affordability_Ratio,
  estimate = pred
)

lm_mae <- mae(
  lm_results,
  truth = Affordability_Ratio,
  estimate = pred
)

lm_rsq <- rsq(
  lm_results,
  truth = Affordability_Ratio,
  estimate = pred
)

lm_rmse
lm_mae
lm_rsq

################# Scaled Linear Regression Coefficient Plot ####################
Housing_model_scaled <- Housing_model_df %>%
  mutate(
    across(
      -Affordability_Ratio,
      ~ as.numeric(scale(.))
    )
  )

lm_model_scaled <- lm(
  Affordability_Ratio ~ .,
  data = Housing_model_scaled
)

KeyDrivers_Housing_Affordability <- tidy(lm_model_scaled) %>%
  filter(term != "(Intercept)") %>%
  arrange(estimate) %>%
  mutate(
    term = factor(term, levels = term),
    Effect = ifelse(estimate > 0, "Increases Affordability Challenges",
                    "Reduces Affordability Challenges")
  ) %>%
  ggplot(aes(x = estimate, y = term, fill = Effect)) +
  geom_col() +
  geom_vline(
    xintercept = 0,
    color = "black",
    linetype = "dashed"
  ) +
  scale_fill_manual(
    values = c(
      "Increases Affordability Challenges" = "#D73027",
      "Reduces Affordability Challenges" = "#1A9850"
    ),
    na.translate = FALSE
  ) +
  labs(
    title = "Key Drivers of Housing Affordability",
    subtitle = "Standardized Linear Regression Coefficients",
    x = "Standardized Coefficient",
    y = NULL,
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title.x = element_text(
      face = "bold", margin = ggplot2::margin(t = 15)),
    plot.subtitle = element_text(size = 11),
    axis.text.y = element_text(face = "bold", size = 9),
    legend.position = "top"
  )


########### saving plot
ggsave(
  filename = "./outputs/figures/KeyDrivers_Housing_Affordability.png",
  plot = KeyDrivers_Housing_Affordability,
  width = 10,
  height = 6,
  dpi = 300
)

####################### End of Linear Regression Model ################
#######################################################################



#######################################################################
######################## Simple Random Forest Modeling ################

set.seed(123)

rf_model <- randomForest(
  Affordability_Ratio ~ .,
  data = housing_train,
  ntree = 1000,
  importance = TRUE
)

rf_model

#######################  RF Prediction #######################  
rf_pred <- predict(
  rf_model,
  newdata = housing_test
)

rf_results <- housing_test %>%
  mutate(pred = rf_pred)

#######################  valuation metrics ####################
rf_rmse <- rmse(
  rf_results,
  truth = Affordability_Ratio,
  estimate = pred
)

rf_mae <- mae(
  rf_results,
  truth = Affordability_Ratio,
  estimate = pred
)

rf_rsq <- rsq(
  rf_results,
  truth = Affordability_Ratio,
  estimate = pred
)

rf_rmse
rf_mae
rf_rsq

#######################  Variable Importance #####################

varImpPlot(rf_model)


imp <- importance(rf_model)

imp_df <- data.frame(
  Variable = rownames(imp),
  IncMSE = imp[, "%IncMSE"]
)

KeyDrivers_Housing_Affordability_RF <- imp_df %>%
  arrange(desc(IncMSE)) %>%
  slice_head(n = 15) %>%
  mutate(
    Variable = factor(
      Variable,
      levels = rev(Variable)
    )
  ) %>%
  ggplot(
    aes(
      x = IncMSE,
      y = Variable,
      fill = IncMSE
    )
  ) +
  geom_col() +
  scale_fill_gradient(
    low = "skyblue",
    high = "darkblue"
  ) +
  labs(
    title = "Key Drivers of Housing Affordability",
    subtitle = "Random Forest Variable Importance",
    x = "Predictive Importance",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title.x = element_text(
      face = "bold", margin = ggplot2::margin(t = 15)),
    plot.subtitle = element_text(size = 11),
    axis.text.y = element_text(face = "bold", size = 9),
    legend.position = "none"
  )

########### saving plot
ggsave(
  filename = "./outputs/figures/KeyDrivers_Housing_Affordability_RF.png",
  plot = KeyDrivers_Housing_Affordability_RF,
  width = 10,
  height = 6,
  dpi = 300
)

####################### End of Random Forest Model ################
###################################################################

#######################################################################
######################## Gradient boosting Modeling ################


set.seed(123)

gbm_model <- gbm(
  Affordability_Ratio ~ .,
  data = housing_train,
  distribution = "gaussian",
  n.trees = 1000,
  interaction.depth = 4,
  shrinkage = 0.01,
  cv.folds = 10,
  n.minobsinnode = 10,
  verbose = FALSE
)

###########  Loss curv ########### 
cv_loss <- data.frame(
  Iteration = 1:gbm_model$n.trees,
  CV_Loss = gbm_model$cv.error
)


###########  Initial Loss curv with 1000 tree ########### 
InitialLossCurve_gb <-  ggplot(cv_loss, aes(x = Iteration, y = CV_Loss)) +
  geom_line(color = "steelblue", linewidth = 1) +
  labs(
    title = "Gradient Boosting Cross-Validation Loss Curve",
    x = "Number of Trees",
    y = "Cross-Validated MSE"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title.x = element_text(
      face = "bold", margin = ggplot2::margin(t = 15)),
    plot.subtitle = element_text(size = 11),
    axis.text.y = element_text(face = "bold", size = 9),
    legend.position = "none"
  )

InitialLossCurve_gb
########### saving plot
ggsave(
  filename = "./outputs/figures/InitialLossCurve_gb.png",
  plot = InitialLossCurve_gb,
  width = 10,
  height = 6,
  dpi = 300
)


###########  optimal number of tree ########### 

best_iter <- gbm.perf(
  gbm_model,
  method = "cv"
)

best_iter

###########  Secondary Loss curv with 1000 tree ########### 
SecondryLossCurve_gb <-  ggplot(cv_loss, aes(Iteration, CV_Loss)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_vline(
    xintercept = best_iter,
    color = "red",
    linetype = "dashed"
  ) +
  annotate(
    "text",
    x = best_iter,
    y = min(cv_loss$CV_Loss),
    label = paste("Best =", best_iter),
    color = "red",
    hjust = -0.1
  ) +
  labs(
    title = "Gradient Boosting Cross-Validation Loss Curve",
    x = "Number of Trees",
    y = "CV Mean Squared Error"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title.x = element_text(
      face = "bold", margin = ggplot2::margin(t = 15)),
    plot.subtitle = element_text(size = 11),
    axis.text.y = element_text(face = "bold", size = 9),
    legend.position = "none"
  )
SecondryLossCurve_gb
########### saving plot
ggsave(
  filename = "./outputs/figures/SecondryLossCurve_gb.png",
  plot = SecondryLossCurve_gb,
  width = 10,
  height = 6,
  dpi = 300
)

############################ gb trainig with optial ntree #############
op_gbm_model <- gbm(
  Affordability_Ratio ~ .,
  data = housing_train,
  distribution = "gaussian",
  n.trees = 565,
  interaction.depth = 4,
  shrinkage = 0.01,
  cv.folds = 10,
  n.minobsinnode = 10,
  verbose = FALSE
)


op_cv_loss <- data.frame(
  Iteration = 1:op_gbm_model$n.trees,
  CV_Loss = op_gbm_model$cv.error
)

op_loss_curve <- ggplot(op_cv_loss, aes(x = Iteration, y = CV_Loss)) +
  geom_line(color = "steelblue", linewidth = 1) +
  labs(
    title = "Gradient Boosting Cross-Validation Loss Curve",
    subtitle = "Optimal tree umbers 565",
    x = "Number of Trees",
    y = "Cross-Validated MSE"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title.x = element_text(
      face = "bold", margin = ggplot2::margin(t = 15)),
    plot.subtitle = element_text(size = 11),
    axis.text.y = element_text(face = "bold", size = 9),
    legend.position = "none"
  )
op_loss_curve
########### saving plot
ggsave(
  filename = "./outputs/figures/op_loss_curve.png",
  plot = op_loss_curve,
  width = 10,
  height = 6,
  dpi = 300
)

####################### Actual vs Predicted plotting ###############
#######################  Actual vs Predicted  GB ###################  

# Optimal number of trees from cross-validation
best_iter <- gbm.perf(gbm_model, method = "cv")

# Predictions
gbm_pred <- predict(
  gbm_model,
  newdata = housing_test,
  n.trees = best_iter
)


# Actual values
actual <- housing_test$Affordability_Ratio

# Create results dataframe
gbm_results <- data.frame(
  Affordability_Ratio = actual,
  pred = gbm_pred
)

# Actual vs Predicted Plot
ActualvsPredicted_GBM <- ggplot(
  gbm_results,
  aes(
    x = Affordability_Ratio,
    y = pred
  )
) +
  geom_point(
    color = "steelblue",
    alpha = 0.7
  ) +
  geom_abline(
    slope = 1,
    intercept = 0,
    color = "red",
    linewidth = 1
  ) +
  labs(
    title = "Gradient Boosting: Actual vs Predicted",
    subtitle = "Affordability Ratio",
    x = "Actual",
    y = "Predicted"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      size = 20,
      face = "bold"
    ),
    axis.text.y = element_text(
      size = 20,
      face = "bold"
    ),
    axis.title.x = element_text(
      size = 20,
      face = "bold",
      margin = ggplot2::margin(t = 15)
    ),
    axis.title.y = element_text(
      size = 20,
      face = "bold",
      margin = ggplot2::margin(r = 15)
    ),
    plot.subtitle = element_text(
      face = "bold",
      size = 14
    ),
    plot.title = element_text(
      face = "bold",
      size = 20
    )
  )



# Save plot
ggsave(
  filename = "./outputs/figures/ActualvsPredicted_GBM.png",
  plot = ActualvsPredicted_GBM,
  width = 10,
  height = 6,
  dpi = 300
)


#######################  Actual vs Predicted RF #######################  

ActualvsPredicted_RF <-  ggplot(
  rf_results,
  aes(
    x = Affordability_Ratio,
    y = pred
  )
) +
  geom_point(
    color = "steelblue",
    alpha = 0.7
  ) +
  geom_abline(
    slope = 1,
    intercept = 0,
    color = "red",
    linewidth = 1
  ) +
  labs(
    title = "Random Forest: Actual vs Predicted",
    subtitle = "Affordability Rate",
    x = "Actual",
    y = "Predicted"
  ) + theme_minimal() +
  theme(
    axis.text.x  = element_text(
      size = 20,
      face = "bold"
    ),
    axis.text.y = element_text(
      size = 20,
      face = "bold"
    ),
    axis.title.x = element_text(
      size = 20,
      face = "bold",
      margin = ggplot2::margin(t = 15)
    ),
    axis.title.y = element_text(
      size = 20,
      face = "bold",
      margin = ggplot2::margin(r = 15)
    ),
    plot.subtitle = element_text(
      face = "bold",
      size = 14),
    plot.title = element_text(
      face = "bold",
      size = 20
    )
  )

ggsave(
  filename = "./outputs/figures/ActualvsPredicted_RF.png",
  plot = ActualvsPredicted_RF,
  width = 10,
  height = 6,
  dpi = 300
)
############ actual vs predicted Linear regression ############ 
ActualvsPredicted_LR <- ggplot(
  lm_results,
  aes(
    x = Affordability_Ratio,
    y = pred
  )
) +
  geom_point(
    color = "darkgreen",
    alpha = 0.7
  ) +
  geom_abline(
    slope = 1,
    intercept = 0,
    color = "red",
    linewidth = 1
  ) +
  labs(
    title = "Linear Regression: Actual vs Predicted",
    subtitle = "Affordability Rate",
    x = "Actual",
    y = "Predicted"
  ) + theme_minimal() +
  theme(
    axis.text.x  = element_text(
      size = 20,
      face = "bold"
    ),
    axis.text.y = element_text(
      size = 20,
      face = "bold"
    ),
    axis.title.x = element_text(
      size = 20,
      face = "bold",
      margin = ggplot2::margin(t = 15)
    ),
    axis.title.y = element_text(
      size = 20,
      face = "bold",
      margin = ggplot2::margin(r = 15)
    ),
    plot.subtitle = element_text(
      face = "bold",
      size = 14),
    plot.title = element_text(
      face = "bold",
      size = 20
    )
  )

ggsave(
  filename = "./outputs/figures/ActualvsPredicted_LR.png",
  plot = ActualvsPredicted_LR,
  width = 10,
  height = 6,
  dpi = 300
)


############## Model Comparison ##############  

rmse <- sqrt(mean((actual - gbm_pred)^2))
mae <- mean(abs(actual - gbm_pred))
r2 <- cor(actual, gbm_pred)^2

model_comparison <- tibble(
  Model = c(
    "Linear Regression",
    "Random Forest",
    "Gradient Boosting"
  ),
  RMSE = c(
    lm_rmse$.estimate,
    rf_rmse$.estimate,
    rmse
  ),
  MAE = c(
    lm_mae$.estimate,
    rf_mae$.estimate,
    mean(abs(actual - gbm_pred))
  ),
  R2 = c(
    lm_rsq$.estimate,
    rf_rsq$.estimate,
    r2
  )
)

View(model_comparison)

write.csv(
  model_comparison,
  "./Outputs/reports/model_comparison.csv",
  row.names = FALSE
)

model_comparison_graph <- model_comparison %>%
  mutate(
    Model = recode(
      Model,
      "Linear Regression" = "LR",
      "Random Forest" = "RF",
      "Gradient Boosting" = "GBM"
    )
  )

ModelPerformanceComparison <- model_comparison_graph %>%
  pivot_longer(
    cols = c(RMSE, MAE, R2),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  ggplot(
    aes(
      x = Model,
      y = Value,
      fill = Model
    )
  ) +
  geom_col(
    width = 0.55,
    show.legend = FALSE
  ) +
  scale_fill_manual(
    values = c(
      "LR" = "#4E79A7",
      "RF" = "#E15759",
      "GBM" = "#59A14F"
    )
  ) +
  facet_wrap(
    ~Metric,
    scales = "free_y"
  ) +
  labs(
    title = "Model Performance Comparison",
    subtitle = "Linear Regression vs Random Forest vs Gradient Boosting",
    x = "Model",
    y = "Metric Value"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      size = 16,
      face = "bold"
    ),
    axis.text.y = element_text(
      size = 16,
      face = "bold"
    ),
    axis.title.x = element_text(
      size = 20,
      face = "bold",
      margin = ggplot2::margin(t = 15)
    ),
    axis.title.y = element_text(
      size = 20,
      face = "bold",
      margin = ggplot2::margin(r = 15)
    ),
    plot.subtitle = element_text(
      face = "bold",
      size = 14
    ),
    plot.title = element_text(
      face = "bold",
      size = 20
    ),
    strip.text = element_text(
      size = 14,
      face = "bold"
    )
  )

ModelPerformanceComparison


ggsave(
  filename = "./outputs/figures/ModelPerformanceComparison.png",
  plot = ModelPerformanceComparison,
  width = 12,
  height = 7,
  dpi = 300
)
