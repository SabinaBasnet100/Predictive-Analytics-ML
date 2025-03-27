# 04 Xgboost Shap Model.R
# Author: Sabina Basnet
# Description: Auto-cleaned and formatted script for GitHub

# Load necessary libraries
library(caret)
library(tidyverse)
library(doParallel)
library(SHAPforxgboost)
library(pROC)
library(ROCR)


# Load the data
OC_data <- read.csv("/Users/rong/Desktop/Predictive_Analytics/Final/OceanCrestdata_cleaned.csv")
OC_data <- OC_data %>% select(-reservation_status)

# Convert target variable to a factor
OC_data$is_canceled <- as.factor(OC_data$is_canceled)

# Create dummy variables using model.matrix() (excluding the intercept)
OC_data_transformed <- model.matrix(is_canceled ~ . - 1, data = OC_data)  # '-1' removes the intercept column

# Add back the target variable as a factor for training
OC_data_transformed <- as.data.frame(OC_data_transformed)
OC_data_transformed$is_canceled <- as.factor(as.character(OC_data$is_canceled))

# Split the data into training and testing sets
set.seed(99)
index <- createDataPartition(OC_data_transformed$is_canceled, p = .8, list = FALSE)
OC_train <- OC_data_transformed[index,]
OC_test <- OC_data_transformed[-index,]

# Set up parallel processing
num_cores <- detectCores() - 2
cl <- makePSOCKcluster(num_cores)
registerDoParallel(cl)

#### Full Model----
# Train the full XGBoost model using the correct factor outcome
set.seed(8)
model_gbm <- train(is_canceled ~ .,
                   data = OC_train,
                   method = "xgbTree",
                   trControl = trainControl(method = "cv", number = 5),
                   tuneGrid = expand.grid(
                     nrounds = 300,           # Optimal number of boosting rounds
                     eta = 0.02,              # Optimal learning rate
                     max_depth = 5,           # Optimal max tree depth
                     gamma = 0.1,             # Optimal gamma value
                     colsample_bytree = 1,    # Optimal column sampling ratio
                     min_child_weight = 1,    # Optimal min child weight
                     subsample = 0.7          # Optimal subsample ratio
                   ),
                   verbose = FALSE)


# plot(model_gbm)
# model_gbm$bestTune
# plot(varImp(model_gbm))


# Full model prediction and AUC calculation
pred_prob_full <- predict(model_gbm, OC_test, type = "prob")

# Calculate the AUC for full model
roc_obj_full <- roc(OC_test$is_canceled, as.numeric(pred_prob_full[, 2]))  # Assuming second column is class "1"
auc_value_full <- auc(roc_obj_full)
print(paste("Full model AUC:", auc_value_full))

# Plot ROC curve with color gradient using ROCR for Full Model
pred_full <- prediction(pred_prob_full[, 2], OC_test$is_canceled)  # Probabilities for class "1"
perf_full <- performance(pred_full, "tpr", "fpr")
plot(perf_full, colorize = TRUE, main = "ROC Curve with Color Gradient for Full Model")


####**SHAP analysis** to identify important features----
# 
Xdata <- as.matrix(select(OC_train, -is_canceled))

# Align feature names between model and SHAP
model_feature_names <- gsub("`", "", model_gbm$finalModel$feature_names)
model_gbm$finalModel$feature_names <- model_feature_names
Xdata <- Xdata[, model_gbm$finalModel$feature_names]

# Perform SHAP analysis
shap <- shap.prep(model_gbm$finalModel, X_train = Xdata)

# Calculate SHAP importance
shap_importance <- shap %>%
  group_by(variable) %>%
  summarise(Importance = mean(abs(value)))

# View the top features by SHAP importance and filter important features
shap_importance <- shap_importance %>% arrange(desc(Importance))
important_features <- shap_importance %>% filter(Importance > 0.01)
print(important_features)

# **Subset the data using only important features**
# Ensure that 'important_features$variable' matches with the column names in OC_train
valid_features <- intersect(important_features$variable, colnames(OC_train))

# Subset the training and test data using the valid features
OC_train_reduced <- OC_train[, c(valid_features, "is_canceled")]
OC_test_reduced <- OC_test[, valid_features]

#### Reduced Model----
# **Retrain the XGBoost model using the reduced feature set**
set.seed(8)
model_gbm_reduced <- train(is_canceled ~ .,
                           data = OC_train_reduced,
                           method = "xgbTree",
                           trControl = trainControl(method = "cv", number = 5),
                           tuneGrid = expand.grid(
                             nrounds = 300,
                             eta = 0.02,
                             max_depth = 5,
                             gamma = 0,
                             colsample_bytree = 1,
                             min_child_weight = 1,
                             subsample = 0.7),
                           verbose = FALSE)


#plot(model_gbm_reduced). ## Since tuning parameters are fixed, skip the plot
model_gbm_reduced$bestTune
plot(varImp(model_gbm_reduced))



# **Get predictions for the reduced model on the test set**
pred_prob_reduced <- predict(model_gbm_reduced, OC_test_reduced, type = "prob")

# **Calculate the AUC using pROC for the reduced model**
roc_obj_reduced <- roc(OC_test$is_canceled, as.numeric(pred_prob_reduced[, 2]))  # Assuming second column is class "1"
auc_value_reduced <- auc(roc_obj_reduced)
print(paste("Reduced model AUC:", auc_value_reduced))

# **Plot ROC curve with color gradient using ROCR for Reduced Model**
pred_reduced <- prediction(pred_prob_reduced[, 2], OC_test$is_canceled)
perf_reduced <- performance(pred_reduced, "tpr", "fpr")
plot(perf_reduced, colorize = TRUE, main = "ROC Curve with Color Gradient for Reduced Model")


####SHAP analysis on the reduced model----
#SHAP analysis on the reduced model
# Perform SHAP analysis on the reduced model using the reduced dataset
Xdata_reduced <- as.matrix(select(OC_train_reduced, -is_canceled))  # Use the preprocessed data without the target

# Ensure feature names match between the reduced dataset and the reduced model
model_feature_names_reduced <- gsub("`", "", model_gbm_reduced$finalModel$feature_names)
model_gbm_reduced$finalModel$feature_names <- model_feature_names_reduced

# Subset Xdata_reduced to match the feature names used in the reduced model
Xdata_reduced <- Xdata_reduced[, model_gbm_reduced$finalModel$feature_names]

# Perform SHAP analysis for the reduced model
shap_reduced <- shap.prep(model_gbm_reduced$finalModel, X_train = Xdata_reduced)

# SHAP importance summary for the reduced model
shap.plot.summary(shap_reduced)

# Calculate SHAP importance as a data frame
shap_importance_reduced <- shap_reduced %>%
  group_by(variable) %>%
  summarise(Importance = mean(abs(value)))

# View the top features by SHAP importance in the reduced model
shap_importance_reduced <- shap_importance_reduced %>% arrange(desc(Importance))
print(shap_importance_reduced)



# SHAP dependence plots for the top 5 important features
top5_features <- head(shap_importance_reduced$variable, 5)
for (feature in top5_features) {
  p <- shap.plot.dependence(shap_reduced, x = feature, color_feature = "auto") + ggtitle(paste("Dependence plot for", feature))
  print(p)
}






# **Stop parallel processing**
stopCluster(cl)

####Export the Subset Dataset ----

# Subset the training and test datasets to keep only the important features and target variable
OC_train_reduced <- OC_train[, c(valid_features, "is_canceled")]
OC_test_reduced <- OC_test[, c(valid_features, "is_canceled")]


# Combine the two datasets (train and test)
OC_combined <- rbind(OC_train_reduced, OC_test_reduced)

# Print the structure of the combined dataset
print("Combined dataset (train and test):")
str(OC_combined)

# Optionally, save the combined dataset to a CSV file
write.csv(OC_combined, "OC_combined_reduced.csv", row.names = FALSE)






