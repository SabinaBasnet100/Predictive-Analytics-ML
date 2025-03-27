# 03 Random Forest Model.R
# Author: Sabina Basnet
# Description: Auto-cleaned and formatted script for GitHub

# Part 2 Modeling
library(caret)
library(tidyverse)
library(skimr)
options(scipen=999)#Turn off scientific notation as global setting

####------------------------------------------------------------------####
# Random_Forest Model
OC_data <- read.csv("/Users/OceanCrestdata_cleaned.csv")
OC_data <- OC_data %>% select(-reservation_status)

# Convert the variables to factors
OC_data <- OC_data %>%
  mutate(
    is_canceled = as.factor(is_canceled),
    is_repeated_guest = as.factor(is_repeated_guest)
  )

# Convert all character and logical variables to factors
OC_data <- OC_data %>% mutate_if(is.character, as.factor) %>% mutate_if(is.logical, as.factor)
skim(OC_data)

# Recode response variable and set reference level
table(OC_data$is_canceled)
# Convert 0 and 1 to factor levels
OC_data$is_canceled <- factor(OC_data$is_canceled, 
                              levels = c(0, 1), 
                              labels = c("Not_Canceled", "Canceled"))

# Relevel to set "Canceled" as the reference level
OC_data$is_canceled <- relevel(OC_data$is_canceled, ref = "Canceled")

# Verify the levels after the changes
levels(OC_data$is_canceled)


#Step 1: Partition into training and test data
#create training and testing
set.seed(99) #set random seed
index <- createDataPartition(OC_data$is_canceled , p = .8,list = FALSE)
OC_train <- OC_data[index,]
OC_test <- OC_data[-index,]

#Step 2: Fit the model
# Train model with preprocessing & cv
library(randomForest)

library(doParallel)

#total number of cores on your computer
num_cores<-detectCores(logical=FALSE)
num_cores

#start parallel processing
cl <- makePSOCKcluster(num_cores-2)
registerDoParallel(cl)

# Check the number of workers
getDoParWorkers()  # Should return the number of cores being used


# Set seed and train Random Forest model
set.seed(8)
model_rf <- train(is_canceled~.,
                  data = OC_train,
                  method = "rf",
                  tuneGrid= expand.grid(mtry = c(1,3,6,9)),
                  trControl =trainControl(method="cv", 
                                          number = 5,
                                          classProbs = TRUE,
                                          summaryFunction = twoClassSummary),
                  metric = "ROC",
                  ntree = 100)

model_rf
plot(model_rf)
model_rf$bestTune

plot(varImp(model_rf))

#Step 3: Get Predictions using Testing Set Data
OC_prob <- predict(model_rf, OC_test, type = "prob")

#Step 4: Evaluate Model Performance

library(ROCR)

pred = prediction(OC_prob$Canceled,
                  OC_test$is_canceled,
                  label.ordering = c("Not_Canceled","Canceled"))

pref = performance(pred,"tpr","fpr")
plot(pref,colorize=TRUE)

unlist(slot(performance(pred, "auc"),"y.values"))


# Stop parallel processing
stopCluster(cl)





# Get variable importance
varImpPlot <- varImp(model_rf)

# Convert to a data frame for easier handling
importance_df <- as.data.frame(varImpPlot$importance)

# Sort the features by their importance in descending order
sorted_importance <- importance_df %>% arrange(desc(Overall))

# Display the top 10 features
top_10_features <- head(sorted_importance, 10)
print(top_10_features)

nzv <- nearZeroVar(OC_data)
# Display the names of the columns with near-zero variance
print(names(OC_data)[nzv])


# Remove the near-zero variance columns
OC_data_cleaned <- OC_data[, -nzv]

# Check the new dataset structure
str(OC_data_cleaned)


# Train the model again with the cleaned dataset
set.seed(8)
model_rf_cleaned <- train(is_canceled~., data = OC_data_cleaned, method = "rf", tuneGrid = expand.grid(mtry = c(1,3,6,9)), trControl = trainControl(method="cv", number=5, classProbs=TRUE, summaryFunction=twoClassSummary), metric="ROC")

# Evaluate the model performance
print(model_rf_cleaned)




# Example: Check distribution of one variable
table(OC_data$babies)
table(OC_data$is_repeated_guest)
summary(OC_data$previous_cancellations)








