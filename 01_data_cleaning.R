# 01 Data Cleaning.R
# Author: Sabina Basnet
# Description: Auto-cleaned and formatted script for GitHub

# Load necessary libraries
library(dplyr)
library(tidyr)
library(caret)

# Load the dataset
data <- read.csv("OceanCrestdata.csv", header=T)

# Load necessary libraries
library(dplyr)
library(tidyr)
library(caret)

# Load the dataset
data <- read.csv("OceanCrestdata.csv")

# 1. Handling Each Categorical Variable
# Convert categorical variables into numerical form
# One-Hot Encoding
data <- data %>%
  mutate(across(c(hotel, meal, market_segment, distribution_channel, customer_type, reservation_status),
                as.factor)) %>%
  # Use model.matrix for one-hot encoding
  bind_cols(model.matrix(~ hotel + meal + market_segment + distribution_channel + customer_type + reservation_status - 1, data)) %>%
  select(-c(hotel, meal, market_segment, distribution_channel, customer_type, reservation_status))

# Label Encoding
# Convert arrival_date_month to numeric
data <- data %>%
  mutate(arrival_date_month = as.factor(arrival_date_month)) %>%
  mutate(arrival_date_month = as.numeric(arrival_date_month))

# 2. Creating New Variables
# Stay Duration
data <- data %>%
  mutate(total_stay_duration = stays_in_weekend_nights + stays_in_week_nights)

# Booking Period
data <- data %>%
  mutate(booking_period = lead_time)

# Is_Repeated_Guest
data <- data %>%
  mutate(is_repeated_guest_binary = ifelse(is_repeated_guest == 1, 1, 0))

# 3. Handling Missing Values
# Mode Imputation for categorical variables
mode_impute <- function(x) {
  tab <- table(x, useNA = "ifany")
  names(tab)[which.max(tab)]
}

data$agent[is.na(data$agent)] <- mode_impute(data$agent)
data$company[is.na(data$company)] <- mode_impute(data$company)

# Median Imputation for numerical variables
data$children[is.na(data$children)] <- median(data$children, na.rm = TRUE)
data$adults[is.na(data$adults)] <- median(data$adults, na.rm = TRUE)
data$adr[is.na(data$adr)] <- median(data$adr, na.rm = TRUE)

# Drop Columns with more than 70% missing values
missing_percentage <- colMeans(is.na(data))
columns_to_drop <- names(missing_percentage[missing_percentage > 0.70])
data <- data %>%
  select(-all_of(columns_to_drop))

# Save the final dataset
write.csv(data, "OceanCrestdata_cleaned.csv", row.names = FALSE)


