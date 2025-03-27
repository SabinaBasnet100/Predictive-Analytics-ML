# 🏨 Hotel Booking Cancellation Prediction & Customer Segmentation (R Project)

This project analyzes hotel booking data to predict cancellations and segment customers using machine learning in R. It applies techniques such as Random Forest, XGBoost with SHAP analysis, and K-Means clustering to help Ocean Crest Resort reduce cancellations and improve customer targeting.

---

## 🎯 Business Objective

- Identify and reduce high booking cancellation patterns
- Predict future cancellations using historical booking data
- Segment guests based on usage patterns for tailored offers

---

## 🧠 Techniques Used

- ✅ Data Cleaning & Feature Engineering
- ✅ Random Forest Classification
- ✅ XGBoost + SHAP Analysis for Feature Importance
- ✅ K-Means Clustering for Segmentation
- ✅ ROC Curve, AUC, Confusion Matrix, and Model Tuning

---

## 📁 Files Included

- `01_data_cleaning.R` – Data preprocessing and transformation  
- `02_full_workflow.R` – EDA, transformation, and cancellation feature creation  
- `03_random_forest_model.R` – Final Random Forest modeling and evaluation  
- `04_xgboost_shap_model.R` – XGBoost with SHAP analysis and insights  
- `README.md` – This documentation file  

---

## 📊 Key Insights

- Guests with long lead times, special requests, and OTA bookings are more likely to cancel
- Guests from certain countries (e.g., Portugal) had significantly higher cancellation rates
- SHAP analysis revealed `deposit_type`, `customer_type`, and `booking_changes` as key predictors

---

## 📌 Business Impact

- Improve communication with high-risk customer segments
- Offer incentives for customers at risk of canceling
- Enhance planning and revenue forecasting

---

## 👩‍💻 Author

**Sabina Basnet**    
🔗 [GitHub](https://github.com/SabinaBasnet100)
