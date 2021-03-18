# KAGGLE Competition: Store Item Demand Forecasting

## Purpose and Introduction
The purpose of this project was to predict 3 months of sales for 50 different items at 10 different stores, given 5 years of store-item sales data.

## File Descriptions
test.csv and train.csv - Files containing the data provided by the competition.
store_item_forecasting_script.R - The script containing all code that was used for the competition including data cleaning, feature engineering, and forecasting.
store_item_forecasting_submission.R - The script containing all code for the first submission which employed and ARIMA model for forecasting.
store_item_forecasting_submission_Prophet.R - The script containing all code for the first submission which employed the Prophet function for forecasting.
store_item_forecasting_submission.R - The script containing all code for the first submission which employed the Prophet function and more advanced feature engineering for forecasting.

## Methods for Data Cleaning and Feature Engineering
First, the training and test sets were combined so that all changes made would be applied to the entirety of the data. Any missing values were kept as NA values. After initial cleaning, a time variable was created by combining the year and day variables, making it easier to see how time was a factor in predicting sales. Next, a weekend indicator varaible was also engineered since it is likely that sales on the weekends differed from those during the week.

## Methods for Forecasting
Several different methods were used for predicting sales. All three methods included creating a model for each item in each store, then combining the predictions for all items. The first method was an ARIMA model with differencing and a small, quickly decaying model for correlation and had only time as an explanatory variable. The second method used the prophet function from the prophet library in R, also using only time as an explanatory variable. Both of these models performed simliarly. Last, the third method also used the prophet function but added the second explanatory variable of weekend and a log transformation of the response variable.
