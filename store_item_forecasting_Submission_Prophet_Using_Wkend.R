
# Libraries Needed
library(vroom)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(forecast)
library(prophet)

# Read in the data
store.train <- vroom('train.csv')
store.test <- vroom('test.csv')
store.train$Set <- 'train'
store.test$Set <- 'test'

all_sales <- bind_rows(store.train,store.test)

## Feature Engineering
# Create month variable
all_sales <- all_sales %>% mutate(month=as.factor(month(date)))

all_sales <- all_sales %>% mutate(time=year(date)+yday(date)/365)

all_sales <- all_sales %>% mutate(wkday=weekdays(date))

all_sales <- all_sales %>% 
  mutate(wkend = case_when(
    ((.$wkday=="Friday") | (.$wkday=="Saturday") | (.$wkday=="Sunday")) ~ 1, 
    ((.$wkday!="Friday") & (.$wkday!="Saturday") & (.$wkday!="Sunday")) ~ 0,
  ))


# Create model for each item in each store
log_forecasts <- c()
for(i in 1:n_distinct(store.train$store)){
  for (j in 1:n_distinct(store.train$item)){
    
    # Get training data for one store, one item
    df.train <- all_sales %>% filter(Set=='train',store==i,item==j) %>% 
      mutate(ds=date,y=log(sales),wkend=wkend) %>% 
      select(ds,y,wkend) 
    
    # Get test and training data
    df.full <- all_sales %>% filter(store==i,item==j) %>%
      mutate(ds=date,y=log(sales),wkend=wkend) %>% 
      select(ds,y,wkend) 
    
    # Build model using y and wkend
    m <- prophet()
    m <- add_regressor(m,'wkend')
    m <- fit.prophet(m,df.train)
    
    # Predict sales
    future <- make_future_dataframe(m, periods = 90)
    future$wkend <- df.full$wkend
    forecast <- predict(m, future)
    
    log_forecasts <- append(log_forecasts,tail(exp(forecast$yhat),90))
  }
}

forecasts_df <- data.frame(id=store.test$id,sales=log_forecasts)

write.csv(x=forecasts_df,file="./store_predictions_sub_prophet_using_wkend.csv",row.names=FALSE)