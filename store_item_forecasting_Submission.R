## Kaggle - Forecasting Challenge
## Store Item Demand


# Libraries Needed
library(vroom)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(forecast)
library(RQuantLib)

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




all_preds <- c()
#all_preds <- append(all_preds,preds$mean)
## Create an SARIMA Model
for(i in 1:n_distinct(store.train$store)){
  for (j in 1:n_distinct(store.train$item)){
    # Run one model for one item and store
    y <- store.train %>% filter(item==j,store==i) %>% 
      pull(sales) %>% ts(data=.,start=1,frequency=365)
    #X.mst <- model.matrix(sales~-1+time+wkend, data=all_sales %>% 
    #                        filter(item==j,store==i,Set=="train"))
    
    mod <- Arima(y,order=c(0,1,1),seasonal=c(0,0,0))
    n_preds <- forecast(mod,h=90)
    all_preds <- append(all_preds,n_preds$mean)
  }
}
plot(all_preds)

store_submission <- data.frame(id=store.test$id,sales=all_preds)

write.csv(x=store_submission,file="./store_predictions_sub.csv",row.names=FALSE)


