
# Libraries Needed
library(vroom)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(forecast)
library(RQuantLib)
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


all_forecasts <- c()
df.p <- store.train
for(i in 1:n_distinct(store.train$store)){
  for (j in 1:n_distinct(store.train$item)){
    # Run one model for one item and store
    df.p <- store.train %>% filter(store==i,item==j)
    selectdf <- df.p %>% mutate(ds=date,y=sales) %>% select(ds,y)
    m <- prophet(selectdf)
    future <- make_future_dataframe(m,periods=90)
    forecast <- predict(m,future)
    
    all_forecasts <- append(all_forecasts,tail(forecast$yhat,90))
  }
}

store_submission2 <- data.frame(id=store.test$id,sales=all_preds)

write.csv(x=store_submission2,file="./store_predictions_sub_prophet.csv",row.names=FALSE)
