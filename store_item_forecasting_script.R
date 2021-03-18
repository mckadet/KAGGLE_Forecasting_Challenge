## Kaggle - Forecasting Challenge
## Store Item Demand


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

## Exploratory Data Analysis
with(store.train,table(item,store))

# Sales by Month
ggplot(data=store.train,store.train %>% filter(item==1),
       mapping=aes(x=month(date) %>% as.factor(),y=sales)) + geom_boxplot()

ggplot(store.train, aes(x=Datetime, y=sales)) + geom_line()

# Sales by Store
ggplot(store.train %>% filter(item==17),
       mapping=aes(x=date, y=sales,col=as.factor(store))) + geom_point()

ggplot(all_sales %>% filter(item==17, store==7), aes(x=time,y=sales)) + 
  geom_line() + geom_smooth(method="lm")



## LM with time and month
mst.lm <- lm(sales~month+time,data=all_sales %>% filter(item==17, store==7))
fit.vals <- fitted(mst.lm)

plot(x=(all_sales %>% filter(item==17,store==7) %>% pull(time)),
     y=(all_sales %>% filter(item==17,store==7) %>% pull(sales)),type="line")
lines((all_sales %>% filter(item==17,store==7,!is.na(sales)) %>% pull(time)),
       fit.vals,col="red",lwd=2)

ggplot(store.train %>% filter(item==17), aes(x=date,y=sales)) + 
  geom_line() + facet_wrap(~as.factor(store))

ggplot(store.train, aes(x=as.factor(item),y=sales)) + 
  geom_boxplot() + facet_wrap(~as.factor(store))

ggplot(store.train, aes(x=as.factor(item),y=sales)) + 
  geom_boxplot() + facet_wrap(~as.factor(store))

ggplot(store.train %>% filter(item==17), aes(x=as.factor(store),y=sales)) + 
  geom_boxplot()

play <- all_sales %>% filter(item==17,store==7,Set=="train") %>% pull(sales)

length(play)
## SARIMA(p,d,g,P,D,Q)
y <- all_sales %>% filter(item==17,store==7,Set=="train") %>% pull(sales) %>% 
  ts(data=.,start=1, frequency=365)
X.mst <- model.matrix(sales~-1+time+wkend, data=all_sales %>% 
                    filter(item==17,store==7,Set=="train"))

arima.mod <- auto.arima(y=y, max.p=2,max.q=2)

my.arima <- Arima(y,order=c(0,1,1),seasonal=c(0,0,0))

#my.arima <- Arima(y, order=c(1,0,2), seasonal=c(0,0,0), xreg=X)
preds <- forecast(my.arima,h=90)
plot(preds)


### Submission 1
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


### Submission 2 - using auto.arima
all_preds2 <- c()
#all_preds <- append(all_preds,preds$mean)
## Create an SARIMA Model
for(i in 1:n_distinct(store.train$store)){
  for (j in 1:n_distinct(store.train$item)){
    # Run one model for one item and store
    y <- store.train %>% filter(item==j,store==i) %>% 
      pull(sales) %>% ts(data=.,start=1,frequency=365)
    #X.mst <- model.matrix(sales~-1+time+wkend, data=all_sales %>% 
    #                        filter(item==j,store==i,Set=="train"))
    
    mod2 <- auto.arima(y=y,max.p=2,max.d=0,max.q=2,max.P=2,max.Q=2,max.D=0)
    n_preds2 <- forecast(mod2,h=90)
    all_preds2 <- append(all_preds2,n_preds2$mean)
  }
}

plot(all_preds)

### Submission 3 - Using Prophet
# Select Explanatory Variables
red <- store.train %>% filter(store==7,item==17)

# Prepare dataframe for prophet
df <- red
df <- df %>% select(date,sales)
colnames(df) <- c("ds","y")

m <- prophet()
#m <- add_regressor(m,'wkend')

m <- fit.prophet(m,df)

future <- make_future_dataframe(m, periods = 1)
#future$wkend <- df$wkend
forecast <- predict(m, future)
plot(m,forecast)

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

tail(forecast$yhat,90)

plot(all_forecasts)

preds <- tail(all_forecasts,n=45000)
plot(preds)


### Submission 3
mst <- all_sales %>% filter(Set=='train',store==6,item==4) %>% 
  mutate(ds=date,y=log(sales),wkend=wkend) %>% 
  select(ds,y,wkend) 

full <- all_sales %>% filter(store==6,item==4) %>%
  mutate(ds=date,y=log(sales),wkend=wkend) %>% 
  select(ds,y,wkend) 

m <- prophet()
m <- add_regressor(m,'wkend')

m <- fit.prophet(m,mst)

zeros <- all_sales %>% filter(sales==0)
future <- make_future_dataframe(m, periods = 90)
future$wkend <- full$wkend
forecast <- predict(m, future)

plot(m,forecast)

new_vals <- tail(exp(forecast$yhat),90)

# Create model for each item in each store
log_forecasts <- c()
for(i in 1:n_distinct(store.train$store)){
  for (j in 1:n_distinct(store.train$item)){
    
    if((i != 6) & (j != 4)){
      # Get training data for one store, one item
      df.train <- all_sales %>% filter(Set=='train',store==i,item==j) %>% 
        mutate(ds=date,y=log(sales),wkend=wkend) %>% 
        select(ds,y,wkend) 
      # Get test and training data
      df.full <- all_sales %>% filter(store==i,item==j) %>%
        mutate(ds=date,y=log(sales),wkend=wkend) %>% 
        select(ds,y,wkend)
    }
    else {
      df.train <- all_sales %>% filter(Set=='train',store==i,item==j) %>% 
        mutate(ds=date,y=sales,wkend=wkend) %>% 
        select(ds,y,wkend) 
      # Get test and training data
      df.full <- all_sales %>% filter(store==i,item==j) %>%
        mutate(ds=date,y=sales,wkend=wkend) %>% 
        select(ds,y,wkend)
    }
    
    
    
    # Build model using y and wkend
    m <- prophet()
    m <- add_regressor(m,'wkend')
    m <- fit.prophet(m,df.train)
    
    # Predict sales
    future <- make_future_dataframe(m, periods = 90)
    future$wkend <- df.full$wkend
    forecast <- predict(m, future)
    
    if((i != 6) & (j != 4)){
      log_forecasts <- append(log_forecasts,tail(exp(forecast$yhat),90))
    }
    else{
      log_forecasts <- append(log_forecasts,tail(forecast$yhat,90))
    }
  }
}
