# import libraries and functions----
library(tidyverse)
library(data.table)
library(stringr)
library(plotly)
library(forecast)
library(tidyquant)
library(timetk)
library(sweep)
library(lubridate)
library(zoo)
library(fpp2)

source("functions/StorePlotFunction.R")
source("functions/ImputationFunction.R")

# data preprocessing----
# import conventional retail and supermarkets
super <- fread("data/CR.supermarket.supprette.bc.txt")

# data preprocessing, clean data
colnames(super) <- c("Date","Customer","id","Volume","DNR")
super$DNR <- parse_number(super$DNR)
super$id <- as.character(parse_integer(super$id))
super$Date <- as.Date(super$Date, format = "%Y-%m-%d")

# test if df has "extra" data inside of it, if NA at top, then it does.
count(super,id, Date) %>% count(id)

# NAs are "totals"
colSums(is.na(super))

super <- super[!is.na(super$id),]

colSums(is.na(super))

# remove DNR
super <- super %>%
  select(-c(DNR))

# remove dec 2016
super <- super %>% 
  filter(year != 2016)

# do only stores with more than 1 order
super <- super %>% 
  group_by(Customer) %>% 
  filter(n() > 1)

# create ts object for one store----
# create full vector of dates
full_calendar <- as.data.frame(seq(as.Date("2017/1/1"), as.Date("2019/3/16"), "days"))
full_calendar$Date <- full_calendar$`seq(as.Date("2017/1/1"), as.Date("2019/3/16"), "days")`

# filter out one store
store <- super %>% 
  filter(id == 500119558)

# add known dates to unknown
full_calendar <- full_calendar %>% 
  left_join(store, "Date")

# coerce to time series object
store_ts <- ts(full_calendar$Volume, start = 2017, freq = 365)

#missing data
store_ts_impt <- na.interp(store_ts)

#plot
autoplot(store_ts_impt)

# ALTERNATIVE create ts object for one store----
store_ts_impt2 <- ImputeMissingDates(500119558)
store_ts_impt2 <- ts(store_ts_impt2$Volume, start = 2017, freq = 365)

autoplot(store_ts_impt2) +
  ggtitle("Volume across time") +
  xlab("Year") +
  ylab("Volume")

# General trend----
fit.consMR <- tslm(store_ts_impt2 ~ trend)
summary(fit.consMR)

autoplot(store_ts_impt2, series="Data") +
  autolayer(fitted(fit.consMR), series="Fitted") +
  xlab("Year") + ylab("") +
  guides(colour=guide_legend(title=" "))

# Forecast combinations----
#Here is an example using monthly expenditure on eating out in Australia, from April 1982 to September 2017. We use forecasts from the following models: ETS, ARIMA, STL-ETS, NNAR, and TBATS; and we compare the results using the last 5 years (60 months) of observations.

train <- window(store_ts_impt2, end=c(2019,1))
h <- length(store_ts_impt2) - length(train)
ETS <- forecast(ets(train), h=h)
ARIMA <- forecast(auto.arima(train, biasadj=TRUE), h=h)
STL <- stlf(train, h=h, biasadj=TRUE)
NNAR <- forecast(nnetar(train), h=h)
TBATS <- forecast(tbats(train, biasadj=TRUE), h=h)
Combination <- (ETS[["mean"]] + ARIMA[["mean"]] +
                  STL[["mean"]] + NNAR[["mean"]] + TBATS[["mean"]])/5

t <- autoplot(store_ts_impt2) +
  autolayer(ETS, series="ETS", PI=FALSE) +
  autolayer(ARIMA, series="ARIMA", PI=FALSE) +
  autolayer(STL, series="STL", PI=FALSE) +
  autolayer(NNAR, series="NNAR", PI=FALSE) +
  autolayer(TBATS, series="TBATS", PI=FALSE) +
  autolayer(Combination, series="Combination") +
  xlab("Time") + ylab("Volume")



c(ETS = accuracy(ETS, store_ts_impt2)["Test set","RMSE"],
  ARIMA = accuracy(ARIMA, store_ts_impt2)["Test set","RMSE"],
  `STL-ETS` = accuracy(STL, store_ts_impt2)["Test set","RMSE"],
  NNAR = accuracy(NNAR, store_ts_impt2)["Test set","RMSE"],
  TBATS = accuracy(TBATS, store_ts_impt2)["Test set","RMSE"],
  Combination =
    accuracy(Combination, store_ts_impt2)["Test set","RMSE"])

#then select lowest










#  Step 1: Coerce to a ts object class.
winn_dixie_store_ts

#  Step 2: Apply a model (or set of models)
# attempt this or the auto.arima vs ets function from forecasting.R
train <- window(winn_dixie_store_ts, end=c(2019,65))
h <- length(winn_dixie_store_ts) - length(train)
ETS <- forecast(ets(train), h=h)
ARIMA <- forecast(auto.arima(train, biasadj=TRUE), h=h)
STL <- stlf(train, h=h, biasadj=TRUE)
NNAR <- forecast(nnetar(train), h=h)
TBATS <- forecast(tbats(train, biasadj=TRUE), h=h)
Combination <- (ETS[["mean"]] + ARIMA[["mean"]] +
                  STL[["mean"]] + NNAR[["mean"]] + TBATS[["mean"]])/5

#  Step 3: Forecast the models (similar to predict)

autoplot(winn_dixie_store_ts) +
  autolayer(ETS, series="ETS", PI=FALSE) +
  autolayer(ARIMA, series="ARIMA", PI=FALSE) +
  autolayer(STL, series="STL", PI=FALSE) +
  autolayer(NNAR, series="NNAR", PI=FALSE) +
  autolayer(TBATS, series="TBATS", PI=FALSE) +
  autolayer(Combination, series="Combination") +
  xlab("Time") + ylab("Volume")

c(ETS = accuracy(ETS, winn_dixie_store_ts)["Test set","RMSE"],
  ARIMA = accuracy(ARIMA, winn_dixie_store_ts)["Test set","RMSE"],
  `STL-ETS` = accuracy(STL, winn_dixie_store_ts)["Test set","RMSE"],
  NNAR = accuracy(NNAR, winn_dixie_store_ts)["Test set","RMSE"],
  TBATS = accuracy(TBATS, winn_dixie_store_ts)["Test set","RMSE"],
  Combination =
    accuracy(Combination, winn_dixie_store_ts)["Test set","RMSE"])
#then select lowest
