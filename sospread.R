# import libraries and functions----
library(tidyverse)
library(data.table)
library(forecast)

source("functions/FillGapsFunction.R")

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
  mutate(year = year(Date)) %>% 
  filter(year != 2016) %>% 
  select(-c(year))

# select a store
store <- super %>% 
  filter(id == 500119558)

# spread orders to fill gaps----
gaps_filled <- OrderGaps(500119558)
gaps_filled$plot.compare
gaps_filled <- gaps_filled$values.compare

# create ts object for one store----
store_ts <- ts(gaps_filled$`full_calendar_filled$Volume`, start = 2017, freq = 365)

autoplot(store_ts) +
  ggtitle("Volume across time") +
  xlab("Year") +
  ylab("Volume")

# General trend----
fit.consMR <- tslm(store_ts ~ trend)
summary(fit.consMR)

autoplot(store_ts, series="Data") +
  autolayer(fitted(fit.consMR), series="Fitted") +
  xlab("Year") + ylab("") +
  guides(colour=guide_legend(title=" "))

# Forecast combinations----
train <- window(store_ts, end=c(2019,1))
h <- length(store_ts) - length(train)
ETS <- forecast(ets(train), h=h)
ARIMA <- forecast(auto.arima(train, biasadj=TRUE), h=h)
STL <- stlf(train, h=h, biasadj=TRUE)
NNAR <- forecast(nnetar(train), h=h)
TBATS <- forecast(tbats(train, biasadj=TRUE), h=h)
Combination <- (ETS[["mean"]] + ARIMA[["mean"]] +
                  STL[["mean"]] + NNAR[["mean"]] + TBATS[["mean"]])/5

autoplot(store_ts) +
  autolayer(ETS, series="ETS", PI=FALSE) +
  autolayer(ARIMA, series="ARIMA", PI=FALSE) +
  autolayer(STL, series="STL", PI=FALSE) +
  autolayer(NNAR, series="NNAR", PI=FALSE) +
  autolayer(TBATS, series="TBATS", PI=FALSE) +
  autolayer(Combination, series="Combination") +
  xlab("Time") + ylab("Volume")

c(ETS = accuracy(ETS, store_ts)["Test set","RMSE"],
  ARIMA = accuracy(ARIMA, store_ts)["Test set","RMSE"],
  `STL-ETS` = accuracy(STL, store_ts)["Test set","RMSE"],
  NNAR = accuracy(NNAR, store_ts)["Test set","RMSE"],
  TBATS = accuracy(TBATS, store_ts)["Test set","RMSE"],
  Combination =
    accuracy(Combination, store_ts)["Test set","RMSE"])

#MAE too
#relative MSE

#then select lowest