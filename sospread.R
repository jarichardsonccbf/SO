# import libraries and functions----
library(tidyverse)
library(data.table)
library(forecast)

source("functions/FillGapsFunction.R")

# data preprocessing----
# import wawa 2017-Q1 2019
wawa <- fread("data/wawa.csv")

# data preprocessing, clean columns
wawa <- wawa %>%
  filter(OrderType == "ZOR") %>% 
  filter(UOM != "EA") %>% 
  select(-c(V1, SalesOrder, SalesOffice, PrimaryGroupDesc, UOM, OrderType, NetVale, Net.Price))

wawa <- wawa %>% 
  rename(outlet = SoldTo, item = MaterialNo)

wawa <- wawa %>%
  transform(OrderDate = as.Date(as.character(OrderDate), "%Y%m%d")) %>% 
  transform(DeliveryDate = as.Date(as.character(DeliveryDate), "%Y%m%d"))

# spread orders to fill gaps at wawa store # and item #----
wawa_slow_consistent <- OrderGaps(wawa, 600379764, 133131)
wawa_slow_consistent$plot.compare
wawa_slow_consistent$values.compare

wawa_fast_inconsistent <- OrderGaps(wawa, 600379764, 115583)
wawa_fast_inconsistent$plot.compare
wawa_fast_inconsistent$values.compare

# create ts object for spread and one with zeroes----
# slow consistent
spread <- ts(wawa_slow_consistent$values.compare$OrderedQtySpread, start = 2017, freq = 365)
zeroes <- ts(wawa_slow_consistent$values.compare$OrderedQtyZeroes, start = 2017, freq = 365)

# fast inconsistent
spread <- ts(wawa_fast_inconsistent$values.compare$OrderedQtySpread, start = 2017, freq = 365)
zeroes <- ts(wawa_fast_inconsistent$values.compare$OrderedQtyZeroes, start = 2017, freq = 365)

# create train and test sets
spread_train <- window(spread, end = c(2018, 365))
spread_test <- window(spread, start = c(2019, 1))

zeroes_train <- window(zeroes, end = c(2018, 365))
zeroes_test <- window(zeroes, start = c(2019, 1))

# General trend----
fit.consMR <- tslm(spread ~ trend)
summary(fit.consMR)

fit.consMR <- tslm(zeroes ~ trend)
summary(fit.consMR)

autoplot(spread, series="Data") +
  autolayer(fitted(fit.consMR), series="Fitted") +
  xlab("Year") + ylab("") +
  guides(colour=guide_legend(title=" "))

autoplot(zeroes, series="Data") +
  autolayer(fitted(fit.consMR), series="Fitted") +
  xlab("Year") + ylab("") +
  guides(colour=guide_legend(title=" "))

# Forecast combinations SPREAD----
h <- length(spread_test)
ETS <- forecast(ets(spread_train), h = h)
ARIMA <- forecast(auto.arima(store_ts, biasadj=TRUE), h=h)
# STL <- stlf(spread_train, h = h, biasadj = TRUE)
NNAR <- forecast(nnetar(spread_train, p = 2, h = h))
TBATS <- forecast(tbats(spread_train, biasadj = TRUE), h = h)
Combination <- (ETS[["mean"]] + ARIMA[["mean"]] + NNAR[["mean"]] + TBATS[["mean"]])/5

autoplot(spread) +
  autolayer(ETS, series="ETS", PI=FALSE) +
  autolayer(ARIMA, series="ARIMA", PI=FALSE) +
# autolayer(STL, series="STL", PI=FALSE) +
  autolayer(NNAR, series="NNAR", PI=FALSE) +
  autolayer(TBATS, series="TBATS", PI=FALSE) +
  autolayer(Combination, series="Combination") +
  xlab("Time") + ylab("Volume")

c(ETS = accuracy(ETS, spread)["Test set","RMSE"],
  ARIMA = accuracy(ARIMA, spread)["Test set","RMSE"],
# `STL-ETS` = accuracy(STL, spread)["Test set","RMSE"],
  NNAR = accuracy(NNAR, spread)["Test set","RMSE"],
  TBATS = accuracy(TBATS, spread)["Test set","RMSE"],
  Combination = accuracy(Combination, spread)["Test set","RMSE"])
# then select lowest

c(ETS = accuracy(ETS, spread)["Test set","MAE"],
  ARIMA = accuracy(ARIMA, spread)["Test set","MAE"],
# `STL-ETS` = accuracy(STL, spread)["Test set","MAE"],
  NNAR = accuracy(NNAR, spread)["Test set","MAE"],
  TBATS = accuracy(TBATS, spread)["Test set","MAE"],
  Combination = accuracy(Combination, spread)["Test set","MAE"])

# Forecast combinations ZEROES----
h <- length(zeroes_test)
ETS <- forecast(ets(zeroes_train), h = h)
ARIMA <- forecast(auto.arima(zeroes_train, biasadj=TRUE), h=h)
# STL <- stlf(zeroes_test, h = h, biasadj = TRUE)
NNAR <- forecast(nnetar(zeroes_train, p = 1, h = h))
TBATS <- forecast(tbats(zeroes_train, biasadj = TRUE), h = h)
Combination <- (ETS[["mean"]] + ARIMA[["mean"]] + NNAR[["mean"]] + TBATS[["mean"]])/5

autoplot(zeroes_train) +
  autolayer(ETS, series="ETS", PI=FALSE) +
  autolayer(ARIMA, series="ARIMA", PI=FALSE) +
# autolayer(STL, series="STL", PI=FALSE) +
  autolayer(NNAR, series="NNAR", PI=FALSE) +
  autolayer(TBATS, series="TBATS", PI=FALSE) +
  autolayer(Combination, series="Combination") +
  xlab("Time") + ylab("Volume")

c(ETS = accuracy(ETS, zeroes)["Test set","RMSE"],
  ARIMA = accuracy(ARIMA, zeroes)["Test set","RMSE"],
# `STL-ETS` = accuracy(STL, store_ts)["Test set","RMSE"],
  NNAR = accuracy(NNAR, zeroes)["Test set","RMSE"],
  TBATS = accuracy(TBATS, zeroes)["Test set","RMSE"],
  Combination = accuracy(Combination, zeroes)["Test set","RMSE"])
# then select lowest

c(ETS = accuracy(ETS, zeroes)["Test set","MAE"],
  ARIMA = accuracy(ARIMA, zeroes)["Test set","MAE"],
#  `STL-ETS` = accuracy(STL, zeroes)["Test set","MAE"],
  NNAR = accuracy(NNAR, zeroes)["Test set","MAE"],
  TBATS = accuracy(TBATS, zeroes)["Test set","MAE"],
  Combination = accuracy(Combination, zeroes)["Test set","MAE"])
