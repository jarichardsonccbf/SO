# import libraries and functions----
library(data.table)
library(forecast)

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
  mutate(year = year(Date)) %>% 
  filter(year != 2016) %>% 
  select(-c(year))

# create ts object for one store----
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
train <- window(store_ts_impt2, end=c(2019,1))
h <- length(store_ts_impt2) - length(train)
ETS <- forecast(ets(train), h=h)
ARIMA <- forecast(auto.arima(train, biasadj=TRUE), h=h)
STL <- stlf(train, h=h, biasadj=TRUE)
NNAR <- forecast(nnetar(train), h=h)
TBATS <- forecast(tbats(train, biasadj=TRUE), h=h)
Combination <- (ETS[["mean"]] + ARIMA[["mean"]] +
                  STL[["mean"]] + NNAR[["mean"]] + TBATS[["mean"]])/5

autoplot(store_ts_impt2) +
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
#MAE too
#relative MSE

#then select lowest