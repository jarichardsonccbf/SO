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
spread <- ts(wawa_slow_consistent$values.compare$OrderedQtySpread, start = 2017, freq = 365)
zeroes <- ts(wawa_slow_consistent$values.compare$OrderedQtyZeroes, start = 2017, freq = 365)

# train <- window(store_ts, end = c(2018, 365))
# test <- window(store_ts, start = c(2019, 1))

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

# Forecast combinations----
train <- window(store_ts, end=c(2019, 1))
h <- length(store_ts) - length(train)
ETS <- forecast(ets(train), h = h)
ARIMA <- forecast(auto.arima(train, biasadj=TRUE), h=h)
STL <- stlf(train, h = h, biasadj = TRUE)
NNAR <- forecast(nnetar(train), h = h)

fit <- nnetar(train, p = 1)
ggplotly(autoplot(forecast(fit,h=30)))

sim <- ts(matrix(0, nrow=(30L), ncol=9L),
          start=2019, freq = 365)

for(i in seq(9))
  sim[,i] <- simulate(fit, nsim=30L)
ggplotly(autoplot(train) + autolayer(sim))

fcast <- forecast(fit, PI=FALSE, h=30)
autoplot(fcast)

TBATS <- forecast(tbats(train, biasadj = TRUE), h = h)
Combination <- (ETS[["mean"]] + ARIMA[["mean"]] + STL[["mean"]] + NNAR[["mean"]] + TBATS[["mean"]])/5

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
  Combination = accuracy(Combination, store_ts)["Test set","RMSE"])
# then select lowest

c(ETS = accuracy(ETS, store_ts)["Test set",],
  ARIMA = accuracy(ARIMA, store_ts)["Test set",],
  `STL-ETS` = accuracy(STL, store_ts)["Test set",],
  NNAR = accuracy(NNAR, store_ts)["Test set",],
  TBATS = accuracy(TBATS, store_ts)["Test set",],
  Combination = accuracy(Combination, store_ts)["Test set",])

# full selection for each alternates----

# bias adjust FALSE uses medians, TRUE uses means
# lambda set as auto transforms data using BoxCox.lambda

# Forecast combinations----
train <- window(store_ts, end=c(2019, 1))
h <- length(store_ts) - length(train)

# ETS <- forecast(ets(train), h = h)  # base model
ETS_base    <- forecast(ets(train), biasadj = FALSE, h = h)
ETS_biasadj <- forecast(ets(train), biasadj = TRUE , h = h)

c(ETS_base    = accuracy(ETS_base   , store_ts)["Test set","RMSE"],
  ETS_biasadj = accuracy(ETS_biasadj, store_ts)["Test set","RMSE"])

## ## ## ##
ARIMA_T_a <- forecast(auto.arima(train, lambda = "auto", biasadj = TRUE),  h = h)
ARIMA_F_a <- forecast(auto.arima(train, lambda = "auto", biasadj = FALSE), h = h)
ARIMA_T_n <- forecast(auto.arima(train, lambda = NULL  , biasadj = TRUE),  h = h)
ARIMA_F_n <- forecast(auto.arima(train, lambda = NULL  , biasadj = FALSE), h = h)


ARIMA_T_a_f <- forecast(auto.arima(train, xreg=fourier(train, K = 30), lambda = "auto", biasadj = TRUE) ,
                        h = h, xreg = fourier(train, K = 30, h = h))
ARIMA_F_a_f <- forecast(auto.arima(train, xreg=fourier(train, K = 30), lambda = "auto", biasadj = FALSE),
                        h = h, xreg = fourier(train, K = 30, h = h))
ARIMA_T_n_f <- forecast(auto.arima(train, xreg=fourier(train, K = 30), lambda = NULL  , biasadj = TRUE) ,                         h = h, xreg = fourier(train, K = 30, h = h))
ARIMA_F_n_f <- forecast(auto.arima(train, xreg=fourier(train, K = 30), lambda = NULL  , biasadj = FALSE),                         h = h, xreg = fourier(train, K = 30, h = h))

# cafe04 <- window(auscafe, start=2004)
# plots <- list()
# for (i in seq(6)) {
#   fit <- auto.arima(cafe04, xreg = fourier(cafe04, K = i),
#                     seasonal = FALSE, lambda = 0)
#   plots[[i]] <- autoplot(forecast(fit,
#                                   xreg=fourier(cafe04, K=i, h=24))) +
#     xlab(paste("K=",i,"   AICC=",round(fit[["aicc"]],2))) +
#     ylab("") + ylim(1.5,4.7)
# }
# gridExtra::grid.arrange(
#   plots[[1]],plots[[2]],plots[[3]],
#   plots[[4]],plots[[5]],plots[[6]], nrow=3)


c(ARIMA_T_a   = accuracy(ARIMA_T_a, store_ts)["Test set","MAE"],
  ARIMA_F_a   = accuracy(ARIMA_F_a, store_ts)["Test set","MAE"],
  ARIMA_T_n   = accuracy(ARIMA_T_n, store_ts)["Test set","MAE"],
  ARIMA_F_n   = accuracy(ARIMA_F_n, store_ts)["Test set","MAE"],
  ARIMA_T_a_f = accuracy(ARIMA_T_a, store_ts)["Test set","MAE"],
  ARIMA_F_a_f = accuracy(ARIMA_F_a, store_ts)["Test set","MAE"],
  ARIMA_T_n_f = accuracy(ARIMA_T_n, store_ts)["Test set","MAE"],
  ARIMA_F_n_f = accuracy(ARIMA_F_n, store_ts)["Test set","MAE"])

## ## ## ##

STL_T_a <- stlf(train, h = h, lambda = "auto", biasadj = TRUE)
STL_F_a <- stlf(train, h = h, lambda = "auto", biasadj = FALSE)
STL_T_n <- stlf(train, h = h, lambda = NULL, biasadj = TRUE)
STL_F_n <- stlf(train, h = h, lambda = NULL, biasadj = FALSE)

c(STL_T_a = accuracy(STL_T_a, store_ts)["Test set","RMSE"],
  STL_F_a = accuracy(STL_F_a, store_ts)["Test set","RMSE"],
  STL_T_n = accuracy(STL_T_n, store_ts)["Test set","RMSE"],
  STL_F_n = accuracy(STL_F_n, store_ts)["Test set","RMSE"])

## ## ## ##

NNAR_a <- forecast(nnetar(train), lambda = "auto", h = h)
NNAR_n <- forecast(nnetar(train), lambda = NULL, h = h)

c(accuracy(NNAR_a, store_ts)["Test set","RMSE"],
  accuracy(NNAR_n, store_ts)["Test set","RMSE"])

for(i in seq(9))
  store_ts[,i] <- simulate(fit)

#throw an xreg in there

## ## ## ##

TBATS_t_a <- forecast(tbats(train, biasadj = TRUE, lambda = "auto"), h = h)
TBATS_f_a <- forecast(tbats(train, biasadj = FALSE, lambda = "auto"), h = h)
TBATS_t_n <- forecast(tbats(train, biasadj = TRUE, lambda = NULL), h = h)
TBATS_f_n <- forecast(tbats(train, biasadj = FALSE, lambda = NULL), h = h)

c(TBATS_t_a = accuracy(TBATS_t_a, store_ts)["Test set","RMSE"],
  TBATS_f_a = accuracy(TBATS_f_a, store_ts)["Test set","RMSE"],
  TBATS_t_n = accuracy(TBATS_t_n, store_ts)["Test set","RMSE"],
  TBATS_f_n = accuracy(TBATS_f_n, store_ts)["Test set","RMSE"])

## ## ## ##

# combination for this new method will take lowest of each
Combination <- (ETS[["mean"]] + ARIMA[["mean"]] + STL[["mean"]] + NNAR[["mean"]] + TBATS[["mean"]])/5
