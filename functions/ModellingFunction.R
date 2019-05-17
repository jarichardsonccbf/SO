ave_orders <- wawa %>% 
  group_by(item) %>% 
  summarise(ave = mean(OrderedQty)) %>% 
  arrange(desc(ave))

ave_orders  # test on products 115583, 133129
            # store 500124262, 600379764

a <- wawa %>% 
  filter(outlet == 600379764) %>% 
  group_by(item)

wawa_fast_inconsistent <- OrderGaps(wawa, 600379764, 133129)
wawa_fast_inconsistent$plot.compare

store_ts <- ts(wawa_fast_inconsistent$values.compare$OrderedQtySpread, start = 2017, freq = 365)

# full selection for each alternates----

# bias adjust FALSE uses medians, TRUE uses means
# lambda set as auto transforms data using BoxCox.lambda

# Forecast combinations----
train <- window(store_ts, end=c(2019, 1))
test <- length(store_ts) - length(train)

# ETS <- forecast(ets(train), h = h)  # base model
ETS_base    <- forecast(ets(train), biasadj = FALSE, h = test)
ETS_biasadj <- forecast(ets(train), biasadj = TRUE , h = test)

accuracy(ETS_base   , store_ts)
accuracy(ETS_biasadj, store_ts)

ETS <- ETS_base

## ## ## ##
ARIMA_T_a <- forecast(auto.arima(train, lambda = "auto", biasadj = TRUE) , h = test)
ARIMA_F_a <- forecast(auto.arima(train, lambda = "auto", biasadj = FALSE), h = test)
ARIMA_T_n <- forecast(auto.arima(train, lambda = NULL  , biasadj = TRUE) , h = test)
ARIMA_F_n <- forecast(auto.arima(train, lambda = NULL  , biasadj = FALSE), h = test)

ARIMA_T_a_f <- forecast(auto.arima(train, xreg=fourier(train, K = 30), lambda = "auto", biasadj = TRUE) ,
                        h = test, xreg = fourier(train, K = 30, h = test))
ARIMA_F_a_f <- forecast(auto.arima(train, xreg=fourier(train, K = 30), lambda = "auto", biasadj = FALSE),
                        h = test, xreg = fourier(train, K = 30, h = test))
ARIMA_T_n_f <- forecast(auto.arima(train, xreg=fourier(train, K = 30), lambda = NULL  , biasadj = TRUE) ,                         h = test, xreg = fourier(train, K = 30, h = test))
ARIMA_F_n_f <- forecast(auto.arima(train, xreg=fourier(train, K = 30), lambda = NULL  , biasadj = FALSE),                         h = test, xreg = fourier(train, K = 30, h = test))

c(accuracy(ARIMA_T_a, store_ts)["Test set","MAE"],
  accuracy(ARIMA_F_a, store_ts)["Test set","MAE"],
  accuracy(ARIMA_T_n, store_ts)["Test set","MAE"],
  accuracy(ARIMA_F_n, store_ts)["Test set","MAE"],
  accuracy(ARIMA_T_a_f, store_ts)["Test set","MAE"],
  accuracy(ARIMA_F_a_f, store_ts)["Test set","MAE"],
  accuracy(ARIMA_T_n_f, store_ts)["Test set","MAE"],
  accuracy(ARIMA_F_n_f, store_ts)["Test set","MAE"])

ARIMA <- ARIMA_F_n_f

## ## ## ##

STL_T_a <- stlf(train, h = test, lambda = "auto", biasadj = TRUE)
STL_F_a <- stlf(train, h = test, lambda = "auto", biasadj = FALSE)
STL_T_n <- stlf(train, h = test, lambda = NULL, biasadj = TRUE)
STL_F_n <- stlf(train, h = test, lambda = NULL, biasadj = FALSE)

c(STL_T_a = accuracy(STL_T_a, store_ts)["Test set","RMSE"],
  STL_F_a = accuracy(STL_F_a, store_ts)["Test set","RMSE"],
  STL_T_n = accuracy(STL_T_n, store_ts)["Test set","RMSE"],
  STL_F_n = accuracy(STL_F_n, store_ts)["Test set","RMSE"])

STL <- STL_F_a

## ## ## ##

NNAR_a <- forecast(nnetar(train), lambda = "auto", h = test)
NNAR_n <- forecast(nnetar(train), lambda = NULL  , h = test)

c(accuracy(NNAR_a, store_ts)["Test set","RMSE"],
  accuracy(NNAR_n, store_ts)["Test set","RMSE"])

NNAR <- NNAR_n

## ## ## ##

TBATS_t_a <- forecast(tbats(train, biasadj = TRUE , lambda = "auto"), h = test)
TBATS_f_a <- forecast(tbats(train, biasadj = FALSE, lambda = "auto"), h = test)
TBATS_t_n <- forecast(tbats(train, biasadj = TRUE , lambda = NULL)  , h = test)
TBATS_f_n <- forecast(tbats(train, biasadj = FALSE, lambda = NULL)  , h = test)

c(TBATS_t_a = accuracy(TBATS_t_a, store_ts)["Test set","RMSE"],
  TBATS_f_a = accuracy(TBATS_f_a, store_ts)["Test set","RMSE"],
  TBATS_t_n = accuracy(TBATS_t_n, store_ts)["Test set","RMSE"],
  TBATS_f_n = accuracy(TBATS_f_n, store_ts)["Test set","RMSE"])

TBATS <- TBATS_f_a

## ## ## ##

# combination for this new method will take lowest of each
Combination <- (ETS[["mean"]] + ARIMA[["mean"]] + STL[["mean"]] + NNAR[["mean"]] + TBATS[["mean"]])/5

autoplot(store_ts) +
  autolayer(ETS, series="ETS", PI=FALSE) +
  autolayer(ARIMA, series="ARIMA", PI=FALSE) +
  autolayer(STL, series="STL", PI=FALSE) +
  autolayer(NNAR, series="NNAR", PI=FALSE) +
  autolayer(TBATS, series="TBATS", PI=FALSE) +
  autolayer(Combination, series="Combination") +
  xlab("Time") + ylab("Volume")


accuracy(ETS        , store_ts)
accuracy(ARIMA      , store_ts)
accuracy(STL        , store_ts)
accuracy(NNAR       , store_ts)
accuracy(TBATS      , store_ts)
accuracy(Combination, store_ts)

