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

source("functions/StorePlotFunction.R")
source("functions/ImputationFunction.R")

# import conventional retail and supermerkets
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

# create week, month, year column and starting day of week column
super <- super %>%
  mutate(year  = year(Date) , 
         month = month(Date),
         week  = week(Date) ,
         day   = day(Date))

# remove dec 2016
super <- super %>% 
  filter(year != 2016)

# do only stores with more than 1 order
super <- super %>% 
  group_by(Customer) %>% 
  filter(n() > 1)

# plot stores
StorePlot(600785148)
ImputeMissingDates(600785148)
StorePlot(600805048)
ImputeMissingDates(600805048)
StorePlot(500117825)
ImputeMissingDates(500117825)


































# view monthly sales  
# aggregate by month
ccbf_sales_monthly <- super %>%
  mutate(month = month(Date, label = TRUE),
         year  = year(Date)) %>%
  group_by(year, month) %>%
  summarise(total.qty = sum(Volume)) 
ccbf_sales_monthly

# Visualize sales by month by year
ccbf_sales_monthly %>%
  filter(year != 2019) %>% 
  ggplot(aes(x = month, y = total.qty, group = year)) +
  geom_area(aes(fill = year), position = "stack") +
  labs(title = "Quantity Sold: Month Plot", x = "", y = "Sales",
       subtitle = "Peaks at March, June, August, September") +
  scale_y_continuous() +
  theme_tq()

# We’ll create a new “order.month” date using zoo::as.yearmon() that captures the year and month information from the “order.date” and then passing this to lubridate::as_date() to convert to date format.

monthly_qty_by_str <- super %>%
  mutate(order.month = as_date(as.yearmon(Date))) %>%
  group_by(Customer, order.month) %>%
  summarise(total.qty = sum(Volume))

monthly_qty_by_str

# cut out stores with only 1 month
monthly_qty_by_str <- monthly_qty_by_str %>% 
  group_by(Customer) %>% 
  filter(n() > 1)

#  all the stores that were removed, were there only deliveries at the start of the time series (that is, went out after that) or just at the end (that is, just opened)
bad_store_list <- monthly_qty_by_str %>% 
  group_by(Customer) %>% 
  filter(n() == 1)
#  not really

# nest by store
monthly_qty_by_str_nest <- monthly_qty_by_str %>%
  group_by(Customer) %>%
  nest(.key = "data.tbl")

monthly_qty_by_str_nest

#  Step 1: Coerce to a ts object class.

monthly_qty_by_str_ts <- monthly_qty_by_str_nest %>%
  mutate(data.ts = map(.x       = data.tbl, 
                       .f       = tk_ts, 
                       select   = -order.month, 
                       start    = 2017,
                       freq     = 12))
monthly_qty_by_str_ts

#  Step 2: Apply a model (or set of models)
# attempt this or the auto.arima vs ets function from forecasting.R

# This takes 70 minutes for 11237 customers
monthly_qty_by_str_fit <- monthly_qty_by_str_ts %>%
  mutate(fit.ets = map(data.ts, ets))

monthly_qty_by_str_fit

# To get the model parameters for each nested list, we can combine sw_tidy within the mutate and map combo. The only real difference is now we unnest the generated column (named “tidy”). Last, because it’s easier to compare the model parameters side by side, we add one additional call to spread() from the tidyr package.

monthly_qty_by_str_fit %>%
  mutate(tidy = map(fit.ets, sw_tidy)) %>%
  unnest(tidy, .drop = TRUE) %>%
  spread(key = Customer, value = estimate)

# We can view the model accuracies also by mapping sw_glance within the mutate and map combo.

monthly_qty_by_str_fit %>%
  mutate(glance = map(fit.ets, sw_glance)) %>%
  unnest(glance, .drop = TRUE)

augment_fit_ets <- monthly_qty_by_str_fit %>%
  mutate(augment = map(fit.ets, sw_augment, timetk_idx = TRUE, rename_index = "date")) %>%
  unnest(augment, .drop = TRUE)

augment_fit_ets

augment_fit_ets %>%
  ggplot(aes(x = date, y = .resid, group = Customer)) +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_line(color = palette_light()[[2]]) +
  geom_smooth(method = "loess") +
  labs(title = "Bike Quantity Sold By Secondary Category",
       subtitle = "ETS Model Residuals", x = "") + 
  theme_tq() +
  facet_wrap(~ Customer, scale = "free_y", ncol = 3) +
  scale_x_date(date_labels = "%Y")

monthly_qty_by_str_fit %>%
  mutate(decomp = map(fit.ets, sw_tidy_decomp, timetk_idx = TRUE, rename_index = "date")) %>%
  unnest(decomp)

#  Step 3: Forecast the models (similar to predict)

monthly_qty_by_str_fcast <- monthly_qty_by_str_fit %>%
  mutate(fcast.ets = map(fit.ets, forecast, h = 12))
monthly_qty_by_str_fcast

# Step 4: Tidy the forecast

monthly_qty_by_str_fcast_tidy <- monthly_qty_by_str_fcast %>%
  mutate(sweep = map(fcast.ets, sw_sweep, fitted = FALSE, timetk_idx = TRUE)) %>%
  unnest(sweep)
monthly_qty_by_cat2_fcast_tidy

monthly_qty_by_cat2_fcast_tidy %>%
  ggplot(aes(x = index, y = total.qty, color = key, group = category.secondary)) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), 
              fill = "# D5DBFF", color = NA, size = 0) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), 
              fill = "# 596DD5", color = NA, size = 0, alpha = 0.8) +
  geom_line() +
  labs(title = "Bike Quantity Sold By Secondary Category",
       subtitle = "ETS Model Forecasts",
       x = "", y = "Units") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_tq() +
  scale_fill_tq() +
  facet_wrap(~ category.secondary, scales = "free_y", ncol = 3) +
  theme_tq() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))