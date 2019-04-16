library(imputeTS)

ImputeMissingDates <- function(ID) {

actual_points <- ggplotly(  
  super %>% 
  filter(id == ID) %>% 
  ggplot(aes(Date, Volume)) + 
  geom_line()  # ts line of actual orders
)

full_calendar <- as.data.frame(seq(as.Date("2017/1/1"), as.Date("2019/3/16"), "days"))
full_calendar$Date <- full_calendar$`seq(as.Date("2017/1/1"), as.Date("2019/3/16"), "days")`  #create vector of full dates for 2 years

store <- super %>% 
  filter(id == ID)  #filter store

full_calendar <- full_calendar %>% 
  left_join(store, "Date") %>% 
  select(-c(`seq(as.Date("2017/1/1"), as.Date("2019/3/16"), "days")`))  #filter store

full_calendar <- full_calendar %>%
  mutate(year  = year(Date) , 
         month = month(Date),
         week  = week(Date) ,
         day   = day(Date)) #make columns for year, month, week, day

full_calendar$Customer <- levels(as.factor(full_calendar$Customer))[1] 
full_calendar$id       <- levels(as.factor(full_calendar$id))[1]  #make columns for year, month, week, day

full_calendar$Volume <- na.interpolation(full_calendar$Volume)  #impute missing values

full_daily_deliveries <- ggplotly(
  full_calendar %>% 
    ggplot(aes(Date, Volume)) + 
    geom_line()
)  #plot the imputation

subplot(actual_deliveries, full_daily_deliveries, nrows = 2)  #plot actual and imputed  top to bottom
}

ImputeMissingDates(600785148)



#ts line of actual orders
actual_deliveries <- ggplotly(
  super %>% 
    filter(id == 600785148) %>% 
    ggplot(aes(Date, Volume)) + 
    geom_line()
)

#create vector of full dates for 2 years
full_calendar <- as.data.frame(seq(as.Date("2017/1/1"), as.Date("2019/3/16"), "days"))
full_calendar$Date <- full_calendar$`seq(as.Date("2017/1/1"), as.Date("2019/3/16"), "days")` 

#filter store
store <- super %>% 
  filter(id == 600785148)

#add existing deliveries into a full calendar
full_calendar <- full_calendar %>% 
  left_join(store, "Date") %>% 
  select(-c(`seq(as.Date("2017/1/1"), as.Date("2019/3/16"), "days")`))

#make columns for year, month, week, day
full_calendar <- full_calendar %>%
  mutate(year  = year(Date) , 
         month = month(Date),
         week  = week(Date) ,
         day   = day(Date))

#add in name and id of store to full df
full_calendar$Customer <- levels(as.factor(full_calendar$Customer))[1] 
full_calendar$id       <- levels(as.factor(full_calendar$id))[1] 

full_calendar$Volume <- na.interpolation(full_calendar$Volume)

full_daily_deliveries <- ggplotly(
  full_calendar %>% 
    ggplot(aes(Date, Volume)) + 
    geom_line()
)

subplot(actual_deliveries, full_daily_deliveries, nrows = 2)