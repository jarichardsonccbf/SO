library(imputeTS)

ImputeMissingDates <- function(ID) {

full_calendar <- as.data.frame(seq(as.Date("2017/1/1"), as.Date("2019/3/16"), "days"))
full_calendar$Date <- full_calendar$`seq(as.Date("2017/1/1"), as.Date("2019/3/16"), "days")`  # create vector of full dates for 2 years

store <- super %>% 
  filter(id == ID)  # filter store

full_calendar <- full_calendar %>% 
  left_join(store, "Date") %>% 
  select(-c(`seq(as.Date("2017/1/1"), as.Date("2019/3/16"), "days")`))  # filter store

full_calendar$Volume <- na.interpolation(full_calendar$Volume)  # impute missing values

return(full_calendar)
}
