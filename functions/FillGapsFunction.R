library(plotly)

OrderGaps <- function(ID) {
  
  store <- super %>% 
    filter(id == ID)  # filter store
  
  actual <- ggplotly(store %>% 
                       ggplot(aes(Date, Volume)) + 
                       geom_line())
  
  full_calendar <- as.data.frame(seq(as.Date("2017/1/1"), as.Date("2019/3/16"), "days"))
  full_calendar$Date <- full_calendar$`seq(as.Date("2017/1/1"), as.Date("2019/3/16"), "days")`  # create vector of full dates for 2 years
  
  full_calendar <- full_calendar %>% 
    left_join(store, "Date") %>% 
    select(-c(`seq(as.Date("2017/1/1"), as.Date("2019/3/16"), "days")`))  # filter store
  
  full_calendar$Volume[is.na(full_calendar$Volume)] <- 0 
  full_calendar_filled <- full_calendar
  full_calendar_filled$Volume <- ave(full_calendar$Volume,cumsum(full_calendar$Volume)) 
  
  filled <- ggplotly(full_calendar_filled %>%
                       ggplot(aes(Date, Volume)) +
                       geom_line())
  
  a <- subplot(actual, filled, nrows = 2)  
  
  b <- cbind(full_calendar, full_calendar_filled$Volume)
  b <- b %>% 
    select(-c(Customer, id))
  
  return(list(plot.compare = a, values.compare = b))
  }