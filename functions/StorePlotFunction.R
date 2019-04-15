library(gtable)
library(grid)

StorePlot <- function(ID) {
  theplot <- super %>% 
    filter(id == ID) %>% 
    ggplot(aes(Date, Volume)) + 
    geom_line()
  
  ggplotly(theplot)
}

#sample ID for testing
600785148

StorePlot(600785148)


daily <- ggplotly(super %>%
                   filter(id == 600785148) %>% 
                   ggplot(aes(Date, Volume)) + 
                   geom_line())

daily 

temp <- super %>%
  filter(id == 600785148) %>% 
  mutate(month = month(Date, label = TRUE),
         week  = week(Date),
         year = year(Date)) %>%
  group_by(week, month, year) %>%
  summarise(total.qty = sum(Volume)) %>%
  rename(saleyear = year)
  
date_base <- as.numeric(plyr::revalue(as.character(temp$saleyear), c("2017" = "100", "2018" = "153", "2019" = "205")))

temp$order <- date_base

test <- temp %>%
  mutate(order = order + week)
  
weekly <- ggplotly(test %>%
                    ggplot(aes(order, total.qty)) + 
                    geom_line())

weekly

subplot(daily, weekly, nrows = 2, margin = 0.04, heights = c(0.6, 0.4))
