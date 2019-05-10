library(plotly)

OrderGaps <- function(df, store, prod) {
  
  store.prod <- df %>% 
    filter(outlet == store) %>% 
    filter(item == prod)
  
  actual <- ggplotly(store.prod %>% 
                       ggplot(aes(OrderDate, OrderedQty)) + 
                       geom_line())
  
  full_calendar <- as.data.frame(seq(as.Date("2017/1/1"), as.Date("2019/3/29"), "days"))
  full_calendar$OrderDate <- full_calendar$`seq(as.Date("2017/1/1"), as.Date("2019/3/29"), "days")`  # create vector of full dates for 2+ years
  
  full_calendar <- full_calendar %>% 
    select(-c(`seq(as.Date("2017/1/1"), as.Date("2019/3/29"), "days")`))
  full_calendar <- full_calendar %>% 
    left_join(store.prod, "OrderDate")
  
  full_calendar$OrderedQtySpread <- full_calendar$OrderedQty
  full_calendar$OrderedQtyZeroes <- full_calendar$OrderedQty
  
  full_calendar$OrderedQtySpread[is.na(full_calendar$OrderedQtySpread)] <- 0 
  full_calendar$OrderedQtySpread <- ave(full_calendar$OrderedQtySpread,cumsum(full_calendar$OrderedQtySpread)) 
  
  full_calendar$OrderedQtyZeroes[is.na(full_calendar$OrderedQtyZeroes)] <- 0
  
  full_calendar <- full_calendar %>% 
    select(-c(DeliveryDate, outlet, item))
  
  filled <- ggplotly(full_calendar %>%
                       ggplot(aes(OrderDate, OrderedQtySpread)) +
                       geom_line())
  
  withzeroes <- ggplotly(full_calendar %>%
                           ggplot(aes(OrderDate, OrderedQtyZeroes)) +
                           geom_line())
  
  a <- subplot(actual, withzeroes, filled, nrows = 3)  
  
  return(list(plot.compare = a, values.compare = full_calendar))
}