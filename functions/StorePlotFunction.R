StorePlot <- function(ID) {
  theplot <- super %>% 
    filter(id == ID) %>% 
    ggplot(aes(Date, Volume)) + 
    geom_line()
  
  ggplotly(theplot)
}