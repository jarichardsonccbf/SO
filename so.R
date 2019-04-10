library(tidyverse)
library(data.table)
library(stringr)

super <- fread("data/CR.supermarket.supprette.bc.txt")

colnames(super) <- c("Date","Customer","id","Volume","DNR")
super$DNR <- parse_number(super$DNR)
super$id <- as.character(parse_integer(super$id))
super$Date <- as.Date(super$Date, format = "%Y-%m-%d")

#test if df has "extra" data inside of it, if NA at top, then it does.
count(super,id, Date) %>% count(id)

#NAs are "totals"
colSums(is.na(super))

super <- super[!is.na(super$id),]

colSums(is.na(super))

#remove DNR
super <- super %>%
  select(-c(DNR))

#create week column and year column and starting day of week column
super <- super %>%
  mutate(year = year(Date), 
         month = month(Date), 
         day = lubridate::day(Date))

