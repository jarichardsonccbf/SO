library(tidyverse)
library(data.table)

SO <- read.csv("data/testdata.csv", stringsAsFactors = FALSE)

SO.FTP <- SO %>% 
  filter(DeliveryDate == "4/10/2019 0:00") %>% 
  mutate(SaTy = "ZOR", 
         SOrg. = 4500,
         DChl = "Z1",
         Dv = "Z0",
         'Purchase order no.' = NA,
         'Your Reference' = paste(
           str_sub(SoldTo, end = 1), 
           str_sub(SoldTo, 4), 
           (as.Date(DeliveryDate, "%m/%d/%Y %H:%M") - as.Date("2016/01/01", format = "%Y/%m/%d")),
           sep = ""),
         SU = "CS",
         RRC = NA,
         POtyp = "PRDT",
         DIBI = "ZP",
         'Doc. Date' = NA,
         Itinerary = "Testing Bogdan prediction",
         Item = rowid(SoldTo) * 10,
         Req.dlv.dt = gsub("-","",as.Date(DeliveryDate, "%m/%d/%Y %H:%M")),) %>% 
  rename('Sold-To Pt' = SoldTo,
         Material = MaterialNo,
         'Order Quantity'  = PredictedO)

SO.FTP <- SO.FTP %>% 
  select(SaTy, 
         SOrg., 
         DChl, 
         Dv, 
         `Purchase order no.`, 
         `Your Reference`, 
         `Sold-To Pt`,
         Req.dlv.dt,
         Item,
         Material,
         `Order Quantity`,
         SU,
         RRC,
         POtyp,
         DIBI,
         `Doc. Date`,
         Itinerary)

write.csv(SO.FTP,"outputs/SO.FTP.csv", row.names = FALSE)
