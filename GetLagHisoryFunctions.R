
setwd("C:/SO")
library(tidyverse) 
library(data.table)
library(lubridate)
pt<- proc.time()



a <- p12 %>% select(SoldTo,ala)
##### LagDataSet function and  CustomerProductSelection function ####
# Takes the input two dataframe with columns "SoldTo","DeliveryDate",("MaterialNo"-not required),"DeliveredQty"
# returns a dataframe = p12 with 91 additional columns- Delivered quantity x days before the delivery date
LagDataSet <- function(p12,p2) {
  for(k in 1:91){
    gc()
    p12 <- p12 %>% select(SoldTo,contains("MaterialNo"),DeliveryDate,DeliveredQty)
    p12$DeliveryDate<-p12$DeliveryDate+1
    p0 <- p12 
    colnames(p0)[colnames(p0)=="DeliveredQty"] <- paste("Day_",k,sep = "") 
    p2<-suppressMessages(left_join(p2,p0))
  }
  
  for(i in 4:ncol(p2)){
    p2[,i][is.na(p2[,i])]<-0
  }
  return(p2)
}
##### Analyze what products each customer orders
### Create the function CustomerProductSelection input p12 "SoldTo","DeliveryDate","MaterialNo","DeliveredQty","Year_Month"
# Customer products are selected if a customer buys the sku at least 4 times in the 4 months
CustomerProductSelection <- function(p12,p2) {
 Prod <- p2%>%
    filter(DeliveredQty>0) %>% 
    group_by(MaterialNo)%>%
    summarise(DQ=sum(DeliveredQty)) %>% arrange(DQ)
  x <- quantile(Prod$DQ,0.50)   
 # plot(Prod$DQ[1:100])
  Prod <- Prod %>%  
    filter(DQ>x)
  Cust <- p2%>%
    filter(DeliveredQty>0) %>% 
    group_by(SoldTo)%>%
    summarise(DQ=sum(DeliveredQty)) %>% arrange(DQ)
  x <- quantile(Cust$DQ,0.30)   
  #plot(Cust$DQ[1:1000])
  Cust <- Cust %>% 
    filter(DQ>x)
  p12<- p12%>%
    filter(SoldTo %in% Cust$SoldTo) %>%
    filter(MaterialNo%in% Prod$MaterialNo)
  CustProd <- p12 %>%
    group_by(SoldTo,MaterialNo)%>%
    summarise(CountPO=n_distinct(DeliveryDate)) %>% 
    filter(CountPO>=6)%>% 
    select(SoldTo,MaterialNo) %>% 
    as.data.frame()
  CustDate <-p12%>%
    filter(DeliveredQty>0) %>% 
    group_by(SoldTo,DeliveryDate)%>%
    summarise(CountPO=n_distinct(MaterialNo)) %>% 
    select(SoldTo,DeliveryDate) %>% 
    as.data.frame()
  CustProdDate <- suppressMessages(inner_join(CustProd,CustDate))
  colSums(is.na(CustProdDate))
  p12 <-  suppressMessages(left_join(CustProdDate,p12))
  colSums(is.na(p12))
  
  p12$OrderedQty[is.na(p12$OrderedQty)] <- 0
  p12$OrderDateMax[is.na(p12$OrderDateMax)] <- as.Date(as.character("20000101"), format = '%Y%m%d')
  p12$DeliveredQty[is.na(p12$DeliveredQty)] <-0 
  return(p12)
}

# all <- fread("All_Filtered.csv")
# all$DeliveryDate<-as.Date(as.character(all$DeliveryDate), format = '%Y-%m-%d')
# all$OrderDateMax<-as.Date(as.character(all$OrderDateMax), format = '%Y%m%d')

##### Read Files####

list.files()
DelivFiles<- c("CCF_SFL_Deliveries_Bogdan_Q1_2017.csv",
               "CCF_SFL_Deliveries_Bogdan_Q2_2017.csv",
               "CCF_SFL_Deliveries_Bogdan_Q3_2017.csv",
               "CCF_SFL_Deliveries_Bogdan_Q4_2017.csv",
               "CCF_SFL_Deliveries_Bogdan_Q1_2018.csv",
               "CCF_SFL_Deliveries_Bogdan_Q2_2018.csv",
               "CCF_SFL_Deliveries_Bogdan_Q3_2018.csv")

x<-c()

for (i in 1:7) {
  xname <- paste("Q", i, sep="")
  x[i]<-xname
  assign(xname, fread(DelivFiles[i]))
}

all<-bind_rows(Q1,Q2,Q3,Q4,Q5,Q6,Q7)
rm(list=setdiff(ls(), c("all","LagDataSet","CustomerProductSelection","pt")))

all<- all%>% filter(OrderType=="ZOR" )
all<- all %>% filter(UOM!="EA")
all$DeliveryDate<-as.Date(as.character(all$DeliveryDate), format = '%Y%m%d')
prod <- fread("Products.csv") %>% 
  filter(ProductType == "Packaged Finished Drink (RTD)")
all<- all%>% filter(MaterialNo %in% prod$MaterialNo )

a <-all %>% 
  group_by(MaterialNo)%>%
  summarise(DeliveredQty=sum(DeliveredQty)) %>%
  arrange(DeliveredQty) %>% 
  as.data.frame()
all <- all%>% group_by(MaterialNo)%>%
  filter(sum(DeliveredQty)>quantile(a$DeliveredQty,.50)) %>% 
  as.data.frame()


all <-all %>% group_by(SoldTo,MaterialNo,DeliveryDate)%>%
  summarise(SalesOrderCount=n_distinct(SalesOrder),
            OrderDateMax=first(OrderDate),
            OrderedQty=sum(OrderedQty),
            DeliveredQty=sum(DeliveredQty)) %>% 
  as.data.frame()

#####Add month and year column
all$Year_Month <- paste(year(all$DeliveryDate),months.Date(all$DeliveryDate),sep="_")
YearMonthAll <- unique(all$Year_Month) 
YearMonthAll <- YearMonthAll[!(YearMonthAll%in%c("2017_January","2017_February","2017_March","2017_April"))]




#####Create a file wih 5 columns "SoldTo","DeliveryDate","MaterialNo","DeliveredQty". "Year_Month"####
# p2 the product orders for a given month
# p12 the product orders for tha given month + orders 91 days prior to that month 


p2 <- all %>% 
  select(SoldTo,MaterialNo,DeliveryDate,DeliveredQty,OrderDateMax,OrderedQty,Year_Month) %>% 
  filter(Year_Month=="2017_April") %>% 
  filter(DeliveryDate>=min(all$DeliveryDate)+91)%>% 
  select(SoldTo,MaterialNo,DeliveryDate,DeliveredQty,OrderDateMax,OrderedQty) 
  
p12<- all %>% 
  select(SoldTo,MaterialNo,DeliveryDate,DeliveredQty,OrderDateMax,OrderedQty,Year_Month) %>%  
  filter((DeliveryDate>=min(p2$DeliveryDate)-91)& (DeliveryDate<=max(p2$DeliveryDate)))%>% 
  select(SoldTo,MaterialNo,DeliveryDate,DeliveredQty,OrderDateMax,OrderedQty) 
minDate_p2 <- min(p2$DeliveryDate)

# p12<- all %>% 
#   select(SoldTo,MaterialNo,DeliveryDate,DeliveredQty)

p12 <- CustomerProductSelection(p12,p2)
p2 <- p12 %>%
  filter(DeliveryDate>minDate_p2)

o2 <- p2 %>% 
  group_by(SoldTo,DeliveryDate) %>% 
  summarise(DeliveredQty=sum(DeliveredQty)) %>% 
  as.data.frame()
o12 <- p12 %>% 
  group_by(SoldTo,DeliveryDate) %>% 
  summarise(DeliveredQty=sum(DeliveredQty)) %>% 
  as.data.frame()

lag_p <- LagDataSet(p12,p2)
lag_o <- LagDataSet(o12,o2)

proc.time()-pt


for (i in seq_along(YearMonthAll)) {
  gc()
  print(i)
  p2 <- all %>% 
    select(SoldTo,MaterialNo,DeliveryDate,DeliveredQty,OrderDateMax,OrderedQty,Year_Month) %>% 
    filter(Year_Month==YearMonthAll[i])%>% 
    select(SoldTo,MaterialNo,DeliveryDate,DeliveredQty,OrderDateMax,OrderedQty)   
  p12<- all %>% 
    select(SoldTo,MaterialNo,DeliveryDate,DeliveredQty,OrderDateMax,OrderedQty,Year_Month) %>%  
    filter((DeliveryDate>=min(p2$DeliveryDate)-91)& (DeliveryDate<=max(p2$DeliveryDate)))%>% 
    select(SoldTo,MaterialNo,DeliveryDate,DeliveredQty,OrderDateMax,OrderedQty) 
  minDate_p2 <- min(p2$DeliveryDate)
  p12 <- CustomerProductSelection(p12,p2)
  p2 <- p12 %>%
    filter(DeliveryDate>minDate_p2)
  lag_p <- bind_rows(lag_p, LagDataSet(p12,p2))
  o2 <- p2 %>% 
    group_by(SoldTo,DeliveryDate) %>% 
    summarise(DeliveredQty=sum(DeliveredQty))
  o12 <- p12 %>% 
    group_by(SoldTo,DeliveryDate) %>% 
    summarise(DeliveredQty=sum(DeliveredQty))
  lag_o <- bind_rows(lag_o, LagDataSet(o12,o2))
}
proc.time()-pt

lag_p1 <- all %>% inner_join(lag_p)

colSums(is.na(p2))


fwrite(lag_p,"All_Filtered_lag_p.csv")
a<-lag_p%>%filter(SoldTo==500113790)
fwrite(a,"OneCustTes_All_p.csv")

fwrite(lag_o,"All_Filtered_lag_o.csv")
a<-lag_o%>%filter(SoldTo==500113790)
fwrite(a,"OneCustTes_All_o.csv")

fwrite(all,"All_Filtered.csv")

rm(list=setdiff(ls(), c("LagDataSet","CustomerProductSelection","pt")))


##### Product Features #######

list.files()
gc()
filesin<-c("All_Filtered_lag_p.csv","All_Filtered_lag_o.csv","All_filtered.csv")
filesout_P<-c("All_Filtered_Ftrs_P.csv")
filesout_O<-c("All_Filtered_Ftrs_O.csv")

i<-1

gc()
lg<-fread(filesin[i])

lg$DeliveryDate<-as.Date(as.character(lg$DeliveryDate), format = '%Y-%m-%d')
lg$DeliveryDate<-as.Date(as.character(lg$DeliveryDate), format = '%Y-%m-%d')

ftrs<-lg[,1:(ncol(lg)-91)]
df<-lg[,(ncol(lg)-90):ncol(lg)]

#Features- Summary of 91 days

#Count Non Zero/Zeros/1s orders of the Product
ftrs$P_CountNZ<-as.numeric(apply(df,1,function(x){sum(x!=0)}))
ftrs$P_CountZeros<-as.numeric(apply(df,1,function(x){sum(x==0)}))
ftrs$P_Count1s<-as.numeric(apply(df,1,function(x){sum(x==1)}))

#Min product order size nonzero
f1<-function(x){
  ifelse(sum(x!=0)==0,0,min(x[x!=0]))
}

ftrs$P_MinNZ<-as.numeric(apply(df,1,f1))
#Quartile 1 product order size nonzero
f1<-function(x){
  ifelse(sum(x!=0)==0,0,quantile(x[x!=0],0.25))
}
ftrs$P_Q1NZ<-as.numeric(apply(df,1,f1))
#mean product order size nonzero
f1<-function(x){
  ifelse(sum(x!=0)==0,0,mean(x[x!=0]))
}
ftrs$P_MeanNZ<-as.numeric(apply(df,1,f1))
#sd product order size nonzero
f1<-function(x){
  ifelse(sum(x!=0)==0,0,sd(x[x!=0]))
}
ftrs$P_SdNZ<-as.numeric(apply(df,1,f1))

#median product order size nonzero
f1<-function(x){
  ifelse(sum(x!=0)==0,0,median(x[x!=0]))
}
ftrs$P_medianNZ<-as.numeric(apply(df,1,f1))

#Quartile 3 product order size nonzero
f1<-function(x){
  ifelse(sum(x!=0)==0,0,quantile(x[x!=0],0.75))
}
gc()
ftrs$P_Q3NZ<-as.numeric(apply(df,1,f1))
#MAX product order size nonzero
f1<-function(x){
  ifelse(sum(x!=0)==0,0,max(x[x!=0]))
}
ftrs$P_MaxNZ<-as.numeric(apply(df,1,f1))

#Mean product order size 
ftrs$P_Mean<-as.numeric(apply(df,1,mean))

#Q3 product order size 
f1<-function(x){
  quantile(x,.75)
}
ftrs$P_Q3<-as.numeric(apply(df,1,f1))


#Mean per day 
ftrs$P_MeanDay0<-as.numeric(apply(df,1,function(x){mean(x[1:91%%7==0])}))
ftrs$P_MeanDay1<-as.numeric(apply(df,1,function(x){mean(x[1:91%%7==1])}))
ftrs$P_MeanDay2<-as.numeric(apply(df,1,function(x){mean(x[1:91%%7==2])}))
ftrs$P_MeanDay3<-as.numeric(apply(df,1,function(x){mean(x[1:91%%7==3])}))
ftrs$P_MeanDay4<-as.numeric(apply(df,1,function(x){mean(x[1:91%%7==4])}))
ftrs$P_MeanDay5<-as.numeric(apply(df,1,function(x){mean(x[1:91%%7==5])}))
ftrs$P_MeanDay6<-as.numeric(apply(df,1,function(x){mean(x[1:91%%7==6])}))

# #mean week 0,1,2
# ftrs$P_MeanWeek0<-as.numeric(apply(df,1,function(x){mean(x[0:90%/%7==0])}))
# ftrs$P_MeanWeek1<-as.numeric(apply(df,1,function(x){mean(x[0:90%/%7==1])}))
# ftrs$P_MeanWeek2<-as.numeric(apply(df,1,function(x){mean(x[0:90%/%7==2])}))
# ftrs$P_MeanWeek3<-as.numeric(apply(df,1,function(x){mean(x[0:90%/%7==3])}))
# ftrs$P_MeanWeek4<-as.numeric(apply(df,1,function(x){mean(x[0:90%/%7==4])}))
# ftrs$P_MeanWeek5<-as.numeric(apply(df,1,function(x){mean(x[0:90%/%7==5])}))
# ftrs$P_MeanWeek6<-as.numeric(apply(df,1,function(x){mean(x[0:90%/%7==6])}))

ftrs$P_MeanWeek0_6days<-as.numeric(apply(df,1,function(x){mean(x[1:91%/%7==0])}))
ftrs$P_MeanWeek1_7days<-as.numeric(apply(df,1,function(x){mean(x[1:91%/%7==1])}))
# ftrs$P_MeanWeek2_7days<-as.numeric(apply(df,1,function(x){mean(x[1:91%/%7==2])}))
# ftrs$P_MeanWeek3_7days<-as.numeric(apply(df,1,function(x){mean(x[1:91%/%7==3])}))
# ftrs$P_MeanWeek4_7days<-as.numeric(apply(df,1,function(x){mean(x[1:91%/%7==4])}))
# ftrs$P_MeanWeek5_7days<-as.numeric(apply(df,1,function(x){mean(x[1:91%/%7==5])}))
# ftrs$P_MeanWeek6_7days<-as.numeric(apply(df,1,function(x){mean(x[1:91%/%7==6])}))

ftrs$P_Day7<-df$Day_7


fwrite(ftrs,filesout_P[1])





##### ORDERS features ##############


lg<-fread(filesin[2])
ftrs<-lg[,1:(ncol(lg)-91)]
df<-lg[,(ncol(lg)-90):ncol(lg)]
a<-as.data.frame(colnames(ftrs))

#Features- Summary of 91 days
#Count Non Zero orders of the Product
ftrs$O_CountNZ<-as.numeric(apply(df,1,function(x){sum(x!=0)}))
#Min product order size nonzero
f1<-function(x){
  ifelse(sum(x!=0)==0,0,min(x[x!=0]))
}
ftrs$O_MinNZ<-as.numeric(apply(df,1,f1))

#Quartile 1 product order size nonzero
f1<-function(x){
  ifelse(sum(x!=0)==0,0,quantile(x[x!=0],0.25))
}
ftrs$O_Q1NZ<-as.numeric(apply(df,1,f1))
#mean product order size nonzero
f1<-function(x){
  ifelse(sum(x!=0)==0,0,mean(x[x!=0]))
}
ftrs$O_MeanNZ<-as.numeric(apply(df,1,f1))
#sd product order size nonzero
f1<-function(x){
  ifelse(sum(x!=0)==0,0,sd(x[x!=0]))
}
ftrs$O_SdNZ<-as.numeric(apply(df,1,f1))
#median product order size nonzero
f1<-function(x){
  ifelse(sum(x!=0)==0,0,median(x[x!=0]))
}
ftrs$O_medianNZ<-as.numeric(apply(df,1,f1))
#Quartile 3 product order size nonzero
f1<-function(x){
  ifelse(sum(x!=0)==0,0,quantile(x[x!=0],0.75))
}
ftrs$O_Q3NZ<-as.numeric(apply(df,1,f1))
#MAX product order size nonzero
f1<-function(x){
  ifelse(sum(x!=0)==0,0,max(x[x!=0]))
}
ftrs$O_MaxNZ<-as.numeric(apply(df,1,f1))
#Med product order size 
ftrs$O_Median<-as.numeric(apply(df,1,median))
#Mean product order size 
ftrs$O_Mean<-as.numeric(apply(df,1,mean))
#SD product order size 
ftrs$O_Sd<-as.numeric(apply(df,1,sd))
#Q3 product order size 
f1<-function(x){
  quantile(x,.75)
}
ftrs$O_Q3<-as.numeric(apply(df,1,f1))
#Max product order size 
ftrs$O_Max<-as.numeric(apply(df,1,max))
#Mean per day 
ftrs$O_MeanDayO<-as.numeric(apply(df,1,function(x){mean(x[1:91%%7==0])}))
ftrs$O_MeanDay1<-as.numeric(apply(df,1,function(x){mean(x[1:91%%7==1])}))
ftrs$O_MeanDay2<-as.numeric(apply(df,1,function(x){mean(x[1:91%%7==2])}))
ftrs$O_MeanDay3<-as.numeric(apply(df,1,function(x){mean(x[1:91%%7==3])}))
ftrs$O_MeanDay4<-as.numeric(apply(df,1,function(x){mean(x[1:91%%7==4])}))
ftrs$O_MeanDay5<-as.numeric(apply(df,1,function(x){mean(x[1:91%%7==5])}))
ftrs$O_MeanDay6<-as.numeric(apply(df,1,function(x){mean(x[1:91%%7==6])}))
#mean wek 0,1,2
ftrs$O_MeanWeek0<-as.numeric(apply(df,1,function(x){mean(x[0:90%/%7==0])}))
ftrs$O_MeanWeek1<-as.numeric(apply(df,1,function(x){mean(x[0:90%/%7==1])}))
ftrs$O_MeanWeek2<-as.numeric(apply(df,1,function(x){mean(x[0:90%/%7==2])}))
ftrs$O_MeanWeek3<-as.numeric(apply(df,1,function(x){mean(x[0:90%/%7==3])}))
ftrs$O_MeanWeek4<-as.numeric(apply(df,1,function(x){mean(x[0:90%/%7==4])}))
ftrs$O_MeanWeek5<-as.numeric(apply(df,1,function(x){mean(x[0:90%/%7==5])}))
ftrs$O_MeanWeek6<-as.numeric(apply(df,1,function(x){mean(x[0:90%/%7==6])}))

ftrs$O_MeanWeek0_6days<-as.numeric(apply(df,1,function(x){mean(x[1:91%/%7==0])}))
ftrs$O_MeanWeek1_7days<-as.numeric(apply(df,1,function(x){mean(x[1:91%/%7==1])}))
ftrs$O_MeanWeek2_7days<-as.numeric(apply(df,1,function(x){mean(x[1:91%/%7==2])}))
ftrs$O_MeanWeek3_7days<-as.numeric(apply(df,1,function(x){mean(x[1:91%/%7==3])}))
ftrs$O_MeanWeek4_7days<-as.numeric(apply(df,1,function(x){mean(x[1:91%/%7==4])}))
ftrs$O_MeanWeek5_7days<-as.numeric(apply(df,1,function(x){mean(x[1:91%/%7==5])}))
ftrs$O_MeanWeek6_7days<-as.numeric(apply(df,1,function(x){mean(x[1:91%/%7==6])}))


ftrs$O_Day7<-df$Day_7

#colnames(ftrs)[colnames(ftrs)=="O_MeanWeek3_6days"]<-c("O_MeanWeek3_7days")

fwrite(ftrs,filesout_O[i])



