library(tidyverse)

file_list <- list.files(pattern="*.csv") # create list of all .csv files in folder

# read in each .csv file in file_list and create a data frame with the same name as the .csv file
for (i in 1:length(file_list)){
  assign(file_list[i], 
         read.csv(file_list[i]))}

CCF_SFL_Deliveries_Bogdan_Q4_2018.csv <- read.csv("CCF_SFL_Deliveries_Bogdan_Q4_2018.csv", sep = ";")
CCF_SFL_Deliveries_Bogdan_Q3_2018.csv <- read.csv("CCF_SFL_Deliveries_Bogdan_Q3_2018.csv", sep = ";")
CCF_SFL_Deliveries_Bogdan_Q1_2019.csv <- read.csv("CCF_SFL_Deliveries_Bogdan_Q1_2019.csv", sep = ";")

all <- rbind(CCF_SFL_Deliveries_Bogdan_Q1_2017.csv,
      CCF_SFL_Deliveries_Bogdan_Q2_2017.csv,
      CCF_SFL_Deliveries_Bogdan_Q3_2017.csv,
      CCF_SFL_Deliveries_Bogdan_Q4_2017.csv,
      CCF_SFL_Deliveries_Bogdan_Q1_2018.csv,
      CCF_SFL_Deliveries_Bogdan_Q2_2018.csv,
      CCF_SFL_Deliveries_Bogdan_Q3_2018.csv,
      CCF_SFL_Deliveries_Bogdan_Q4_2018.csv,
      CCF_SFL_Deliveries_Bogdan_Q1_2019.csv)

wawa <- all %>% 
  filter(PrimaryGroupDesc == "WAWA SOUTHEAST")

write.csv(wawa, "wawa.csv")
