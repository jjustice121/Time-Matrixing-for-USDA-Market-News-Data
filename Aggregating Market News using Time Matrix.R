#Aggregating Biweekly Market News Data using Time Matrix
#references: see Time Matrixing Script.R to get the Time Matrix 

#Import Libraries
library(lubridate)
library(tidyverse)
library(USDAMARS)

#set working directory
setwd("C:/Users/jessie.justice/OneDrive - USDA/Desktop/Data Analysis Projects/EAB Data Management/Test Scripts and Programs/Test Outputs")

#Set API Key
MARS_API_Key("d8qcrgndThTeo/iTpu5AJF8rqbe+AT7mImzqT4gWe2U=")

#write function to convert $/MT to $/LB
Conversion_to_Dollars_Per_Lb <- function(i){
  
  DollarsPerLb <- i * 0.00045359237
}

#write function to iterate over all International DMN Reports
Aggregate_Biweekly_to_Monthly <- function(slug_id){
  
  #Get Table
  df <- MARS_Table_Pull(slug_id)
  
  #import time matrix
  time_matrix <- read.xlsx(paste0(getwd(),"/International_Market_News_Time_Matrix.xlsx"), sheet = df$report_title[1])

  df <- df %>% select(c("report_begin_date","report_title","price_Unit","price_min","price_max")) %>% mutate(price_midpoint = (price_min + price_max)/2)

  #add unique ID
  df$ID <- as.numeric(factor(df$report_begin_date, levels = unique(df$report_begin_date)))
  
  #drop begin date for merge
  df <- df %>% select(-c("report_begin_date"))
  
  #Merge Time Matrix and Data
  df_Matrix <- df %>% left_join(time_matrix, by =c("ID"))

  #list of columns to transform
  Months <- c("January","February","March","April","May","June","July","August","September","October","November","December")

  #get rowsums
  df_Matrix$Days_Total <- rowSums(df_Matrix[,Months], na.rm = TRUE)

  #get total business days by month-year combination
  df_Matrix <- df_Matrix %>% group_by(`Month-Year`) %>% mutate(Days_Total_Month = sum(Days_Total)) %>% ungroup()

  #multiply price columns by Days_total
  df_Matrix <- df_Matrix %>% mutate(price_min_prod_days = price_min * Days_Total, price_max_prod_days = price_max * Days_Total, price_midpoint_prod_days = price_midpoint * Days_Total)

  #get monthly aggregate products
  df_Matrix <- df_Matrix %>% group_by(`Month-Year`) %>% mutate(price_min_prod_days_agg = sum(price_min_prod_days), price_max_prod_days_agg = sum(price_max_prod_days), price_midpoint_prod_days_agg = sum(price_midpoint_prod_days)) %>% ungroup()

  #divide by total number of business days in month
  df_Matrix <- df_Matrix %>% mutate(price_min_monthly = price_min_prod_days_agg/Days_Total_Month, price_max_monthly = price_max_prod_days_agg/Days_Total_Month, price_midpoint_monthly = price_midpoint_prod_days_agg/Days_Total_Month)

  #convert to dollar per lb

  #function to convert from $/mt to $/lb
  df_Matrix <- df_Matrix %>% mutate_at(c("price_min_monthly","price_max_monthly","price_midpoint_monthly"),Conversion_to_Dollars_Per_Lb)

  #drop columns we don't need
  df_Matrix <- df_Matrix %>% select(-c(price_Unit,price_min_prod_days,price_max_prod_days,price_midpoint_prod_days,price_min_prod_days_agg,price_max_prod_days_agg,price_midpoint_prod_days_agg))

  #aggregate dataframe to get a single value for each month-year combination
  df_Matrix <- df_Matrix %>% select(-c(Months,Days_Total, price_min,price_max,price_midpoint,ID,report_begin_date)) %>% distinct()

  assign(df$report_title[1],df_Matrix, envir = .GlobalEnv)
  
  
}


#test function
#Aggregate_Biweekly_to_Monthly(1034)

#European Whey, Oceania SMP, Oceania Cheese, Oceania Butter
Report_List <- c(1034,1038,1082,1099)

#Iterate Over List
lapply(Report_List,Aggregate_Biweekly_to_Monthly)

#export to excel file


#create list of sheets for Excel File
sheets <- list("Butter - Oceania" = `Butter - Oceania`, "Cheese - Oceania" = `Cheese - Oceania`,"Skim Milk Powder - Oceania" = `Skim Milk Powder - Oceania`,"Dry Whey - Europe" = `Dry Whey - Europe`)

#Write Sheets to Excel File
write.xlsx(sheets,"C:/Users/jessie.justice/OneDrive - USDA/Desktop/Data Analysis Projects/EAB Data Management/Test Scripts and Programs/Test Outputs/International_Monthly_Market_News_Data.xlsx")
