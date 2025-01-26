#Aggregating Biweekly Market News Data using Time Matrix
#references: see Time Matrixing Script.R to get the Time Matrix 

#Import Libraries
library(lubridate)
library(tidyverse)
library(USDAMARS)

#Set API Key
MARS_API_Key("7XWk8PBs9+OLcbfPji2p2iUhoyoONlYP")

#Import Time Matrix
Time_Matrix <- read.csv("C:/Users/djset/OneDrive/Desktop/Independent Projects/Time Matrixing and Aggregating Market News Data from Biweekly to Monthly/Data Out/International_Time_Matrix_Market_News.csv")

Dir <- MARS_Table_Directory()
#Get Table
Butter <- MARS_Table_Pull(1099)

Butter <- Butter %>% select(c("report_begin_date","report_end_date","report_title","price_Unit","price_min","price_max"))

#add unique ID
Butter$ID <- as.numeric(factor(Butter$report_begin_date, levels = unique(Butter$report_begin_date)))

#Merge Time Matrix and Data
Butter_Matrix <- Butter %>% left_join(Time_Matrix, by =c("ID","report_begin_date"))

#Multiply Across Rows
