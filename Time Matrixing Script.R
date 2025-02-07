#Time Matrixing Script 

#Load Libraries
library(lubridate)
library(tidyverse)
library(USDAMARS)
library(openxlsx)

#Set API Key
MARS_API_Key("d8qcrgndThTeo/iTpu5AJF8rqbe+AT7mImzqT4gWe2U=")

#get MARS report directory
Dir <- MARS_Table_Directory()

#Write function for create time matrix

Time_Matrixing <- function(slug_id){
  
  df <- MARS_Table_Pull(slug_id)
  
  title <- df$report_title[1]

  df <- df %>% select(c("report_begin_date","report_end_date"))
  
  df <- df %>% filter(!(report_begin_date == report_end_date))

  #get days in range

  #add unique ID
  df$ID <- as.numeric(factor(df$report_begin_date, levels = unique(df$report_begin_date)))

  #functions to get day count in each respective month within range for report

  f1 <- function(d_first,d_last){
    d_first <- mdy(d_first)
    d_last <- mdy(d_last)
  
  
    D <- seq.Date(d_first, d_last, 1) # generate all days in [d_first,d_last]
  
    D <- D[!weekdays(D) %in% c("Saturday","Sunday")]
    
    if(length(D) < 10){
      
      D <- seq.Date(d_first,d_last + 4,1)
      
    }else{
      
      D <- seq.Date(d_first,d_last,1)
    }
    
    D <- D[!weekdays(as.Date(D)) %in% c("Saturday","Sunday")]
    
    print(D)
    
    M <- unique(format(D, "%m")) # all months in [d_first,d_lst]
  
  
    f2 <- function(x) length(which(format(D, "%m") == x)) # returns number of days in month x
  
    res <- vapply(M,f2,numeric(1))
  
    return(cbind(unique(format(D, "%Y-%m")),res))
  }

  f3 <- function(k) f1(df$report_begin_date[k],df$report_end_date[k])

  f4 <- function(k) cbind(df$ID[k],df$report_begin_date[k], f3(k))


  Days_output <- sapply(1:nrow(df), f4)

  Days_df <- as.data.frame(do.call(rbind, Days_output))

  colnames(Days_df) <- c("ID","report_begin_date","Month-Year","Number_of_Business_Days") 

  Days_df$Month <- substr(Days_df$`Month-Year`,6,7)

  Days_df <- Days_df %>% pivot_wider(names_from = Month,values_from = Number_of_Business_Days) %>% mutate(ID = as.numeric(ID))

  Days_df <- Days_df %>% select(c("ID","report_begin_date","Month-Year","01","02","03","04","05","06","07","08","09","10","11","12")) 

  colnames(Days_df)[4:15] <- month.name

  #merge Days_df into the original Butter df on ID and report begin date

  Matrix <- Days_df %>% left_join(df, by = c("ID","report_begin_date")) %>% select(-c("report_end_date"))
  
  #vector of month names
  Months <- c("January","February","March","April","May","June","July","August","September","October","November","December")
  
  #transform columns
  Matrix[,Months] <- lapply(Matrix[,Months], function(x) substr(x,1,2))
  
  #transform to numeric
  Matrix[,Months] <- lapply(Matrix[,Months], function(x) as.numeric(x))
  
  assign(title,Matrix, envir = .GlobalEnv)
  
}

#Iterate Over List of Reports

#report list
Report_List <- c(1034,1038,1082,1099)

lapply(Report_List,Time_Matrixing)

#export dataframe

#create list of sheets for Excel File
sheets <- list("Butter - Oceania" = `Butter - Oceania`, "Cheese - Oceania" = `Cheese - Oceania`,"Skim Milk Powder - Oceania" = `Skim Milk Powder - Oceania`,"Dry Whey - Europe" = `Dry Whey - Europe`)

#Write Sheets to Excel File
write.xlsx(sheets,"C:/Users/jessie.justice/OneDrive - USDA/Desktop/Data Analysis Projects/EAB Data Management/Test Scripts and Programs/Test Outputs/International_Market_News_Time_Matrix.xlsx")

