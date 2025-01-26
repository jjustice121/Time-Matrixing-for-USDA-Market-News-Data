#Time Matrixing Script 

#Load Libraries
library(lubridate)
library(tidyverse)
library(USDAMARS)

#Set API Key
MARS_API_Key()

Dir <- MARS_Table_Directory()
#Get Table
Butter <- MARS_Table_Pull(1099)

Butter <- Butter %>% select(c("report_begin_date","report_end_date"))

#get days in range

#add unique ID
Butter$ID <- as.numeric(factor(Butter$report_begin_date, levels = unique(Butter$report_begin_date)))

#functions to get day count in each respective month within range for report

f1 <- function(d_first,d_last){
  d_first <- mdy(d_first)
  d_last <- mdy(d_last)
  
  
  D <- seq.Date(d_first, d_last, 1) # generate all days in [d_first,d_last]
  
  D <- D[!weekdays(D) %in% c("Saturday","Sunday")]
  
  M <- unique(format(D, "%m")) # all months in [d_first,d_lst]
  
  
  f2 <- function(x) length(which(format(D, "%m") == x)) # returns number of days in month x
  
  res <- vapply(M,f2,numeric(1))
  
  return(cbind(unique(format(D, "%Y-%m")),res))
}

f3 <- function(k) f1(Butter$report_begin_date[k],Butter$report_end_date[k])

f4 <- function(k) cbind(Butter$ID[k],Butter$report_begin_date[k], f3(k))


Days_output <- sapply(1:nrow(Butter), f4)

Days_df <- as.data.frame(do.call(rbind, Days_output))

colnames(Days_df) <- c("ID","report_begin_date","Month-Year","Number_of_Business_Days") 

Days_df$Month <- substr(Days_df$`Month-Year`,6,7)

Days_df <- Days_df %>% pivot_wider(names_from = Month,values_from = Number_of_Business_Days) %>% mutate(ID = as.numeric(ID))

Days_df <- Days_df %>% select(c("ID","report_begin_date","Month-Year","01","02","03","04","05","06","07","08","09","10","11","12")) 

colnames(Days_df)[4:15] <- month.name

#merge Days_df into the original Butter df on ID and report begin date

International_Time_Matrix_MMN <- Days_df %>% left_join(Butter, by = c("ID","report_begin_date")) %>% relocate(report_end_date,.after = report_begin_date)


#export dataframe
write.csv(International_Time_Matrix_MMN, row.names = FALSE)

