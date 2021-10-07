library(readr)
library(dplyr)
library(tidyr)
library(purrr)


setwd("C:/Users/jc09975/Desktop/Model Validation/Core Deposit/Deposit Data From Treasury")
data_path <- "C:/Users/jc09975/Desktop/Model Validation/Core Deposit/Deposit Data From Treasury"
files <- list.files(data_path, pattern = "*.csv", recursive = TRUE)

##no filename column##
to_rename <- rlang::exprs(
  'accountNo' = 'account_id',
  'Region' = 'region',
  'State' = 'state')

read_source <- function(flnm){
  read_csv(flnm) %>% 
    mutate(filname = flnm)
}

Data <- files %>% map_df(~read_csv(., col_types = cols(orig_date = col_date(format = "%m/%d/%y"),
                                                       mat_date = col_date(format = "%m/%d/%y"),
                                                       last_renew_date = col_date(format = "%m/%d/%y"),
                                                       close_date = col_date(format = "%m/%d/%y"),
                                                       promo_end_date = col_date(format = "%m/%d/%y"))),
                         filename = files)

Data_State <- Data[complete.cases(Data$State),]


Dep_Data <- Data

write.csv(Dep_Data, "Dep Data_Treasury.csv")

###Negative Age Check###
neg_age <- if (Dep_Data$orig_date < Dep_Data$mat_date) {
  neg_age$neg == "1" 
 
} else {
  neg_age$neg == "0"
}





































##filename column##  
Data_files <- data_frame(filename = files) %>% 
  mutate(file_contents= map(filename,
                            ~ read_csv(file.path(data_path,.,
                                                 col_types = cols(.default = "c")))))

Data_files <- unnest(data = Data_files)


#Data <- data_frame(filename = files) %>% 
#  mutate(file_contents= map(filename,
#                            ~ read_csv(file.path(data_path,.))))