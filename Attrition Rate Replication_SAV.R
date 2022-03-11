library(data.table)
library(dplyr)
library(lubridate)

setwd("C:/Users/jc09975/Desktop/Model Validation/NMD Attrition Model/2022/Attrition Rate Replication")

depdata <- fread("C:/Users/jc09975/Desktop/Model Validation/NMD Attrition Model/2022/Attrition Rate Replication/Deposit.csv")

###Data Cleaning###
depdata$originationDate <- as.POSIXct(depdata$originationDate, format= "%m/%d/%Y")

###Variables Added###
EvalDate <-"01/31/2022"
depdata$EvalDate <- as.POSIXct(EvalDate, format= "%m/%d/%Y")

Month <- month(as.POSIXlt(EvalDate, format="%m/%d/%Y"))
depdata$Age <- as.Date(as.character(depdata$EvalDate, format= "%Y-%m-%d")) -
  as.Date(as.character(depdata$originationDate, format="%Y-%m-%d")) 
depdata$Age <- as.numeric(depdata$Age / 365.24)
depdata$Age_pos1 <- pmin(depdata$Age, 25.34) #set max age cap
depdata$Business <- ifelse(depdata$`Customer Type` == "Business", 1, 0)
depdata$Age_less_6m <- ifelse(depdata$Age_pos1 < 0.5, 1, 0)
depdata$Age_neg05 = depdata$Age_pos1^(-0.5)
depdata$Age_neg2 <- depdata$Age_pos1^(-2)

###Modeling Variables - SAV###
depdata$Log_age <- log(depdata$Age_pos1 + 1)
depdata$Orig_BOH <- ifelse(depdata$institution == "BOH", 1, 0)
depdata$Orig_Nara <- ifelse(depdata$institution == "Nara", 1, 0)
depdata$Orig_Foster <- ifelse(depdata$institution == "Foster", 1, 0)
depdata$Orig_pi <- ifelse(depdata$institution == "pacific", 1, 0)
depdata$Try120M <- 0.0179 #Need to update with data
depdata$Age_neg1 <- depdata$Age_pos1^(-1)
depdata$Bus_tm_ageless6m <- depdata$Business * depdata$Age_less_6m
depdata$Age_neg05_tmlog <- depdata$Age_neg05 * depdata$Log_age
depdata$Age_neg2_tmlog <- depdata$Age_neg2 * depdata$Log_age
depdata$Season_eff_sin4 <- sin(4*2*pi* Month/12)
depdata$Season_eff_sin5 <- sin(5*2*pi*Month/12)
depdata$Merger = 0
depdata$Age_pos2_tmlog <- depdata$Age_pos1^2 * depdata$Log_age

##Regression##

depdata$predict_initial <- -4.372822668322 - 0.622049963976*depdata$Log_age - 0.129649746998*depdata$Orig_BOH
- 0.007425096259*depdata$Orig_Nara + 0.116852347991*depdata$Orig_Foster - 0.117496402153 * depdata$Orig_pi 
+ 0.098450053251*depdata$Try120M + 0.005663785078*depdata$Age_neg1 - 0.919032753235*depdata$Bus_tm_ageless6m
+1.719137411905*depdata$Age_neg05_tmlog + 0.009167721931*depdata$Age_neg2_tmlog + 0.067942444377*depdata$Season_eff_sin4 + 
  0.059473423985*depdata$Season_eff_sin5 + 0.336096698160*depdata$Merger + 0.000335589621*depdata$Age_pos2_tmlog

depdata$predict_attrition_probability <- exp(depdata$predict_initial)/(1+exp(depdata$predict_initial)) 
depdata$predict_attrition_rate <- depdata$predict_attrition_probability*100 

write.csv(x = depdata, file = "SAV_Attrition.csv")
