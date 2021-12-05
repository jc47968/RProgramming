library(data.table)
library(moments)
library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)
library(tidyr)



setwd("C:/Users/jason/OneDrive/Desktop/NCU/8550_Data Preparation Methods/Week 4")

##Import Datasets##
TurnData <- fread("C:/Users/jason/OneDrive/Desktop/NCU/8550_Data Preparation Methods/Week 4/Turnstile Data.csv")
MktData <- fread("C:/Users/jason/OneDrive/Desktop/NCU/8550_Data Preparation Methods/Week 4/marketing_campaign.csv")

##Functions##
Mode <- function(x) {
  uq <- unique(x)
  uq[which.max(tabulate(match(x,uq)))]
}

###Turnstile Data###
##Data Cleaning##

#Transform Data Type for Date and Time Variables
TurnData$DATE <- as.Date(TurnData$DATE, "%m/%d/%y")
TurnData$TIME[1] <- "12:00:00 AM"
TurnData$TIME <- parse_date_time(TurnData$TIME, "%H:%M:%S, %p")
TurnData$TIME <- format(as.POSIXct(TurnData$TIME), "%H:%M:%S")

str(TurnData) #Review the data structure 
#change data structure from character to factor for variables: C/A, UNIT, 
#SCP, STATION,LINENAME,DIVISION, DESC into factors
TurnNames <- c(2:7,10)
setDT(TurnData)[,(TurnNames):= lapply(.SD, factor), .SDcols=TurnNames]

#Unique Check
TurnUnique <-unique(TurnData[,c(1:12)]) #combine all variable and get unique observations
nrow(TurnUnique)#number of unique rows is 841269
nrow(TurnData) #number of unique rows is 841269

#Review for String Inconsistencies
sapply(TurnData[,c(2:7,10)], unique)#get all unique levels
sapply(TurnData, function(x) length(unique(x)))#get the count of levels


#Data Analysis for Outliers
TurnStats <- as.data.frame(summary(TurnData)) #get summary of dataset
colnames(TurnStats) <-c('N/A', 'Variable', 'Metrics') #rename columns
TurnStats <- TurnStats[,!names(TurnStats) %in% 'N/A'] #remove "N/A" column

Mode(TurnData$ENTRIES)#0
Mode(TurnData$EXITS) #0
Mode(TurnData$DATE)#"2020-11-07"

#Check For Missing Data
sum(is.na(TurnData)) #get total missing data in data set

#percentage of missing data for each variable
apply(TurnData, 2, function(col)sum(is.na(col))/length(col))





##Marketing Data##
MktStats <- as.data.frame(summary(MktData))
colnames(MktStats) <-c('N/A', 'Variable', 'Metrics')
MktStats <- MktStats[,!names(MktStats) %in% 'N/A']
write.csv(MktStats, "MktStats.csv")

Mode(MktData$Year_Birth) #1976
Mode(MktData$Income)#7500
Mode(MktData$Kidhome)#0
Mode(MktData$Teenhome)#0
Mode(MktData$Recency)#56
Mode(MktData$MntWines)#2
Mode(MktData$MntFruits)#0
Mode(MktData$MntMeatProducts)#7
Mode(MktData$MntFishProducts)#0
Mode(MktData$MntSweetProducts)#0
Mode(MktData$MntGoldProds)#1
Mode(MktData$NumDealsPurchases)#1
Mode(MktData$NumWebPurchases)#2
Mode(MktData$NumCatalogPurchases)#0
Mode(MktData$NumStorePurchases)#3
Mode(MktData$NumWebVisitsMonth)#7
Mode(MktData$AcceptedCmp3)#0
Mode(MktData$AcceptedCmp2)#0
Mode(MktData$AcceptedCmp1)#0
Mode(MktData$AcceptedCmp5)#0
Mode(MktData$AcceptedCmp4)#0
Mode(MktData$Complain)
Mode(MktData$Response)






















