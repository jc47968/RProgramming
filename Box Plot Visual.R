library(data.table)
library(dplyr)
library(tidyverse)
library(moments)
library(ggplot2)
library(lattice)
library(Hmisc)
library(corrplot)

setwd("C:/Users/jason/OneDrive/Desktop/NCU/8501_Quantitative Methods for Data Analytics and Business Intelligence/Week 8")
Data1 <- fread("C:/Users/jason/OneDrive/Desktop/NCU/8501_Quantitative Methods for Data Analytics and Business Intelligence/Week 8/Skilled_Nursing_Facility_Quality_Reporting_Program_Provider_Data_Jul2021.csv")

Data1$`Start Date` <- as.Date(Data1$`Start Date`, "%m/%d/%y")#Convert format to Date#
Data1$`End Date` <- as.Date(Data1$`End Date`, "%m/%d/%y")#Convert Format to Date#


length(unique(Data1$`CMS Certification Number (CCN)`)) #15298 Skilled Nursing Facility (SNF)

Data_Pivot <- Data1 %>% select(`CMS Certification Number (CCN)`, `Measure Code`, Score) %>% 
  group_by(`CMS Certification Number (CCN)`) %>% 
  spread(key = "Measure Code", value = Score)


#Selected Candidat Variables#
Data_2 <- Data_Pivot %>% select(S_006_01_MSPB_SCORE, S_038_02_ADJ_RATE, S_007_02_OBS_RATE, S_013_02_OBS_RATE, 
                                S_001_03_OBS_RATE, S_024_02_OBS_RATE, S_022_02_ADJ_CHG_SFCR_SCORE,
                                S_004_01_PPR_PD_RSRR, S_004_01_PPR_PD_COMP_PERF, S_005_02_DTC_RS_RATE, 
                                S_006_01_MSPB_NUMB, S_006_01_MSPB_SCORE, S_005_02_DTC_COMP_PERF)


#Transformation#
Data_2 %>% group_by(S_004_01_PPR_PD_COMP_PERF) %>% 
  count()

#1 Better than the National Rate          13
#2 No Different than the National Rate 13417
#3 Not Available                        1752
#4 Worse than the National Rate          116

Data_2 %>% group_by(S_005_02_DTC_COMP_PERF) %>% 
  count()

#1 Better than the National Rate        4506
#2 No Different than the National Rate  6157
#3 Not Available                        1798
#4 Worse than the National Rate         2837

Data_2[Data_2 == "Better than the National Rate"] <- "3"
Data_2[Data_2 == "No Different than the National Rate"] <- "2"
Data_2[Data_2 == "Worse than the National Rate"] <- "1"
Data_2$S_004_01_PPR_PD_COMP_PERF <- as.numeric(Data_2$S_004_01_PPR_PD_COMP_PERF)
Data_2$S_005_02_DTC_COMP_PERF <- as.numeric(Data_2$S_005_02_DTC_COMP_PERF)
Data_2 <- Data_2 %>% drop_na(S_005_02_DTC_COMP_PERF)
Data_2$`CMS Certification Number (CCN)` <- as.numeric(Data_2$`CMS Certification Number (CCN)`)
Data_3 <- Data_2 %>% mutate_if(is.character, as.numeric) #change all chr to numeric

summary(Data_3)

Mode <- function(x) {
  uq <- unique(x)
  uq[which.max(tabulate(match(x,uq)))]
}

Mode(na.omit(Data_3)$`CMS Certification Number (CCN)`) #15009
Mode(na.omit(Data_3)$S_006_01_MSPB_SCORE)#0.98
Mode(na.omit(Data_3)$S_038_02_ADJ_RATE) #0
Mode(na.omit(Data_3)$S_007_02_OBS_RATE) #100
Mode(na.omit(Data_3)$S_013_02_OBS_RATE) #0
Mode(na.omit(Data_3)$S_001_03_OBS_RATE) #100
Mode(na.omit(Data_3)$S_024_02_OBS_RATE) #50
Mode(na.omit(Data_3)$S_022_02_ADJ_CHG_SFCR_SCORE)#8.6
Mode(na.omit(Data_3)$S_004_01_PPR_PD_RSRR) #7.48
Mode(na.omit(Data_3)$S_004_01_PPR_PD_COMP_PERF) #2
Mode(na.omit(Data_3)$S_005_02_DTC_RS_RATE) #59.23
Mode(na.omit(Data_3)$S_006_01_MSPB_NUMB) #76
Mode(na.omit(Data_3)$S_005_02_DTC_COMP_PERF) #2


Data_3_var <- sapply(na.omit(Data_3), var)
Data_3_skew <- sapply(na.omit(Data_3), skewness)
Data_3_kurt <- sapply(na.omit(Data_3), kurtosis)
Data_3_min <- sapply(na.omit(Data_3), min)
Data_3_max <- sapply(na.omit(Data_3), max)


clipr::write_clip(Data_3_max)

##Boxplot##
boxplot(S_038_02_ADJ_RATE ~ S_005_02_DTC_COMP_PERF, data = Data_3)
boxplot(S_006_01_MSPB_NUMB ~ S_005_02_DTC_COMP_PERF, data = Data_3)

Data_3_long <- reshape2::melt(Data_3[,-1], id = "S_005_02_DTC_COMP_PERF")
ggplot(Data_3_long, aes(x = S_005_02_DTC_COMP_PERF, y = value, fill = variable)) +
  geom_boxplot() +
  facet_wrap(~ S_005_02_DTC_COMP_PERF, scale = "free")

ggplot(Data_3_long, aes(x = S_005_02_DTC_COMP_PERF, y = value, fill = variable)) +
  geom_boxplot() +
  facet_wrap(~ variable, scale = "free")


##Correlation##

corr <- cor(na.omit(Data_3))
corrplot(corr)


