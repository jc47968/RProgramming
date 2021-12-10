library(data.table)
library(cvAUC)
library(dplyr)
library(tidyverse)
library(ROCR)
library(AUC)
library(MLmetrics)



setwd("C:/Users/jc09975/Desktop/Model Validation/C&I Moody's RiskCalc/OPM 3Q2021")
Data1 <- fread("C:/Users/jc09975/Desktop/Model Validation/C&I Moody's RiskCalc/OPM 3Q2021/AUC.csv")
Data1$`ACB based PD %` <- str_remove_all(Data1$`ACB based PD %`, "[%]")
Data1$`ACB based PD %` <- as.numeric(Data1$`ACB based PD %`)


##Calculate AUC##
pred_ci <- prediction(Data1$`ACB based PD %`, Data1$Default)
auc_perf_ci <- performance(pred_ci, measure = "auc")
auc_metric_ci <- auc_perf_ci@y.values[[1]] #0.8489249

#Plot ROC#
rocci <- performance(pred_ci, "tpr", "fpr")
plot(rocci, col = "red", lwd = 5, main = "ROC Curve for C&I CECL")



