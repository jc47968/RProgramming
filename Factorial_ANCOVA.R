
#Install required pacakges for analyses
install.packages("car")
install.packages("compute.es")
install.packages("ggplot2")
install.packages("multcomp")
install.packages("mlogit")
install.packages("nlme")
install.packages("reshape")
install.packages("effects")
install.packages("Hmisc")
install.packages("psych")
install.packages("HH")

#Load required packages
library(car)
library(compute.es)
library(ggplot2)
library(multcomp)
library(mlogit)
library(nlme)
library(reshape)
library(effects)
library(Hmisc)
library(psych)
library(HH) 
library(data.table)
library(dplyr)

setwd("C:/Users/jason/OneDrive/Desktop/NCU/8525_Multivariate Analysis/Week 2")

#Inpute data into R 
LeadershipData <- fread("C:/Users/jason/OneDrive/Desktop/NCU/8525_Multivariate Analysis/Week 2/TIM8525.csv")

#Omit missing data cases - you can select to simply delete all of the missing values, but you need to justify your approach. You are not required to run this line of code.
LeadershipData <- na.omit(LeadershipData)

#Omit zero variance response observations
LeadershipData <- LeadershipData %>% filter(ZeroVar != 1)

#Getting outlier for Durationinseconds using 2.5 x IQR
DurSumm <- summary(LeadershipData$Durationinseconds)
DurIQR <- DurSumm[5] - DurSumm[2]
DurLower <- DurSumm[2] - 2.5*DurIQR
DurUpper <- DurSumm[5] + 2.5*DurIQR

#Omit Outlier 
LeadershipData <- LeadershipData %>% filter(Durationinseconds < DurUpper)

#Change Variables to Factors
ModelData <- LeadershipData
ModelData[,c(4,5,7:41)] <- lapply(ModelData[,c(4,5,7:41)], factor)

#Create Quantitative Dataset
QuantData <- as.data.frame(LeadershipData[,44:46])

#Create histogram of continuous variables
hist(QuantData$MediatorDomain,main = "MediatorDomain")
hist(QuantData$ValuesDomain, main = "ValuesDomain")
hist(QuantData$FulfillmentDomain, main = "FulfillmentDomain")

#Scatterplot of continuous variables
plot(QuantData)

#Get correlation between continuous variables
corr <- cor(QuantData)
corrplot::corrplot(corr)


#Get Modeling Dataset: 
ModelData <- ModelData[,c(4,8,45,46)]
ModelData$'MediatorDomain*Employment' <- ModelData$MediatorDomain * as.numeric(ModelData$Employment)
ModelData$'MediatorDomain*Age' <-ModelData$MediatorDomain * as.numeric(ModelData$Age)

#Testing Normality in Dependent Variable
dep <- ModelData$FulfillmentDomain
shapiro.test(dep)
ks.test(dep, "pnorm", mean=mean(error), sd=sd(error))
nortest::ad.test(dep)


#Create Model
Model <- aov(FulfillmentDomain ~ ., data = ModelData)
anova(Model)
plot(Model)

#Testing Normality in Error Term
error <- Model$residuals
ks.test(error, "pnorm", mean=mean(error), sd=sd(error))


#Box Cox Transformation of Dependent Variable
library(MASS)
bc <- boxcox(Model)
(bc.power <- bc$x[which.max(bc$y)])
BCTransform <- function(y, lambda=0) {
  if (lambda == 0L) { log(y) }
  else { (y^lambda - 1) / lambda }
}
BCTransformInverse <- function(yt, lambda=0) {
  if (lambda == 0L) { exp(yt) }
  else { exp(log(1 + lambda * yt)/lambda) }
}
yt <- BCTransform(ModelData$FulfillmentDomain, 0)
yo <- BCTransformInverse(yt, 0)
unique(round(yo-ModelData$FulfillmentDomain),8)

yt <- BCTransform(ModelData$FulfillmentDomain, 0.5)
yo <- BCTransformInverse(yt, 0.5)
unique(round(yo-ModelData$FulfillmentDomain),8)

ModelData$FulfillmentDomain.bc <- BCTransform(ModelData$FulfillmentDomain, bc.power)
hist(ModelData$FulfillmentDomain.bc, breaks=12); rug(ModelData$FulfillmentDomain.bc)

summary(Model.bc <- aov(FulfillmentDomain.bc ~ Employment +
                         Age + MediatorDomain + MediatorDomain*Employment +
                         MediatorDomain*Age, data=ModelData))

#Visual after Box Cox Transformation
par(mfrow=c(2,2))
plot(Model.bc)

#Testing Normality in Error Term after Box Cox Transformation
error.bc <- Model.bc$residuals
ks.test(error.bc, "pnorm", mean=mean(error.bc), sd=sd(error.bc))

#Model Result after Box Cox
summary(Model.bc)
anova(Model.bc)

#Durbin-Watson Test
lmtest::dwtest(Model.bc)#p-value <2e-16: positive autocorrelation

#Levene's Test for Homogeneity of Variance
library(car)
leveneTest(ModelData$FulfillmentDomain.bc, ModelData$Employment)#0.8874
leveneTest(ModelData$FulfillmentDomain.bc, ModelData$Age)#0.1568


#Post Hoc Analysis for Age - Tukey
TukeyHSD(Model.bc)

library(multcomp)
postHocsAge <- glht(Model.bc, linfct = mcp(Age = "Tukey"))
summary(postHocsAge)
confint(postHocsAge)
plot(postHocsAge)

#Histogram Age
ggplot(ModelData, aes(x = FulfillmentDomain.bc)) +
  geom_histogram(aes(color = Age), position = "identity", 
                 bins = 30)

#Post Hoc Analysis for Employment - ttest
pairwise.t.test(ModelData$FulfillmentDomain.bc, 
                ModelData$Employment,
                p.adjust.method = "BH")

#Histogram Employment
ggplot(ModelData, aes(x = FulfillmentDomain.bc)) +
  geom_histogram(aes(color = Employment), fill="white",
                 position = "identity", 
                 bins = 30)










