
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

setwd("C:/Users/jason/OneDrive/Desktop/NCU/8525_Multivariate Analysis/Week 3")

#Inpute data into R 
LeadershipData <- fread("C:/Users/jason/OneDrive/Desktop/NCU/8525_Multivariate Analysis/Week 3/TIM8525.csv")

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
ModelData <- ModelData[,c(8,45,46)]

#Testing Normality - Mardia's Test
install.packages("QuantPsyc")
library(QuantPsyc)
mardia <- mult.norm(data.frame(ModelData$MediatorDomain, ModelData$FulfillmentDomain))

#           Beta-hat  kappa     p-val
#Skewness  0.7766297 154.1610     0
#Kurtosis 11.0356344  13.0953     0
#Result: reject null - data is not multivariate normal


#Create Model
Model <- lm(cbind(ModelData$MediatorDomain, ModelData$FulfillmentDomain) ~ ModelData$Age)
Manova <- Manova(Model)
plot(Model)


#Create box-cox transformed dependent variables
StdModel <- powerTransform(cbind(FulfillmentDomain, MediatorDomain)~ Age, data = ModelData)

lambdaFul <- data.frame(StdModel$lambda)[1,1]
lambdaMed <- data.frame(StdModel$lambda)[2,1]

ModelData$FulfillmentDomain.bc <- (ModelData$FulfillmentDomain^lambdaFul - 1)/lambdaFul
ModelData$MediatorDomain.bc <- (ModelData$MediatorDomain^lambdaMed - 1)/lambdaMed

#Testing Normality - Mardia's Test after Transformation
mardia1 <- mult.norm(data.frame(ModelData$MediatorDomain.bc, ModelData$FulfillmentDomain.bc))


#MANOVA Model

Model1 <- lm(cbind(ModelData$MediatorDomain.bc, ModelData$FulfillmentDomain.bc) ~
               Age, data = ModelData)
mm <- manova(Model1)


#Different test for MANOVA
summary(mm, test = "Hotelling-Lawley")
summary(mm, test = "Roy")
summary(mm, test = "Pillai")
summary(mm, test = "Wilks")

#Check by Each Dependent Variable
summary.aov(mm)


#Box M Test - Homogenity of Covariance 
install.packages("heplots")
library(heplots)
boxM(ModelData[,4:5], ModelData$Age)

#Post-hoc Test: Linear Discriminat Analysis
depvar <- cbind(ModelData$MediatorDomain.bc, ModelData$FulfillmentDomain.bc)
mm_lda <- lda(ModelData$Age ~ depvar, CV=F)
lda_df <- data.frame(
  Age = ModelData[,"Age"],
  lda = predict(mm_lda)$x)

#plot LDA
ggplot(lda_df) + 
  geom_point(aes(x=lda.LD1, y=lda.LD2, color = Age), size = 4)+
  theme_classic()

#plot LDA by Level
for (i in 1:6) {
  assign(paste("lda_df",i, sep = ""),lda_df %>% filter(Age == i))
  
}

for (i in 1:6) {
  ggplot(assign(paste("lda_df",i, sep = ""),lda_df %>% filter(Age == i))) + 
    geom_point(aes(x=lda.LD1, y=lda.LD2, color = Age), size = 4)+
    theme_classic()+
    ggtitle(paste("Group Age:",i))
}


#LDA Plot Age Group 1
ggplot(lda_df1) + 
  geom_point(aes(x=lda.LD1, y=lda.LD2), size = 4)+
  theme_classic()+
  ggtitle("Group Age: 1")
#LDA Plot Age Group 2
ggplot(lda_df2) + 
  geom_point(aes(x=lda.LD1, y=lda.LD2), size = 4)+
  theme_classic()+
  ggtitle("Group Age: 2")
#LDA Plot Age Group 3
ggplot(lda_df3) + 
  geom_point(aes(x=lda.LD1, y=lda.LD2), size = 4)+
  theme_classic()+
  ggtitle("Group Age: 3")
#LDA Plot Age Group 4
ggplot(lda_df4) + 
  geom_point(aes(x=lda.LD1, y=lda.LD2), size = 4)+
  theme_classic()+
  ggtitle("Group Age: 4")
#LDA Plot Age Group 5
ggplot(lda_df5) + 
  geom_point(aes(x=lda.LD1, y=lda.LD2), size = 4)+
  theme_classic()+
  ggtitle("Group Age: 5")
#LDA Plot Age Group 6
ggplot(lda_df6) + 
  geom_point(aes(x=lda.LD1, y=lda.LD2), size = 4)+
  theme_classic()+
  ggtitle("Group Age: 6")







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
lmtest::dwtest(Model.bc)

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










