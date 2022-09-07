
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

#Testing Normality - Mardia's Test
mardia <- QuantPsyc::mult.norm(data.frame(ModelData$MediatorDomain, ModelData$FulfillmentDomain))


#Create Model
Model <- lm(cbind(ModelData$MediatorDomain, ModelData$FulfillmentDomain) ~ 
              ModelData$Age + ModelData$Employment)
fManova <- Manova(Model)


#Create box-cox transformed dependent variables
StdModel <- powerTransform(cbind(FulfillmentDomain, MediatorDomain)~ Age + Employment, data = ModelData)

lambdaFul <- data.frame(StdModel$lambda)[1,1]
lambdaMed <- data.frame(StdModel$lambda)[2,1]

ModelData$FulfillmentDomain.bc <- (ModelData$FulfillmentDomain^lambdaFul - 1)/lambdaFul
ModelData$MediatorDomain.bc <- (ModelData$MediatorDomain^lambdaMed - 1)/lambdaMed

#Testing Normality - Mardia's Test after Transformation
mardia1 <- mult.norm(data.frame(ModelData$MediatorDomain.bc, ModelData$FulfillmentDomain.bc))

#MANOVA Model

Model1 <- lm(cbind(ModelData$MediatorDomain.bc, ModelData$FulfillmentDomain.bc) ~
               Age + Employment + Age*Employment, data = ModelData)
mm <- manova(Model1)

#Different test for MANOVA
summary(mm, test = "Hotelling-Lawley", type="III")
summary(mm, test = "Roy", type="III")
summary(mm, test = "Pillai", type="III")
summary(mm, test = "Wilks", type="III")

#Testing Normality in Error Term
error <- mm$residuals
ks.test(error, "pnorm", mean=mean(error), sd=sd(error))

#Box M Test - Homogenity of Covariance - Age
heplots::boxM(cbind(ModelData$MediatorDomain.bc, ModelData$FulfillmentDomain.bc) ~
                Age, data = ModelData)

#Box M Test - Homogenity of Covariance - Employment
heplots::boxM(cbind(ModelData$MediatorDomain.bc, ModelData$FulfillmentDomain.bc) ~
                Employment, data = ModelData)

#Box M Test - Homogenity of Covariance - Age:Employment
heplots::boxM(cbind(ModelData$MediatorDomain.bc, ModelData$FulfillmentDomain.bc) ~
                Age * Employment, data = ModelData)

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
}#not working


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

###Post-hoc tests for statistically significant MANOVA###
#1. anova_test() when normality and homogeneity of variance is met
#2. welch_anova_test() when homogeneity of variance is violated
#3. kruskal_test () non-parametric alternative

library(tidyr)
library(rstatix)
pht <- ModelData %>% 
  gather(key = "variable", value = "value", 
         FulfillmentDomain.bc, MediatorDomain.bc) %>% 
  group_by(variable)
#post hoc for Age
pht %>% anova_test(value ~ Age)
pht %>% welch_anova_test(value ~ Age)
pht %>% kruskal.test(value ~ Age)

#post hoc for Employment
pht %>% anova_test(value ~ Employment)
pht %>% welch_anova_test(value ~ Employment)
pht %>% kruskal.test(value ~ Employment)




