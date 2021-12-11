library(data.table)
library(tidyverse)
library(dplyr)
library(broom)
library(MASS)

setwd("C:/Users/jason/OneDrive/Desktop/NCU/8520_Inferential Statistics and Predictive Analytics/Week 6")
Data <- fread("C:/Users/jason/OneDrive/Desktop/NCU/8520_Inferential Statistics and Predictive Analytics/Week 6/VideoGameData.csv")

##Data Transformation##
Data1 <- Data
Data1 <- Data1 %>% mutate(Date = recode(Date, 'Sunday' = "0", 'Monday' = "1", 'Tuesday' = "2",
                                        'Wednesday' = "3", 'Thursday' = "4", 'Friday' = "5", 
                                        'Saturday' = "6",), 
                          Game = recode(Game, 'Police' = "0", 'Theif' = "1"),
                          Advertising = recode(Advertising, 'No' = "0", 'Yes' = "1"),
                          Date = as.numeric(Date), 
                          Game = as.numeric(Game),
                          Advertising = as.numeric(Advertising)) 
#Modeling#
Model_uni <- glm(Advertising ~ VisitTime, data = Data1, family = "binomial")
Model_multi <- glm(Advertising ~ VisitTime + Game, data = Data1, family = "binomial")

#Prediction#
pred_uni <- predict(Model_uni, type = "response")
pred_multi <- predict(Model_multi, type = "response")


#Linearity Assumption
#univariate linear chart
ldata <- Data1 %>% mutate(logit = log(pred_uni/(1-pred_uni))) %>% 
        gather(key="Advertising", value = "Ind_Var.value", -logit)
ggplot(ldata, aes(logit, Ind_Var.value)) +
        geom_point(size = 0.5, alpha = 0.5) +
        geom_smooth(method = "loess") +
        theme_bw() +
        facet_wrap(~Advertising, scale = "free_y")

#multi linear chart
lmdata <- Data1 %>% mutate(logit = log(pred_multi/(1-pred_multi))) %>% 
        gather(key="Advertising", value = "Ind_Var.value", -logit)
ggplot(lmdata, aes(logit, Ind_Var.value)) +
        geom_point(size = 0.5, alpha = 0.5) +
        geom_smooth(method = "loess") +
        theme_bw() +
        facet_wrap(~Advertising, scale = "free_y")


#Influential Values
ck_uni <- plot(Model_uni, which = 4, id.n = 3, main = "Univariate")
ck_multi <- plot(Model_multi, which = 4, id.n = 3, main = "Multivariate")

model_uni.data <- augment(Model_uni) %>% 
        mutate(index = 1:n()) %>% 
        top_n(3,.cooksd) %>% 
        filter(abs(.cooksd) > 0.09524)
         #No Influential Observations 

model_multi.data <- augment(Model_multi) %>% 
        mutate(index = 1:n()) %>% 
        top_n(3,.cooksd) %>% 
        filter(abs(.cooksd) > 0.09756) #No Influential Observations 

#Multicollinearity
car::vif(Model_multi)
#VisitTime      Game 
# 1.028265  1.028265 


##GOF Results##
#chi square test#
chi_uni <- anova(Model_uni, test = "Chisq")#0.002016 **
chi_multi <-anova(Model_multi, test = "Chisq")
#VisitTime  1   9.5352        42     51.462 0.002016 **
#Game       1   0.2536        41     51.208 0.614566 

#Pseudo R2#
library(DescTools)
pse_uni <- PseudoR2(Model_uni, c("McFadden", "Nagel"))     
#McFadden Nagelkerke 
#0.1563229  0.2597817 

pse_multi <- PseudoR2(Model_multi, c("McFadden", "Nagel"))
#McFadden Nagelkerke 
#0.1604802  0.2659509

#Complete Separation Test#
library(detectseparation)
sep_uni <- glm(Advertising ~ VisitTime, data = Data1, family = binomial("logit"),
               method = "detect_separation") #FALSE
sep_multi <- glm(Advertising ~ VisitTime + Game, data = Data1, family = binomial("logit"),
                 method = "detect_separation")#FALSE

######################################################################################
###################web scrape MTA Turnstile Data######################################

##Function to get all links starting with home directory##
library(xml2)
library(magrittr)
.get_link <- function(u){
        node <- xml2::read_html(u)
        hrefs <- xml2::xml_find_all(node, ".//a[not(contains(@href,'../'))]") %>% xml_attr("href")
        urls <- xml2::url_absolute(hrefs, xml_url(node))
        if(!all(tools::file_ext(urls) == "txt")){
                lapply(urls, .get_link)
        }else {
                return(urls)
        }
}
#Get links
a <- .get_link("http://web.mta.info/developers/turnstile.html")



################################3
library(rvest)
link <-readLines("http://web.mta.info/developers/turnstile.html")
sig_pattern = '^.*<p><a href="(https.*)">.*$'
sig_hrefs = grep(sig_pattern, link, value = TRUE)





