#Install packages.
 
install.packages("car"); install.packages("compute.es"); install.packages("ggplot2"); install.packages("multcomp"); install.packages("pastecs"); install.packages("mlogit"); install.packages("nlme"); 
install.packages("reshape"); install.packages("QuantPsyc"); install.packages("boot")

#Load packages
library(car); library(compute.es); library(ggplot2); library(multcomp); library(multcomp); library(pastecs); library(mlogit); library(nlme); library(reshape); library(QuantPsyc); library(boot)

#Inpute data into R 
LeadershipData <- read.csv("https://raw.githubusercontent.com/jshutay/TIM-8520/master/LeadershipData.csv",
                           header = TRUE,
                           sep = ",")

#Omit missing data cases - it is recommended that you run this line of code to avoid errors in comparing models.
LeadershipData <- na.omit(LeadershipData)

#Create factor variable - using Gender as the example
LeadershipData$Gender <- as.factor(LeadershipData$Gender)

#Create models with fixed intercept 
fixedInterceptOnly <-gls(ValuesDomain ~ 1, data = LeadershipData, method = "ML", na.action = na.exclude)
 summary(fixedInterceptOnly)

#Create models with random intercept
randomInterceptOnly <-lme(ValuesDomain ~ 1, data = LeadershipData, random = ~1|Gender, method = "ML", na.action = na.exclude)
 summary(randomInterceptOnly)

#Adding fixed effects 
randomInterceptFixed <-lme(ValuesDomain ~ MediatorDomain + FulfillmentDomain, data = LeadershipData, random = ~1|Gender, method = "ML", na.action = na.exclude)
summary(randomInterceptFixed)

#Comparing the fit of the models
anova(fixedInterceptOnly, randomInterceptOnly, randomInterceptFixed)
