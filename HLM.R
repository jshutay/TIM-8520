#Install packages.
 
install.packages("car"); install.packages("compute.es"); install.packages("ggplot2"); install.packages("multcomp"); install.packages("pastecs"); install.packages("mlogit"); install.packages("nlme"); 
install.packages("reshape"); install.packages("QuantPsyc"); install.packages("boot")

#Load packages
library(car); library(compute.es); library(ggplot2); library(multcomp); library(multcomp); library(pastecs); library(mlogit); library(nlme); library(reshape); library(QuantPsyc); library(boot)

#Inpute data into R 
LeadershipData <- read.csv("https://raw.githubusercontent.com/jshutay/TIM-8520/master/LeadershipData.csv",
                           header = TRUE,
                           sep = ",")

#Create factor variables
LeadershipData$Gender <- as.factor(LeadershipData$Gender)
LeadershipData$Employment <- as.factor(LeadershipData$Employment)

#Create models with fixed intercept and random intercept
fixedInterceptOnly <-gls(ValuesDomain ~ 1, data = LeadershipData, method = "ML", na.action = na.exclude)
 summary(fixedInterceptOnly)

randomInterceptOnly <-lme(ValuesDomain ~ 1, data = LeadershipData, random = ~1|Gender, method = "ML", na.action = na.exclude)
 summary(randomInterceptOnly)

#Compare models to determine if random is sig. improvement 
 logLik(fixedInterceptOnly)*-2
 logLik(randomInterceptOnly)*-2
 
#Adding fixed effects - NOTE: IF YOU USE THE INDIVIDUAL ITEMS (X1-X31) AS FIXED EFFECTS, YOU WILL GET AN ERROR BECAUSE OF MISSING VALUES AND THE MODELS NOT HAVING THE SAME NUMBER OF OBSERVATIONS
randomInterceptFixed <-lme(ValuesDomain ~ MediatorDomain + FulfillmentDomain, data = LeadershipData, random = ~1|Gender, method = "ML", na.action = na.exclude)
summary(randomInterceptFixed)


#Comparing the fit of the models
anova(randomInterceptOnly, randomInterceptFixed)
