
#Install required pacakges for analyses
install.packages("car"); install.packages("compute.es"); install.packages("ggplot2"); install.packages("multcomp"); install.packages("mlogit"); 
install.packages("nlme"); install.packages("reshape"); install.packages("effects"); install.packages("Hmisc"); install.packages("psych"); install.packages("HH")

#Load required packages
library(car); library(compute.es); library(ggplot2); library(multcomp); library(mlogit); library(nlme); library(reshape); library(effects); library(Hmisc); library(psych); library(HH)


#Inpute data into R 
LeadershipData <- read.csv("https://raw.githubusercontent.com/jshutay/TIM-8520/master/LeadershipData.csv",
                       header = TRUE,
                       sep = ",")

#Omit missing data cases - you can select to simply delete all of the missing values, but you need to justify your approach. You are not required to run this line of code.
LeadershipData <- na.omit(LeadershipData)

#Conduct data explorations and test statistical assumptions - use code from ANCOVA.R and add as needed



#Factorial MANOVA
Education <- as.factor(LeadershipData$Education)
Gender <- as.factor(LeadershipData$Gender)
y <- cbind(LeadershipData$ValuesDomain, LeadershipData$MediatorDomain, LeadershipData$FulfillmentDomain)
fit <- manova(y ~ Education + Gender)
summary(fit)

summary.aov(fit)

