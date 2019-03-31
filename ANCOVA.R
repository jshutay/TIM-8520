
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

#Conduct data explorations and test statistical assumptions 


#create a bar chart
bar <- ggplot(LeadershipData, aes(Gender, ValuesDomain))
bar + stat_summary(fun.y = mean, geom = "bar", position ="dodge") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width = 0.90), width = 0.2) + labs(x = "Gender", y = "Values Score")

bar <- ggplot(LeadershipData, aes(Gender, MediatorDomain))
bar + stat_summary(fun.y = mean, geom = "bar", position ="dodge") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width = 0.90), width = 0.2) + labs(x = "Gender", y = "Mediator Score")

bar <- ggplot(LeadershipData, aes(Gender, FulfillmentDomain))
bar + stat_summary(fun.y = mean, geom = "bar", position ="dodge") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width = 0.90), width = 0.2) + labs(x = "Gender", y = "Fulfillment Score")


#Testing normality with a Q-Q plot
qqplot.time <- qplot(sample = LeadershipData$ValuesDomain, stat="qq") 
qqplot.time

#Testing normality with a Q-Q plot
qqplot.time <- qplot(sample = LeadershipData$MediatorDomain, stat="qq") 
qqplot.time

#Testing normality with a Q-Q plot
qqplot.time <- qplot(sample = LeadershipData$FulfillmentDomain, stat="qq") 
qqplot.time

#Histogram
myhistogram <- ggplot(LeadershipData, aes(ValuesDomain))
myhistogram + geom_histogram(aes(y = ..density..))
hist_values <- ggplot(LeadershipData, aes(ValuesDomain)) + theme(legend.position = "none") + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x="Values Score", y = "Density")
hist_values

#Create histograms for MediatorDomain and FulfillmentDomain: write code below.




#Creating separate dataframes for males and females and creating histograms
males<-subset(LeadershipData, LeadershipData$Gender == 1)
females<-subset(LeadershipData, LeadershipData$Gender == 2)


hist.males <- ggplot(males, aes(ValuesDomain)) + geom_histogram(aes(y = ..density..), fill = "white", colour = "black", binwidth = 1) + labs(x = "Values Score", y = "Density") + stat_function(fun=dnorm, args=list(mean =  mean(males$ValuesDomain, na.rm = TRUE), sd = sd(males$ValuesDomain, na.rm = TRUE)), colour = "blue", size=1)
hist.males

hist.females <- ggplot(females, aes(ValuesDomain)) + geom_histogram(aes(y = ..density..), fill = "white", colour = "black", binwidth = 1) + labs(x = "Values Score", y = "Density") + stat_function(fun=dnorm, args=list(mean =  mean(females$ValuesDomain, na.rm = TRUE), sd = sd(females$ValuesDomain, na.rm = TRUE)), colour = "blue", size=1)
hist.females



#Scatterplot
scatterplot(LeadershipData$ValuesDomain, LeadershipData$MediatorDomain)
scatterplot(LeadershipData$ValuesDomain, LeadershipData$FulfillmentDomain)
scatterplot(LeadershipData$FulfillmentDomain, LeadershipData$MediatorDomain)


#Testing statistical assumptions for parametric tests

#Shapiro-Wilk tests for normality
shapiro.test(LeadershipData$ValuesDomain)
by(LeadershipData$ValuesDomain, LeadershipData$Gender, shapiro.test)
by(LeadershipData$ValuesDomain, LeadershipData$Employment, shapiro.test)

#Create Shapiro-Wilk tests for the MediatorDomain and FulfillmentDomain variables: write your code below.


#Compute descritive statistics
describeBy(LeadershipData$ValuesDomain, LeadershipData$Gender)
describeBy(LeadershipData$ValuesDomain, LeadershipData$Employment)

#Converting gender and employment variables to factors

LeadershipData$Gender <- as.factor(LeadershipData$Gender)
LeadershipData$Employment <- as.factor(LeadershipData$Employment)

#Levene's test for homogeneity of variance
leveneTest(LeadershipData$ValuesDomain, LeadershipData$Gender)
leveneTest(LeadershipData$ValuesDomain, LeadershipData$Employment)


#Testing homogeneity of regression slopes (assuming Durationseconds is the covariate)
RegressionGender <- aov(LeadershipData$ValuesDomain ~ LeadershipData$Durationinseconds*LeadershipData$Gender)
summary(RegressionGender)

RegressionEmployment <- aov(LeadershipData$ValuesDomain ~ LeadershipData$Durationinseconds*LeadershipData$Employment)
summary(RegressionEmployment)

#View the results of the homogeneity of regression slopes
ancova(ValuesDomain ~ Durationinseconds + Gender, data=LeadershipData)
ancova(ValuesDomain ~ Durationinseconds + Employment, data=LeadershipData)

#Run hypothesis/significance tests

#Factorial ANCOVAs
FactorialModel<-aov(ValuesDomain ~ Gender + Employment + Gender*Employment + Durationinseconds, data = LeadershipData)
Anova(FactorialModel, type="III")


#Plot results: hit enter when prompted, may be up to four prompts
plot(FactorialModel)




