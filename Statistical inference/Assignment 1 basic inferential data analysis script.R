#1. Load the ToothGrowth data and perform some basic exploratory data analyses

#Load libraries
library(datasets)
library(dplyr)

#Read the data
data<-tbl_df(ToothGrowth)

#Exploratory data analysis
names(data)
dim(data)
head(data)

unique(data$supp)
unique(data$dose)

#2. Provide a basic summary of the data
summary (data)

#3. Use confidence intervals and/or hypothesis tests to compare tooth growth 
#by supp and dose. (Only use the techniques from class, even if there's other
#approaches worth considering)

#Create 2 data sets filtered by the values of the sub variable
OJ <- filter(data, supp=="OJ") %>%
      select(len)
VC <- filter(data, supp=="VC") %>%
      select(len)

x1 <- cbind(OJ, VC)

#Perform a two-sided t-test
t.test(x1, alternative="two.sided")

#Create 3 data sets filtered by the values of the dose variable
dose0.5 <- filter(data, dose==0.5)%>%
            select(len)
dose1.0 <- filter(data, dose==1.0)%>%
            select(len)
dose2.0 <- filter(data, dose==2.0)%>%
            select(len)

x2 <- cbind(dose0.5, dose1.0, dose2.0)

#Perform a two-sided t-test
t.test(x2[,1], x2[,2], alternative="two.sided")
t.test(x2[,1], x2[,3], alternative="two.sided")
t.test(x2[,2], x2[,3], alternative="two.sided")

#Test if variances are equal
print(var(x1[,1])==var(x1[,2]))
print(var(x2[,1])==var(x2[,2]))
print(var(x2[,1])==var(x2[,3]))
print(var(x2[,2])==var(x2[,3]))