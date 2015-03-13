#Loads the data
data(mtcars)

#Sets the transmission as factors and names them
mtcars$am <- as.factor(mtcars$am)
levels(mtcars$am) <- c("Automatic", "Manual")

#Plots a boxplot
boxplot(mpg~am, data=mtcars, sub="Figure 1: fuel efficiency by transmission type",
        xlab="Transmission type", ylab="Fuel efficiency (mpg)")

#T-test of mpg by transmission type
automatic <- mtcars[mtcars$am == "Automatic",]
manual <- mtcars[mtcars$am == "Manual",]
t.test(automatic$mpg, manual$mpg)

#Simple linear regression
simplelm <- lm(mpg~am, data=mtcars)
print(simplelm)
summary(simplelm)

#Multivatiate regression
#Step algorithm
data(mtcars)
stepmodel = step(lm(data = mtcars, mpg ~ .),trace=0,steps=10000)
summary(stepmodel)

#Best approximation
bestfit <- lm(mpg~am + wt + qsec, data = mtcars)
print(bestfit)

#Comparing the 2 models
anova(simplelm, bestfit)

#checking for signs of non-normality
lm.stdres = rstandard(bestfit)

qqnorm(lm.stdres, main="", sub="Figure 2: Q-Q plot of the residuals",
       ylab="Standardized Residuals", xlab="Normal Scores") 
qqline(lm.stdres)

#Residual plots
par(mfrow = c(2,2))
plot(bestfit)