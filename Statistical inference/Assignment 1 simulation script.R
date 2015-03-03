#1. Show the sample mean and compare it to the theoretical mean of the distribution.

#Set parameters
set.seed(111)
n = 40
lambda = 0.2
runs = 10000

#Determine the experimental mean
expvalues <- rexp(n, lambda)
expmean1 = mean(expvalues)

hist(expvalues, main="Histogram of the random sample", xlab="Sample values")
print(expmean1)

#Calcualte the theoretical mean
theomean1 = 1/lambda
print(theomean1)

#Determine the absolute difference between the theoretical and experimental mean
diffmean1 = abs(theomean1 - expmean1)
print(diffmean1)

#2. Show how variable the sample is (via variance) and compare it to the
#theoretical variance of the distribution

#Determine the experiental variance
expvar1 = var(expvalues)
print(expvar1)

#Calculate the theoretical variance 
#(http://en.wikipedia.org/wiki/Exponential_distribution#Mean.2C_variance.2C_moments_and_median)
theovar1 = 1/(lambda^2)
print(theovar1)

#Determine the absolute difference between the theoretical and experimental mean
diffvar = abs(theovar1 - expvar1)
print(diffvar1)

#3 Show that the distribution is approximately normal
#Run 1000 examples
mns = NULL
for (i in 1 : runs){
  mns = append(mns, mean(rexp(n, lambda)))
}


#Calculate theoretical values for the average and the variance
theomean2 <- theomean1
theovar2 <- (1/(lambda^2))/n
print(theomean2)
print(theovar2)

#Create a an normal approximation
binwidth <- 0.5 
x <- seq(-4, 4, length=100)*theovar2 + theomean2
hx <- binwidth*runs*dnorm(x, mean=theomean2, sd=sqrt(theovar2))

#Plot the histogram of the experiments and the normal approximation
hist(mns, breaks=seq(1,9,by=binwidth),main="Histogram of the average samples",
     xlab="Sample average")
lines(x, hx)

histmns <- hist(mns, breaks=seq(1,9,by=binwidth), plot=FALSE)
fmns<- histmns$density
pmns <- pnorm(fmns, mean=theomean2, sd=sqrt(theovar2))
qmns <- qnorm(pmns, mean=theomean2, sd=sqrt(theovar2))

px <- pnorm(hx/(binwidth*runs), mean=theomean2, sd=sqrt(theovar2))
qx <- qnorm(px, mean=theomean2, sd=sqrt(theovar2))

plot(qx, qx, main="Normal approcimation of experimental data", xlab="quartile",
     ylab="quartile")
lines(qmns, qmns)
