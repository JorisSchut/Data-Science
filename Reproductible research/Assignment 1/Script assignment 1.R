library(lattice)
library(dplyr)

#Download the file (if not already done)
if (!file.exists("activity.csv")) {
  url  = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  dest = "activity.zip"
  meth = "internal"
  quit = TRUE
  mode = "wb"
  download.file(url, dest, meth, quit, mode)
  #Works on tested operating system (Windows 7). Please change values if needed.
  unzip("activity.zip")
  } 

activity <- read.csv("activity.csv", na.strings="NA")
activity$date <- as.Date(activity$date, "%Y-%m-%d")
activity <- tbl_df(activity)

stepstotalbydate <- aggregate(steps ~ date, data=activity, sum, na.rm = TRUE)

hist(stepstotalbydate$steps, main="Distribution of steps per day", xlab="Steps per day")

mean_steps_per_day <- mean(stepstotalbydate$steps, na.rm=TRUE)
median_steps_per_day <- median(stepstotalbydate$steps, na.rm=TRUE)
print(paste("Average number of steps per day:", mean_steps_per_day, sep=" "))
print(paste("Median of steps per day:", median_steps_per_day, sep=" "))



stepsmeanbyinterval <- aggregate(steps ~ interval, data=activity, mean, na.rm = TRUE)

plot(stepsmeanbyinterval$interval, stepsmeanbyinterval$steps,
     type="l", main="Average number of steps per interval",
     xlab="Interval", ylab="Number of steps")

a <- max(stepsmeanbyinterval$steps)
a <- stepsmeanbyinterval$steps == a
interval_maxsteps <- stepsmeanbyinterval$interval[a]
rm(a)
print(paste("Interval containing the highest number of steps:",
            interval_maxsteps, sep=" "))


numberNA <- sum(is.na(activity))
print(paste("total number of NA's:", numberNA, sep=" "))
numberNAsteps <- sum(is.na(activity$steps))
print(paste("total number of NA's in the 'steps' column:", numberNAsteps, sep=" "))
if(numberNA == numberNAsteps){
  print("All NA's are in the steps column")
}

newsteps <- as.numeric()
for(i in 1:length(activity$steps)){
  if(is.na(activity$steps[i] == TRUE)){
    newsteps <- append(newsteps, stepsmeanbyinterval$steps[i])
  }
  else{
    newsteps <- append(newsteps, activity$steps[i])
  }
}

newactivity <- as.data.frame(matrix(newsteps, ncol=1))
names(newactivity)[1]<-paste("steps")
newactivity <- cbind(newactivity, activity[,2:3])
newactivity$date <- as.Date(newactivity$date, "%Y-%m-%d")

newstepstotalbydate <- aggregate(steps ~ date, data=newactivity, sum, na.rm = TRUE)

hist(newstepstotalbydate$steps, main="Distribution of steps per day", xlab="Steps per day")

newmean_steps_per_day <- mean(newstepstotalbydate$steps, na.rm=TRUE)
newmedian_steps_per_day <- median(newstepstotalbydate$steps, na.rm=TRUE)
print(paste("Average number of steps per day:", newmean_steps_per_day, sep=" "))
print(paste("Median of steps per day:", newmedian_steps_per_day, sep=" "))

difmean <- abs(mean_steps_per_day-newmean_steps_per_day)
difmedian <- abs(median_steps_per_day-newmedian_steps_per_day)
print(paste("Difference in means between old and new dataset:",
            difmean, sep=" "))
print(paste("Difference in median between old and new dataset:",
            difmedian, sep=" "))



days <- weekdays(newactivity$date)

for(i in 1:length(days)){
  if(days[i]=="Saturday" | days[i]=="Sunday"){
  days[i] <- "weekend"
  }
  else{
    days[i] <- "weekday"
  }
}

days <- factor(days)
newactivity <- cbind(newactivity, days)
names(newactivity)[4]<-paste("type_of_day")

stepsmeanbydaytype <- aggregate(steps ~ interval + type_of_day, data=newactivity,
                                mean, na.rm = TRUE)

xyplot(steps ~ interval | type_of_day, stepsmeanbydaytype, type="l", layout=c(1, 2), 
       xlab="Interval", ylab="Number of steps")
