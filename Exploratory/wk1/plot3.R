#R Script to create the third plot

#check if data sources are present
if (!file.exists("household_power_consumption.txt")) {
  url  = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
  dest = "powerconsumption.zip"
  meth = "internal"
  quit = TRUE
  mode = "wb"
  download.file(url, dest, meth, quit, mode)
  #Works on tested operating system (Windows 7). Please change values if needed.
  unzip("powerconsumption.zip")
  file.remove("powerconsumption.zip")
}

# Load in the required libraries
library(dplyr)
library(lubridate)

#Reads the dataset into R and converts it in a tbl_df
power <- read.csv("household_power_consumption.txt", sep = ";", 
                   col.names = c("Date", "Time", "Global_active_power",
                                 "Global_reactive_power", "Voltage", "Global_intensity",
                                 "Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
                   na.strings = "?", 
                   colClasses = c("character", "character", "numeric", "numeric",
                                   "numeric", "numeric", "numeric", "numeric",
                                  "numeric"),
                  nrows = 2075259)
power<- tbl_df(power)

#Mutates the date and time variables and converts them into the POSIXct class
power <- mutate(power, dtime = paste(Date, Time))
power$Date <- as.Date(power$Date, format = "%d/%m/%Y")
power$dtime <- as.POSIXct(power$dtime, format = "%d/%m/%Y %H:%M:%S")
power <- filter(power, Date > '2007-01-31' & Date < '2007-02-03')

#Plots the graph
plot(power$dtime, power$Sub_metering_1, type="l",
     xlab = " ", ylab = "Energy sub metering")

#Adds the lines
lines(power$dtime, power$Sub_metering_2, col = "red")
lines(power$dtime, power$Sub_metering_3, col = "blue")

#Plots the legend
legend("topright", lty = c(1,1,1), col = c("black","red", "blue"),
legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

#Writes the plot as a png file
dev.print(png, file = "plot3.png", width = 480, height = 480)
dev.off