#R Script to create the first plot

#check if data sources are present
if (!file.exists("summarySCC_PM25.rds")) {
  url  = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  dest = "NEIdata.zip"
  meth = "internal"
  quit = TRUE
  mode = "wb"
  download.file(url, dest, meth, quit, mode)
  #Works on tested operating system (Windows 7). Please change values if needed.
  unzip("NEIdata.zip")
  file.remove("NEIdata.zip")
}

#loads libraries
library(dplyr)

#loads the data files and converts them into the tbl_df class
NEI <- readRDS("summarySCC_PM25.rds")

#select the variables to be used
NEI <- select(NEI, Emissions, year)

#aggregate the total PM2.5 values for each year
Emissionsum <- aggregate(Emissions ~ year, data=NEI, sum)

#plot the graphic
barplot(Emissionsum$Emissions, Emissionsum$year, xlab="Years",
        names.arg=Emissionsum$year, ylab="Total PM2.5 (ton)",
        main= "Total PM2.5 emissions in the US (1999-2008)")

#Writes the plot as a png file
dev.print(png, file = "plot1.png", width = 480, height = 480)
dev.off