#R Script to create the first plot

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
        ylab="Total PM2.5 (ton)",
        main= "Total PM2.5 emissions in the US (1999-2008)")

#Writes the plot as a png file
dev.print(png, file = "plot1.png", width = 480, height = 480)
dev.off