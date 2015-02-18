#R Script to create the first plot

#loads libraries
library(dplyr)

#loads the data files and converts them into the tbl_df class
NEI <- readRDS("summarySCC_PM25.rds")

#initializes classes
Emissionsum <- as.numeric()
NEI <- tbl_df(NEI)
years <- c(1999, 2002, 2005, 2008)

NEI <- select(NEI, Emissions, year)

#loop to obtain the total PM2.5 values for each year
for(i in 1:length(years)){
#Calculate the sum
NEIyear <- filter(NEI, Emissions, year == years[i])
Emissionsum <- append(Emissionsum, sum(NEIyear$Emissions))
}

#plot the graphic
barplot(Emissionsum, names.arg=years, xlab="Years", ylab="Total PM2.5 (ton)",
        main= "Total PM2.5 in different years")

#Writes the plot as a png file
dev.print(png, file = "plot1.png", width = 480, height = 480)
dev.off