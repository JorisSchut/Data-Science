#R Script to create the fourth plot

#loads libraries
library(dplyr)
library(ggplot2)

#loads the data files and converts them into the tbl_df class
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

years <- c(1999, 2002, 2005, 2008)

#initializes classes
Emissionsum <- as.numeric()
NEI <- tbl_df(NEI)
SCC <- tbl_df(SCC)

NEI <- select(NEI, SCC, Emissions, year)
SCC <- select(SCC, SCC, SCC.Level.One, SCC.Level.Three)%>%
        filter(SCC.Level.One == grep("Combustion", SCC$SCC.Level.One, value=TRUE) & 
                 SCC.Level.Three == grep("Coal", SCC$SCC.Level.Three, value=TRUE))%>%
                 select(SCC)

NEI <- filter(NEI, SCC==SCC$SCC)

#loop to obtain the total PM2.5 values for each year
for(i in 1:length(years)){
  #Calculate the sum
   NEIyear <- filter(NEI, Emissions, year == years[i])
   Emissionsum <- append(Emissionsum, sum(NEIyear$Emissions))
}
Emissionsum <- append(Emissionsum, years)
Emissionsum <- as.data.frame(matrix(Emissionsum, ncol = 2))

#plot the graphic
qplot(V2, V1, data=Emissionsum, 
      main="Total PM2.5 emissions from coal related sources by year",
      xlab="Years", ylab="Total PM2.5 emissions (ton)",
      geom=c("point",  "smooth"))

#Writes the plot as a png file
dev.print(png, file = "plot4.png", width = 480, height = 480)
dev.off