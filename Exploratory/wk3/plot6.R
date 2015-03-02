#R Script to create the sixth plot

#check if data sources are present
if (!file.exists("summarySCC_PM25.rds") | !file.exists("Source_Classification_Code.rds")) {
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
library(ggplot2)

#loads the data files and converts them into the tbl_df class
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
NEI$fips <- as.string(NEI$fips)

#filter for Baltimore and LA and select the colums to be used
NEI <- filter(NEI, fips=="24510" | fips=="06037") %>%
        select(SCC, Emissions, year, fips)
SCC <- select(SCC, SCC, SCC.Level.One)  

#merge the datasets and keep the ones that have 'Mobile in the SCC.Level.One variable
#as all mobile polluters are motor vehicles (includes trucks and other vehicles other
#than cars)
NEI <- merge(NEI, SCC, by="SCC")%>%
        filter(SCC.Level.One == grep("Mobile", NEI$SCC.Level.One, value=TRUE))%>%
        select(Emissions, year, fips)

#aggregate the total PM2.5 values for each year
Emissionsum <- aggregate(Emissions ~ year + fips, data=NEI, sum, rm.na=TRUE)

#change some names
colnames(Emissionsum) <- c("year","Placename","Emissions")
Emissionsum$placename[grep("24510", Emissionsum$Placename)] <- "Baltimore"
Emissionsum$placename[grep("06037", Emissionsum$Placename)] <- "Los Angeles"

#plot the graphic
qplot(year, Emissions, data=Emissionsum, color=Placename,
      main="Total PM2.5 emissions from mobile sources (1999-2008)",
      xlab= "Years", ylab= "Total PM2.5 emissions (ton)",
      geom  =	c("point",	"smooth"))
#this trows a couple of Warnings as a result of the added geom. This however, does not
#influence the plot.

#Writes the plot as a png file
dev.print(png, file = "plot6.png", width = 480, height = 480)
dev.off