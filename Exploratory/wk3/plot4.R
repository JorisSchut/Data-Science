#R Script to create the fourth plot

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

#filters the rows to be used; Coal combustion is captured by lookng first looking at
#all combustion sources in SCC.Level.One (the ones that include 'Combustion') and then
#looking for all coal related sources in the SCC.Level.Three (the ones that include 'Coal')
NEI <- select(NEI, SCC, Emissions, year)
SCC <- select(SCC, SCC, SCC.Level.One, SCC.Level.Three)%>%
        filter(SCC.Level.One == grep("Combustion", SCC$SCC.Level.One, value=TRUE) & 
                 SCC.Level.Three == grep("Coal", SCC$SCC.Level.Three, value=TRUE))%>%
                 select(SCC)

#filter for the SCC values (its also possible to first merge and than filter but this is
#faster). It however rturns a Warning but result are the same (verified this).
NEI <- filter(NEI, SCC==SCC$SCC)

#aggregate the total PM2.5 values for each year
Emissionsum <- aggregate(Emissions ~ year, data=NEI, sum, rm.na=TRUE)


#plot the graphic
qplot(year, Emissions, data=Emissionsum, 
      main="Total PM2.5 emissions in the US
      (coal combustion related sources) (1999-2008)",
      xlab="Years", ylab="Total PM2.5 emissions (ton)",
      geom=c("point",  "smooth"))
#this trows a couple of Warnings as a result of the added geom. This however, does not
#influence the plot.

#Writes the plot as a png file
dev.print(png, file = "plot4.png", width = 480, height = 480)
dev.off