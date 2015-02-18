#R Script to create the sixth plot

#loads libraries
library(dplyr)
library(ggplot2)

#loads the data files and converts them into the tbl_df class
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
NEI$fips <- as.string(NEI$fips)

years <- c(1999, 2002, 2005, 2008)
place <- c("24510", "06037")
placenames <- rep(c("Baltimore", "Los Angeles"), times=4)

#initializes classes
Emissionsum <- as.numeric()
NEI <- tbl_df(NEI)
SCC <- tbl_df(SCC)

NEI <- filter(NEI, fips==place[1] | fips==place[2]) %>%
        select(SCC, Emissions, year, fips)
SCC <- select(SCC, SCC, SCC.Level.One)  

NEI <- merge(NEI, SCC, by="SCC")%>%
        filter(SCC.Level.One == grep("Mobile", NEI$SCC.Level.One, value=TRUE))%>%
        select(Emissions, year, fips)

#loop to obtain the total PM2.5 values for each year
for(i in 1:length(years)){
  for(j in 1:length(place)){
    NEIyear <- filter(NEI, Emissions, year == years[i] &
                        fips == place[j])
    Emissionsum <- append(Emissionsum, sum(NEIyear$Emissions))
  }
}
Emissionsum <- append(Emissionsum, rep(years, each=2))
Emissionsum <- as.data.frame(matrix(Emissionsum, ncol = 2))
Emissionsum <- cbind(Emissionsum, placenames)

#plot the graphic
qplot(V2, V1, data=Emissionsum, color=placenames,
      main="Total PM2.5 emissions from mobile sources by year",
      xlab= "Years", ylab= "Total PM2.5 emissions (ton)",
      geom  =	c("point",	"smooth"))

#Writes the plot as a png file
dev.print(png, file = "plot6.png", width = 480, height = 480)
dev.off