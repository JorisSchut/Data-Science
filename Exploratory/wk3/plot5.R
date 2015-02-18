#R Script to create the fifth plot

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

NEI <- filter(NEI, fips==24510) %>%
        select(SCC, Emissions, year)
SCC <- select(SCC, SCC, SCC.Level.One)  

NEI <- merge(NEI, SCC, by="SCC")%>%
        filter(SCC.Level.One == grep("Mobile", NEI$SCC.Level.One, value=TRUE))%>%
        select(Emissions, year)

#loop to obtain the total PM2.5 values for each year
for(i in 1:length(years)){
    NEIyear <- filter(NEI, Emissions, year == years[i])
    Emissionsum <- append(Emissionsum, sum(NEIyear$Emissions))
}
Emissionsum <- append(Emissionsum, years)
Emissionsum <- as.data.frame(matrix(Emissionsum, ncol = 2))


#plot the graphic
qplot(V2, V1, data=Emissionsum,
      main="Total PM2.5 emissions in Baltimore from mobile sources by year",
      xlab= "Years", ylab= "Total PM2.5 emissions (ton)",
      geom  =  c("point",	"smooth"))

#Writes the plot as a png file
dev.print(png, file = "plot5.png", width = 480, height = 480)
dev.off