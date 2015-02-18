#R Script to create the third plot

#loads libraries
library(dplyr)
library(ggplot2)

#loads the data files and converts them into the tbl_df class
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Initializes the years and SCCtypes to be used for the selection
years <- c(1999, 2002, 2005, 2008)
SCCtype <- c("Point", "Nonpoint", "Onroad", "Nonroad")
SCCtypes <- rep(SCCtype, times=4) 

#initializes classes
Emissionsum <- as.numeric()
NEI <- tbl_df(NEI)
SCC <- tbl_df(SCC)

#filters the rows to be used and merges the NEI and SCC variables by SCCtype
NEI <- filter(NEI, fips==24510) %>%
        select(SCC, Emissions, year)
SCC <- select(SCC, SCC, Data.Category)  

NEI <- merge(NEI, SCC, by="SCC")%>%
        filter(Data.Category == SCCtype)%>%
          select(Emissions, year, Data.Category)

#loop to obtain the total PM2.5 values for each year and each SCCtype
for(i in 1:length(years)){
  for(j in 1:length(SCCtype)){
    #Calculate the sum
    NEIyear <- filter(NEI, Emissions, year == years[i] & Data.Category == SCCtype[j])
    Emissionsum <- append(Emissionsum, sum(NEIyear$Emissions))
  }
}

#appends years and SCC types to the Emissionsum variable and ads colum names
Emissionsum <- append(Emissionsum, rep(years, each=4))
Emissionsum <- as.data.frame(matrix(Emissionsum, ncol = 2))
Emissionsum <- cbind(Emissionsum, SCCtypes)
colnames(Emissionsum) <- c("TotalPM2.5", "Year", "SCCtypes")

#plot the graphic
qplot(Year, TotalPM2.5, data=Emissionsum, color =SCCtypes,
      main="Total PM2.5 emissions by year", xlab= "Years",
      ylab= "Total PM2.5 emissions (ton)", geom=c("point",  "smooth"))

#Writes the plot as a png file
dev.print(png, file = "plot3.png", width = 480, height = 480)
dev.off