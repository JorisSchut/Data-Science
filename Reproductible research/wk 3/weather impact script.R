#Download the file (if not already done)
if (!file.exists("repdata-data-StormData.csv.bz2")) {
  url  = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  dest = "repdata-data-StormData.csv.bz2"
  meth = "internal"
  quit = TRUE
  mode = "wb"
  download.file(url, dest, meth, quit, mode)
  #WOrks on tested operating system (Windows 7). Please change values if needed.
 } 

#Load the libraries
library(dplyr)
library(ggplot2)

#Reads the dataset into R
stormdata <- read.csv(bzfile("repdata-data-StormData.csv.bz2"), header=TRUE, 
                      nrows = 1125211, stringsAsFactor=FALSE, 
                      colClasses = "character", comment.char = "")

#Select the data relevant for research question 1 and 2
stormdata <- select(stormdata, BGN_DATE,EVTYPE, FATALITIES:CROPDMGEXP)

#Transforem th date colomn into dat format and filter the rows for years from 1990
#to 2010.
stormdata$BGN_DATE <- sub(".*(....) 0.*", "\\1", stormdata$BGN_DATE,
                          perl=TRUE)
stormdata$BGN_DATE <- as.numeric(stormdata$BGN_DATE)

stormdata <- filter(stormdata, BGN_DATE>=1990 &
                     BGN_DATE<=2009)

#Correcting the PROPDMGEXP column in a value 
stormdata$PROPDMGEXP[grep("K|k", stormdata$PROPDMGEXP)] <- 3
stormdata$PROPDMGEXP[grep("M|m", stormdata$PROPDMGEXP)] <- 6
stormdata$PROPDMGEXP[grep("H|h", stormdata$PROPDMGEXP)] <- 2
stormdata$PROPDMGEXP[grep("B|b", stormdata$PROPDMGEXP)] <- 9
stormdata$PROPDMGEXP[grep("0", stormdata$PROPDMGEXP)] <- 0
stormdata$PROPDMGEXP[grep("1", stormdata$PROPDMGEXP)] <- 1
stormdata$PROPDMGEXP[grep("2", stormdata$PROPDMGEXP)] <- 2
stormdata$PROPDMGEXP[grep("3", stormdata$PROPDMGEXP)] <- 3
stormdata$PROPDMGEXP[grep("4", stormdata$PROPDMGEXP)] <- 4
stormdata$PROPDMGEXP[grep("5", stormdata$PROPDMGEXP)] <- 5
stormdata$PROPDMGEXP[grep("6", stormdata$PROPDMGEXP)] <- 6
stormdata$PROPDMGEXP[grep("7", stormdata$PROPDMGEXP)] <- 7
stormdata$PROPDMGEXP[grep("8", stormdata$PROPDMGEXP)] <- 8
stormdata$PROPDMGEXP[grep("0|1|2|3|4|5|6|7|8|9", stormdata$PROPDMGEXP,
                          invert=TRUE)] <- 0

#Correcting the CROPDMGEXP column in a value
stormdata$CROPDMGEXP[grep("K|k", stormdata$CROPDMGEXP)] <- 3
stormdata$CROPDMGEXP[grep("M|m", stormdata$CROPDMGEXP)] <- 6
stormdata$CROPDMGEXP[grep("H|h", stormdata$CROPDMGEXP)] <- 2
stormdata$CROPDMGEXP[grep("B|b", stormdata$CROPDMGEXP)] <- 9
stormdata$CROPDMGEXP[grep("0", stormdata$CROPDMGEXP)] <- 0
stormdata$CROPDMGEXP[grep("0|2|3|6|7|8|9", stormdata$CROPDMGEXP,
                          invert=TRUE)] <- 0

#Change class to intgers
stormdata$PROPDMGEXP <- as.integer(stormdata$PROPDMGEXP)
stormdata$PROPDMG <- as.integer(stormdata$PROPDMG)
stormdata$CROPDMG <- as.integer(stormdata$CROPDMG)
stormdata$CROPDMGEXP <- as.integer(stormdata$CROPDMGEXP)
stormdata$FATALITIES <- as.integer(stormdata$FATALITIES)
stormdata$INJURIES <- as.integer(stormdata$INJURIES)

#Add variables containing the total property damage, total crop damage and
#total damage (property damage + crop damage)
stormdata <- mutate(stormdata, TOTALPRPDMG=PROPDMG*10^PROPDMGEXP)
stormdata <- mutate(stormdata, TOTALCROPDMG=CROPDMG*10^CROPDMGEXP)
stormdata <- mutate(stormdata, TOTALDMG=TOTALPRPDMG+TOTALCROPDMG)

#Allocate the date to a decade (1990's or 2000's)
stormdata$BGN_DATE[grep("199", stormdata$BGN_DATE)] <- 1990
stormdata$BGN_DATE[grep("200", stormdata$BGN_DATE)] <- 2000

#Transform the EVTYPE column in 11 different categories
stormdata$EVTYPE <- toupper(stormdata$EVTYPE)

stormdata$EVTYPE[grep("COLD|HAIL|ICE|SNOW|BLIZZARD|FREEZ|GLARE", stormdata$EVTYPE)] <- "Cold"
stormdata$EVTYPE[grep("STORM|HURRICANE|WIND", stormdata$EVTYPE)] <- "Storm"
stormdata$EVTYPE[grep("HEAT|FIRE|DROUGHT", stormdata$EVTYPE)] <- "Heat"
stormdata$EVTYPE[grep("THUNDERSTORM|TSTM", stormdata$EVTYPE)] <- "Thunderstorm"
stormdata$EVTYPE[grep("FLOOD|FLD", stormdata$EVTYPE)] <- "Flood"
stormdata$EVTYPE[grep("SLIDE|AVALANCE", stormdata$EVTYPE)] <- "Slide"
stormdata$EVTYPE[grep("TORNADO", stormdata$EVTYPE)] <- "Tornado"
stormdata$EVTYPE[grep("RAIN", stormdata$EVTYPE)] <- "Rain"
stormdata$EVTYPE[grep("RIP CURRENT", stormdata$EVTYPE)] <- "Rip current"
stormdata$EVTYPE[grep("LIGHTNING", stormdata$EVTYPE)] <- "Lightning"
stormdata$EVTYPE[grep("Cold|Storm|Heat|Thunderstorm|Slide|Tornado|Rain|Rip current|Lightning",
                      stormdata$EVTYPE, invert=TRUE)] <- "Misc"
stormdata$EVTYPE <- factor(stormdata$EVTYPE)



#Aggregate the total number of fatalaities and injuries by eventtype
popfatbyevent<-aggregate(FATALITIES ~ EVTYPE + BGN_DATE, data=stormdata,
              sum, na.rm=TRUE)
popinjbyevent<-aggregate(INJURIES ~ EVTYPE + BGN_DATE, data=stormdata,
                          sum, na.rm=TRUE)

#Aggregate the ecomomic impact (property damage + crop damage)
damagebyevent<-aggregate(TOTALDMG ~ EVTYPE + BGN_DATE, data=stormdata,
                          sum, na.rm=TRUE)

#Plot fatalities per decade, injuries by decade and total damage by decade for
#types of disasters.
qplot(BGN_DATE, FATALITIES, data=popfatbyevent, colour=EVTYPE, group=EVTYPE,
      main="Fatalities per decade", xlab="Decade", ylab="Number of fatalities") + 
geom_line() + geom_point(size=4)

qplot(BGN_DATE, INJURIES, data=popinjbyevent, colour=EVTYPE, group=EVTYPE,
      main="Injuries per decade", xlab="Decade", ylab="Number of Injuries") + 
  geom_line() + geom_point(size=4)

qplot(BGN_DATE, TOTALDMG, data=damagebyevent, colour=EVTYPE, group=EVTYPE,
      main="Damage per decade", xlab="Decade", ylab="Total damage in $") + 
  geom_line() + geom_point(size=4)