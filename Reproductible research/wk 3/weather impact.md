---
title: "Changes in the impact of weather related events in the 1990's and 2000's"
author: "Joris Schut"
date: "Sunday, February 22, 2015"
output: html_document
---

##Synopstis
s

##Data processing
###Introduction
This section describes the steps that were taken to create the plots from which conclusions were derived. It starts by loading the data, after which it will process the data. After the processing steps the plots will be generated.

###Step 0: downloading the data
Before Starting with the actual processing the script checks if the datafile is present. If this is not the case, the script will download the data set that is used in this analysis. Note that some of the variables in this first block of code might have to be changed for it to work on your operating system. The settings presented in this document were tested on a system running Windows 7 as operating system.
```{r}
#Download the file (if not already done)
if (!file.exists("repdata-data-StormData.csv.bz2")) {
  url  = "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  dest = "repdata-data-StormData.csv.bz2"
  meth = "internal"
  quit = TRUE
  mode = "wb"
  download.file(url, dest, meth, quit, mode)
  #Works on tested operating system (Windows 7). Please change values if needed.
 } 
````

###Step1: load the libraries and load the dataset into R
In this section the libraries are loaded and the data set is directly loaded into R. The "dplyr" and "ggplot2" libraries were used in this analysis. Dplyr was used to perform select, filter and mutate some variables (see next steps) and ggplot2 was used for plotting the data.
```{r}
#Load the libraries
library(dplyr)
library(ggplot2)

#Reads the dataset into R
stormdata <- read.csv(bzfile("repdata-data-StormData.csv.bz2"), header=TRUE, 
                      nrows = 1125211, stringsAsFactor=FALSE, 
                      colClasses = "character", comment.char = "")
```

###Step 2: select the relevant data
In order to reduce the amount of RAM memory that is used by the computer, irrelevant data was discarded. The data that was kept is related to the Date of the measurement, the type of event and the outcomes (fatalities, injuries, property damage and crop damage) of the event.
```{r}
#Select the data relevant for research question 1 and 2
stormdata <- select(stormdata, BGN_DATE,EVTYPE, FATALITIES:CROPDMGEXP)
````

###Step3: editting the dates of the events

```{r}
#Transform th date colomn into dat format and filter the rows for years from 
#1990 to 2010.
stormdata$BGN_DATE <- sub(".*(....) 0.*", "\\1", stormdata$BGN_DATE,
                          perl=TRUE)
stormdata$BGN_DATE <- as.numeric(stormdata$BGN_DATE)

stormdata <- filter(stormdata, BGN_DATE>=1990 &
                     BGN_DATE<=2009)
```

```{r}
#Allocate the date to a decade (1990's or 2000's)
stormdata$BGN_DATE[grep("199", stormdata$BGN_DATE)] <- 1990
stormdata$BGN_DATE[grep("200", stormdata$BGN_DATE)] <- 2000
````

###Step4: Calculating damages

```{r}
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
````

```{r}
#Correcting the CROPDMGEXP column in a value
stormdata$CROPDMGEXP[grep("K|k", stormdata$CROPDMGEXP)] <- 3
stormdata$CROPDMGEXP[grep("M|m", stormdata$CROPDMGEXP)] <- 6
stormdata$CROPDMGEXP[grep("H|h", stormdata$CROPDMGEXP)] <- 2
stormdata$CROPDMGEXP[grep("B|b", stormdata$CROPDMGEXP)] <- 9
stormdata$CROPDMGEXP[grep("0", stormdata$CROPDMGEXP)] <- 0
stormdata$CROPDMGEXP[grep("0|2|3|6|7|8|9", stormdata$CROPDMGEXP,
                          invert=TRUE)] <- 0
````

```{r}
#Change class to intgers
stormdata$PROPDMGEXP <- as.integer(stormdata$PROPDMGEXP)
stormdata$PROPDMG <- as.integer(stormdata$PROPDMG)
stormdata$CROPDMG <- as.integer(stormdata$CROPDMG)
stormdata$CROPDMGEXP <- as.integer(stormdata$CROPDMGEXP)
stormdata$FATALITIES <- as.integer(stormdata$FATALITIES)
stormdata$INJURIES <- as.integer(stormdata$INJURIES)
````

```{r}
#Add variables containing the total property damage, total crop damage and
#total damage (property damage + crop damage)
stormdata <- mutate(stormdata, TOTALPRPDMG=PROPDMG*10^PROPDMGEXP)
stormdata <- mutate(stormdata, TOTALCROPDMG=CROPDMG*10^CROPDMGEXP)
stormdata <- mutate(stormdata, TOTALDMG=TOTALPRPDMG+TOTALCROPDMG)
```

###Step5: classifing the events

```{r}
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
```

###Step 6: aggregating the variables used for plotting

```{r}
#Aggregate the total number of fatalaities and injuries by eventtype
popfatbyevent<-aggregate(FATALITIES ~ EVTYPE + BGN_DATE, data=stormdata,
              sum, na.rm=TRUE)

popinjbyevent<-aggregate(INJURIES ~ EVTYPE + BGN_DATE, data=stormdata,
                          sum, na.rm=TRUE)
```

```{r}
#Aggregate the ecomomic impact (property damage + crop damage)
damagebyevent<-aggregate(TOTALDMG ~ EVTYPE + BGN_DATE, data=stormdata,
                          sum, na.rm=TRUE)
```

##Results
###Introduction

###Result 1: fatalities per decade

```{r}
#Plot fatalities per decade, injuries by decade and total damage by decade for
#types of disasters.
qplot(BGN_DATE, FATALITIES, data=popfatbyevent, colour=EVTYPE, group=EVTYPE,
      main="Fatalities per decade", xlab="Decade", ylab="Number of fatalities") + 
geom_line() + geom_point(size=4)
```

###Result 2: injuries per decade

```{r}
qplot(BGN_DATE, INJURIES, data=popinjbyevent, colour=EVTYPE, group=EVTYPE,
      main="Injuries per decade", xlab="Decade", ylab="Number of Injuries") + 
  geom_line() + geom_point(size=4)
```

###Result 3: total damage per decade

```{r}
qplot(BGN_DATE, TOTALDMG, data=damagebyevent, colour=EVTYPE, group=EVTYPE,
      main="Damage per decade", xlab="Decade", ylab="Total damage in $") + 
  geom_line() + geom_point(size=4)
```

##Conclusion and discussion
###Introduction

###Conclusion

###Discussion
