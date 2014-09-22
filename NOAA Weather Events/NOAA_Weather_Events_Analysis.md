# Effects of Weather Events
Jeferson Bisconde  
Sunday, September 21, 2014  

### Introduction

This project involves exploring the **U.S. National Oceanic and Atmospheric Administration's** (NOAA) storm database. 

This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any *fatalities*, *injuries*, and *property damage*.

The data came from [Weather Storm Data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2)

For more info, you can also visit: [National Weather Service Documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf)

===

### Synopsis

In this report, I aim to describe the costs of severe weather events in the United States between the years 1996 and 2011. We specifically obtained the data for the years 1996 until 2011 (the most complete data available). 

From these data, I found that overall, different weather events can cause different costs (**economic costs** or in **human population**). 

1) Tornado affects the total number of injuries the most.  
2) Excessive Heat affects the total number of fatalities the most.  
3) Flood affects the total amount of economic costs the most in terms of crop and property values.  

===

### Data Loading

In here, I simply downloaded the file to my Working directory and afterwards, read the file using `read.csv`

Afterwards, I looked at the data to see what it looks like.


```r
# Load the necessary libraries
library(dplyr); library(lubridate); library(car); library(ggplot2)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```


```r
# Read in the data
StormData <- read.csv(bzfile("stormData.csv.bz2"), sep=",", header=TRUE)

# Check what the data looks like
head(StormData, 100)
str(StormData)
summary(StormData)
```

===

### Data Processing

I had an assumption here that the best data to use comes from 1996 until 2011.

I reduced the Weather data so that my analysis wouldn't take as long. 

I only selected the needed columns to do my analysis, and only those columns with values **greater than 0**.


```r
# Subset the ones without any fatalities, injuries, property damage or crop damage
StormData <- subset(StormData, FATALITIES > 0 | INJURIES > 0 | PROPDMG > 0 | CROPDMG > 0)

# Since the data collection was completely overhauled in 1996, I will only be using the data from 1996 until 2011
# with the assumption that the year is in 'Begin Date'
StormData$DATE <- as.POSIXct(strptime(StormData$BGN_DATE, "%m/%d/%Y %H:%M:%S"))
StormData$YEAR <- year(StormData$DATE)
newStormData <- subset(StormData, YEAR >= 1996, 
                      select=c("YEAR", "EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP"))

# Change EVTYPE to uppercase
newStormData$EVTYPE <- toupper(newStormData$EVTYPE)
```


Afterwards, noticing that the **event types** has several inconsistencies, I decided to simplify the category values.


```r
# Function to manipulate EVTYPE later
uniqueEvents = function(dataset, string) {
        string <- as.character(toupper(string))
        uniqueList <- unique(dataset$EVTYPE[grepl(string, dataset$EVTYPE)])
        # Return the list
        return(uniqueList)
}

# Cleaning up EVTYPE
newStormData$EVTYPE[newStormData$EVTYPE %in% uniqueEvents(newStormData, "TORNADO")] <- "TORNADO"
newStormData$EVTYPE[newStormData$EVTYPE %in% uniqueEvents(newStormData, "TSTM")] <- "THUNDERSTORM"
newStormData$EVTYPE[newStormData$EVTYPE %in% uniqueEvents(newStormData, "THUNDERSTORM")] <- "THUNDERSTORM"
newStormData$EVTYPE[newStormData$EVTYPE %in% uniqueEvents(newStormData, "WIND")] <- "WIND"
newStormData$EVTYPE[newStormData$EVTYPE %in% uniqueEvents(newStormData, "FLOOD")] <- "FLOOD"
newStormData$EVTYPE[newStormData$EVTYPE %in% uniqueEvents(newStormData, "SNOW")] <- "SNOW"
newStormData$EVTYPE[newStormData$EVTYPE %in% uniqueEvents(newStormData, "FREEZE")] <- "FREEZE"
newStormData$EVTYPE[newStormData$EVTYPE %in% uniqueEvents(newStormData, "FOG")] <- "FOG"
newStormData$EVTYPE[newStormData$EVTYPE %in% uniqueEvents(newStormData, "FIRE")] <- "FIRE"
newStormData$EVTYPE[newStormData$EVTYPE %in% uniqueEvents(newStormData, "FREEZ")] <- "FREEZE"
newStormData$EVTYPE[newStormData$EVTYPE %in% uniqueEvents(newStormData, "STORM")] <- "STORM"
newStormData$EVTYPE[newStormData$EVTYPE %in% uniqueEvents(newStormData, "TYPHOON")] <- "HURRICANE"

# Change EVTYPE to factor
newStormData$EVTYPE <- as.factor(newStormData$EVTYPE)
```


The units in **crop** and **property** values also have some problems. I assumed that the meaning in **EXP** meant what I've written down in my code. I used the new units to calculate the new **crop** and **property** values.


```r
# Change the units to something meaningful for both Property and Crop Damage
unique(newStormData$CROPDMGEXP)
```

```
## [1] K   M B
## Levels:  ? 0 2 B k K m M
```

```r
unique(newStormData$PROPDMGEXP)
```

```
## [1] K   M B
## Levels:  - ? + 0 1 2 3 4 5 6 7 8 B h H K m M
```

```r
# Making the assumption that everything in [ ]EXP is in power of 10s. Also removing the ones that do not make sense
newStormData$CROPDMGEXP <- toupper(newStormData$CROPDMGEXP)
newStormData$PROPDMGEXP <- toupper(newStormData$PROPDMGEXP)

# Getting the new Crop Damage Values
newStormData$CROPDMGUNITS <- recode(newStormData$CROPDMGEXP, " ''=0; '0'=0; '?'=0; '2'=100; 'K'=1000; 'M'=1000000; 'B'=1000000000",
                                 as.factor.result=FALSE)
newStormData$PROPDMGUNITS <- recode(newStormData$PROPDMGEXP, " ''=0; '0'=0; '?'=0; '-'=0; '+'=0; 
                                 '1'=10; '2'=100; '3'=1000; '4'=10000; '5'=100000; '6'=1000000; '7'=10000000; '8'=100000000;
                                 'H'=100; 'K'=1000; 'M'=1000000; 'B'=1000000000",
                                 as.factor.result=FALSE)
# Calculate the new Damage Values
newStormData$CROPDMG <- newStormData$CROPDMG * newStormData$CROPDMGUNITS
newStormData$PROPDMG <- newStormData$PROPDMG * newStormData$PROPDMGUNITS
```

===

### Results

In here, I simply plotted the 3 categories to see the Top 5 Events. This should be pretty straightforward.

In Summary,  

        1) Injuries - in Hundreds (total number)  
        2) Fatalities - in Hundreds (total number)  
        3) Economic Costs - in Billions ($)  


```r
# Aggregate the data
stormSummary <- summarise(group_by(newStormData, EVTYPE),
                          total_Injuries = sum(INJURIES),
                          total_Fatalities = sum(FATALITIES),
                          total_Property_Dmg = sum(PROPDMG) / 1e9,
                          total_Crop_Dmg = sum(CROPDMG) / 1e9,
                          total_Dmg = total_Property_Dmg + total_Crop_Dmg)

# Top 5 for Human Injuries
injury_Summary <- head(arrange(stormSummary, desc(total_Injuries)), 5)

# Top 5 for Fatalities
fatality_Summary <- head(arrange(stormSummary, desc(total_Fatalities)), 5)

# Top 5 for Economic Cost
dmg_Summary <- head(arrange(stormSummary, desc(total_Dmg)), 5)

eventPlot = function(dataset, y_column, title, ylabel) {
        if (y_column==2) {
                event_Type_Plot <- ggplot(dataset, aes(x=reorder(EVTYPE, -total_Injuries), y=total_Injuries / 100))
        }
        else if (y_column==3) {
                event_Type_Plot <- ggplot(dataset, aes(x=reorder(EVTYPE, -total_Fatalities), y=total_Fatalities / 100))
        }
        else if (y_column==4) {
                event_Type_Plot <- ggplot(dataset, aes(x=reorder(EVTYPE, -total_Dmg), y=total_Dmg))
        }
        event_Type_Plot <- event_Type_Plot + geom_bar(aes(fill=EVTYPE), colour="black", stat="identity") +
                xlab("Event Types") + ylab(ylabel) + labs(title=title) +
                theme(axis.text.x=element_text(angle=90)) + 
                scale_fill_discrete(name="Event Types")
        # Return the plot
        return(event_Type_Plot)
}
```


The plot for each category.


```r
# Injury Plot
eventPlot(injury_Summary, y_column=2, title="Top 5 Event Types by Injuries", ylabel="Total Injuries (in Hundreds)")
```

![plot of chunk unnamed-chunk-7](./Reproducible_Research_Assignment2_files/figure-html/unnamed-chunk-71.png) 

```r
# Fatality Plot
eventPlot(fatality_Summary, y_column=3, title="Top 5 Event Types by Fatalities", ylabel="Total Fatalities (in Hundreds)")
```

![plot of chunk unnamed-chunk-7](./Reproducible_Research_Assignment2_files/figure-html/unnamed-chunk-72.png) 

```r
# Economic Costs Plot
eventPlot(dmg_Summary, y_column=4, title="Top 5 Event Types by Economic Costs", ylabel="Total Economic Costs (in Billions $)")
```

![plot of chunk unnamed-chunk-7](./Reproducible_Research_Assignment2_files/figure-html/unnamed-chunk-73.png) 

===
