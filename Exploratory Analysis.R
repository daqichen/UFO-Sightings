##########################
## Exploratory Analysis ##
##########################

## Questions to Answer: 

# 1. What is the most common shape reported in UFO sightings?
  
# 2. Is there a pattern as the years go on in frequency of UFO sightings?


#________________________________________________________________________
# Required packages 

require(ggplot2)
require(lubridate)
require(maps)
require(mapdata)
require(data.table)


#________________________________________________________________________
# Read in Data

library(ggplot2)
library(tidyverse)
library(lubridate)
library(maps)
library(mapdata)
library(data.table)

ufo <- read.csv("scrubbed.csv")

#####################
## Date formatting ##
#####################
ufo$datetime <- mdy_hm(ufo$datetime) 
ufo$date.posted <- mdy(ufo$date.posted)
ufo$duration..seconds.<- as.numeric(ufo$duration..seconds.)

ufo$country <- as.factor(ufo$country) 
levels(ufo$country) #[1] ""   "au" "ca" "de" "gb" "us"
levels(ufo$country) <- c("NA","Australia","Canada","Germany","Great Britain","US")


#________________________________________________________________________
# Question 1:
## What is the most common shape reported in UFO sightings?

ufo %>%
  ggplot(aes(x=forcats::fct_infreq(shape),fill=shape))+
  stat_count(show.legend = FALSE)+
  ggtitle("What is the most common shape reported in UFO sightings?")+
  theme(axis.text.x = element_text(angle=70, vjust=0.5))+
  xlab("Shape") + ylab("Frequency")

#________________________________________________________________________
# Question 2:
## Is there a pattern as the years go on in frequency of UFO sightings?

ufo$year <- lubridate::year(ufo$datetime)
ufo_year <- subset(ufo, year>1940)
ufo_year %>%
  ggplot(aes(x=year,y=..count..,fill=..count..))+
  stat_count(show.legend = FALSE)+
  ggtitle("Is there a pattern as the years go on in frequency of UFO sightings?")+
  xlab("Year") + ylab("Number of Sightings")


#________________________________________________________________________
# Follow-up: 
## What about months and time-of-the-day, is there a pattern in the frequency of UFO sightings?

ufo$month <- lubridate::month(ufo$datetime, label = T, abbr = T)

ufo %>%
  ggplot(aes(x=month,y=..count..,fill=..count..))+
  stat_count(show.legend = FALSE)+
  ggtitle("What about month, is there a pattern in the frequency of UFO sightings?")+
  xlab("Month") + ylab("Number of Sightings")

