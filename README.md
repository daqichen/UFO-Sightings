# UFO Sightings Report
## STOR 320 Final Project, Written in R
### Authors: Daqi Chen, Erica Spruell, Yiran Li, Brent Tompkins

## Progress Tracker
- [x] Project Proposal
- [x] Exploratory Analysis
- [ ] Final Written Paper
- [ ] Final Presentation

## Exploratory Analysis
#### Packages Used:
```r
library(ggplot2)
library(tidyverse)
library(lubridate)
library(maps)
library(mapdata)
library(data.table)
```

#### Questions to answer:
1. What is the most common shape reported in UFO sightings?
```r
ufo %>%
  ggplot(aes(x=forcats::fct_infreq(shape),fill=shape))+
  stat_count(show.legend = FALSE)+
  ggtitle("What is the most common shape reported in UFO sightings?")+
  theme(axis.text.x = element_text(angle=70, vjust=0.5))+
  xlab("Shape") + ylab("Frequency")
```
* The most common shape in UFO sightings is light.

![image](https://user-images.githubusercontent.com/55526292/95629032-2b4a9600-0a4d-11eb-8c16-700a79681f1b.png)


2. Is there a pattern as the years go on in frequency of UFO sightings?
```r
ufo$year <- lubridate::year(ufo$datetime)
ufo_year <- subset(ufo, year>1940)
ufo_year %>%
  ggplot(aes(x=year,y=..count..,fill=..count..))+
  stat_count(show.legend = FALSE)+
  ggtitle("Is there a pattern as the years go on in frequency of UFO sightings?")+
  xlab("Year") + ylab("Number of Sightings")
```
* As the years go on, there is a general upward trend in frequency of UFO sightings.

![image](https://user-images.githubusercontent.com/55526292/95629157-64830600-0a4d-11eb-8aba-bc9550cfc116.png)




