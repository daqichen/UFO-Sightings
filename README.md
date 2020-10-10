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
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lubridate)
library(data.table)
library(rmarkdown)
library(tibble)
library(tidyr)
library(readxl)
```

#### Questions to answer:
##### by Daqi Chen
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

##### by Erica Spruill
3. Is there a change in UFO sightings during specific seasons of the year?
```{r}
ufo$Date <- Date
 months <- as.numeric(format(as.Date(UFOsightings$Date, '%m/%d/%Y'), '%m'))
 indx <- setNames( rep(c('winter', 'spring', 'summer',
                   'fall'),each=3), c(12,1:11))
ufo$Season <- unname(indx[as.character(months)])

UFOSeason <- ufo %>%
  group_by(Season) %>%
  summarise(frequency = n())
ggplot(UFOSeason, aes(x = Season, y = frequency)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = frequency), vjust = -0.3)
```
![image](https://user-images.githubusercontent.com/55526292/95641991-b63e8700-0a73-11eb-98db-35b962206f0b.png)


4. What is the distribution of duration of UFO sightings?
```{r}
is.numeric("duration..seconds.")
D <- c(-Inf, 60, 1800, Inf)
names <- c("Less than 60 seconds", "Between 60 seconds and 30 Minutes","Longer than 30 minutes")

UFO2 <- ufo %>%
  group_by(duration..seconds.) %>%
  summarise(frequency = n())
ggplot(UFO2, aes(x = duration..seconds., y = frequency)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = frequency), vjust = -0.3) 
```
![image](https://user-images.githubusercontent.com/55526292/95642081-3c5acd80-0a74-11eb-85a0-fd092e234e82.png)

##### by Yiran Li
5, How many different countries were represented in the dataset and how many ufos were found in each of them?
```{r}
ufo %>%
  ggplot(aes(x=forcats::fct_infreq(country),fill=country))+
  stat_count(show.legend = FALSE)+
  ggtitle("How many different countries were represented in the dataset and how many ufos were found in each of them?")+
  theme(axis.text.x = element_text(angle=70, vjust=0.5))+
  xlab("Country") + ylab("Frequency")
```
![image](https://user-images.githubusercontent.com/55526292/95642110-67452180-0a74-11eb-9c1b-7db1112e3356.png)

6, How many different states were represented?
```{r}
ufo %>%
  ggplot(aes(x=forcats::fct_infreq(state),fill=state))+
  stat_count(show.legend = FALSE)+
  ggtitle("How many different states were represented?")+
  theme(axis.text.x = element_text(angle=70, vjust=0.5))+
  xlab("State") + ylab("Frequency")
```
![image](https://user-images.githubusercontent.com/55526292/95642150-93f93900-0a74-11eb-999f-062ed54bd949.png)

##### by Brent Tompkins
7, What is the average duration of a UFO sighting in seconds?
```{r}
duration <- as.factor(UFOsightings$duration..seconds.)
duration1 <- as.numeric(duration)
mean(duration1)
```
![image](https://user-images.githubusercontent.com/55526292/95642172-b8edac00-0a74-11eb-9c54-cd76ddbb0384.png)

8, What is the frequency of sightings by time of day?
```{r}
UFOsightings %>%
  ggplot(aes(x=Time,y=..count..,fill=..count..))+ stat_count(show.legend = FALSE)+ ggtitle("Frequency of UFO Sightings by Time of Day")+ xlab("Time")+ ylab("Number of Sightings")
```
![image](https://user-images.githubusercontent.com/55526292/95642183-d3c02080-0a74-11eb-9fd7-060e4a6542df.png)


<details>
  <summary>Next Steps</summary>
  <p> EDA Meeting Oct 16 </p>
  <p> P3	Final Report	Sakai	Nov 17 (11:59PM)</p>
  <p> P4	Final Presentation	Sakai + Class	Nov 12 or Nov 17 (11:30AM-12:45PM)</p>
</details>


