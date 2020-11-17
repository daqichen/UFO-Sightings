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

### Questions to answer:
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

### Follow-up Questions
* What about months, is there a pattern in the frequency of UFO sightings?
```{r}
ufo$month <- lubridate::month(ufo$datetime, label = T, abbr = T)
ufo %>%
  ggplot(aes(x=month,y=..count..,fill=..count..))+
  stat_count(show.legend = FALSE)+
  ggtitle("What about month, is there a pattern in the frequency of UFO sightings?")+
  xlab("Month") + ylab("Number of Sightings")
```
![image](https://user-images.githubusercontent.com/55526292/95642526-08cd7280-0a77-11eb-89da-4bf6359dc30a.png)


* Is there an improved way to identify a possible pattern in duration of sightings? 
```{r}
UFOsightings <- fread("scrubbed.csv")
UFOsightings$`duration (seconds)` <- as.numeric(UFOsightings$`duration (seconds)`)

ggplot(UFOsightings[`duration (seconds)`<10000],aes(`duration (seconds)`))+
         stat_density(fill="blue")+scale_x_log10()+ggtitle("UFO Duration")
```
![image](https://user-images.githubusercontent.com/55526292/95642758-79c15a00-0a78-11eb-94f9-825f240356b0.png)


<details>
  <summary>Summary</summary>
  <p> We found a lot of interesting information from our initial questions. First of all we found that the state of California had by far the most UFO sightings in the United States, over twice as many as the next highest state, Washington. We found that by far the most common 'shape' reported was just a 'light' in the sky, which is not exactly a shape so that is a bit unusual.  One surprising thing is the wide range in duration of sightings, anywhere from less than a second to multiple hours, with a mean of about 4 and a half minutes. We also discovered that there was considerable increase in sightings starting in the 1990s and has trended up since then. The questions that were most helpful in creating follow-up questions were "What is the distribution of duration of UFO sightings?", "Is there a change in UFO sightings during specific seasons of the year?" and "What is the frequency of sightings by time of day?". </p>
  <p> Our first follow up question that we further investigated was "What about months, is there a pattern in the frequency of UFO sightings?". We further investigated this question because our initial question looking at frequency by season left out a lot of the sightings and we were interested in an accurate display of sightings by time of the year. To investigate this we created a bar graph with frequency of sightings by month. This graph shows that overall the sightings are spread out across the calendar year but there is a spike in the summer months. We thought when looking at it by season that winter may have the most sightings but this analysis disproved that. This could be because people tend to spend more time outside during the summer. Our second follow up question that we further investigated was "Is there an improved way to identify a possible pattern in duration of sightings?". We wanted to further investigate duration because there was a surprisingly wide range for duration that we found from our initial question. To investigate this question we created a density graph. This graph shows that when the large duration outliers are taken away the durations are a lot more concentrated around 0.02 to 0.06 seconds. This shows us that the majority of sightings were very brief which was not obvious from our initial investigation.</p>
</details>    


<details>
  <summary>Next Steps</summary>
  <p> EDA Meeting Oct 16 </p>
  <p> P3	Final Report	Sakai	Nov 17 (11:59PM)</p>
  <p> P4	Final Presentation	Sakai + Class	Nov 12 or Nov 17 (11:30AM-12:45PM)</p>
</details>


