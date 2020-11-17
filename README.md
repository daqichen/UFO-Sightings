# UFO Sightings Report
## STOR 320 Final Project, Written in R
### Authors: Daqi Chen, Erica Spruell, Yiran Li, Brent Tompkins

## Progress Tracker
- [x] Project Proposal
- [x] Exploratory Analysis
- [x] Final Written Paper
- [x] Final Presentation

# Final Report
## INTRODUCTION

Unidentified Flying Object (UFO) sightings are a rare phenomenon that few people get to experience in their lives. Our dataset has information on over 80,000 reported UFO sightings during the last century. The dataset includes the following information from each sighting; the date of the sighting, the time of day, the duration of the sighting, the location of the sighting, and the shape of the UFO. The data set includes data from mostly the United States but there are some reports from other countries. 

The theme of our analysis pertains to the general trend of UFO sightings throughout various time periods. In our initial data exploration, we were able to get a better idea of how the variables relate to one another. This helped us narrow down our focus to two questions related to the patterns in UFO Sightings across various time periods.

### Question1: Are there any trends in the frequency of UFO sightings during different seasons of the year, and different months of the year?

### Question2: Is there a trend in the time of day of UFO sightings and is there a trend in sightings by year over the last century? 

These questions further explore the data set, searching for trends that might highlight patterns in UFO sightings based on month, season, year, and time of day. Finding and highlighting patterns in the dataset can give us useful information to develop new questions about UFO sightings. Our analysis will help you determine the time when you are most likely to experience the phenomenon of seeing a UFO.

### Packages Used:
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

## DATA

### Describe data

The dataset we chose to investigate contains over 80,000 reports of Unidentified Flying Object (UFO) sightings throughout the world from the year 1998 to 2014. The dataset was collected by the NAtional UFO Reporting Center and has been scrubbed of observations that contain no time frame, no location, or errors in recording. There are 11 columns in the UFO sightings dataset. The first column, “datetime,” lists the time and date of each observation in the format of month/day/year hour:minute. There are 4 columns identifying the location of each sighting: city, state, country and latitude. The “shape” column describes the object shape as “light,” “triangle,” “circle,” “fireball,” or “unknown.” The “duration” column specifies how long the sighting was in hours, minutes and seconds. There is also a “comments” column with descriptive notes about each observation.  There is also a “date posted” column specifying when the observation was recorded with the National UFO Reporting Center. 

```{r, echo=FALSE}
ufo <- read.csv("scrubbed.csv")
UFOsightings = ufo
```

### Descriptive Table

#### What are the variables in this dataset?

```{r, echo=FALSE}
#####################
## Date formatting ##
#####################
ufo = UFOsightings
ufo$datetime <- mdy_hm(ufo$datetime) 
ufo$date.posted <- mdy(ufo$date.posted)

ufo$year <- year(ufo$datetime)
glimpse(ufo)

```
![image](https://user-images.githubusercontent.com/55526292/99347624-869a5000-2865-11eb-8bf9-fb01028770d8.png)

#### What are the different shapes of UFOs?

```{r, echo=FALSE}
shapesUFO <- ufo %>% 
  count(shape, sort=TRUE)
head(tibble(shapesUFO))
```
![image](https://user-images.githubusercontent.com/55526292/99347648-974ac600-2865-11eb-9502-fd283ecda71e.png)

#### What are the different countries these reports are from?

```{r, echo=FALSE}
countriesUFO <- ufo %>% 
  count(country, sort=TRUE)
countriesUFO$country <- as.factor(countriesUFO$country)
levels(countriesUFO$country) #[1] "au"           "ca"           "de"           "gb"           "other region" "us"
levels(countriesUFO$country) <- c("Australia","Canada","Germany","Great Britain","Other Regions","US")
head(tibble(countriesUFO))
```
![image](https://user-images.githubusercontent.com/55526292/99347691-aa5d9600-2865-11eb-8f27-6132432b015a.png)

### Descriptive Figure

```{r}
ggplot(shapesUFO, aes(reorder(shape, -n), n, fill = shape)) + 
  geom_bar(stat = "identity") + 
  labs(title="What are the different shapes of UFOs?", x="Shapes", y="") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle=70, vjust=0.5))+
  scale_fill_viridis_d()
```
![image](https://user-images.githubusercontent.com/55526292/99347718-bb0e0c00-2865-11eb-8cbc-0bf52d14e427.png)


```{r, echo=FALSE}
ggplot(countriesUFO, aes(reorder(country, -n), n, fill = country)) + 
  geom_bar(stat = "identity") + 
  labs(title="What are the different countries these reports are from?", x="Countries", y="") +
  theme_bw() + scale_fill_viridis_d()
```
![image](https://user-images.githubusercontent.com/55526292/99347728-c5c8a100-2865-11eb-8b56-df8e6b6e5970.png)


#### Look at "Countries" and "Shapes" Together

```{r, echo=FALSE}
library(highcharter)
ufo$country <- as.factor(ufo$country)
head(ufo$country)
ufo %>% 
  group_by(country,shape) %>% 
  summarise(n=n()) %>% 
  top_n(5) %>%
  hchart("column", hcaes(x = 'country', y = 'n', group = 'shape'))

# ufo %>% group_by(year) %>% summarise(n=n()) %>%
#   hchart("line", hcaes(x = 'year', y = 'n'))
# 
# ufo %>% group_by(country) %>% summarise(n=n()) %>%
#   hchart("column", hcaes(x = 'country', y = 'n'), color="purple")

```
![image](https://user-images.githubusercontent.com/55526292/99347744-d416bd00-2865-11eb-9e18-1cee0e6fb536.png)
![image](https://user-images.githubusercontent.com/55526292/99347769-e1cc4280-2865-11eb-91ee-ab7eb62e0cb5.png)

## RESULTS

### Trends in the past 80 years 

To answer the question of whether or not there is a trend in UFO Sightings over the past century, we first look at the overall frequency of sighting reports by year for the entire dataset. However, the data points are not visible on the graph until 1940, and we altered the investigating question to whether or not there is a trend in UFO Sightings over the past 80 years. The graph below demonstrates an overall increase in the number of UFO Sightings across the years and shows a peak during the last few years. 

```{r, echo=FALSE}
#####################
## Date formatting ##
#####################

ufo$year <- year(ufo$datetime)
ufo_year <- subset(ufo, year>1940)

ufo_year %>%
  ggplot(aes(x=year,y=..count..,fill=..count..))+
  stat_count(show.legend = FALSE)+
  ggtitle("Is there a pattern as the years go on in frequency of UFO sightings?")+
  xlab("Year") + ylab("Number of Sightings")

```
![image](https://user-images.githubusercontent.com/55526292/99347950-569f7c80-2866-11eb-824c-07d13b52ad1b.png)

In order to verify our findings, we conducted hypothesis testing to confirm that there exists a significant relationship between records of UFO sightings and year. 

```{r,echo=FALSE}
trendByYears <- ufo %>% 
  count(year, sort=TRUE)
head(trendByYears)

trendMod <- lm(n~year, data = trendByYears)
summary(trendMod)
```
![image](https://user-images.githubusercontent.com/55526292/99347972-64550200-2866-11eb-9f59-25cc6f794299.png)

Before we were able to build the model, we first constructed a new subset of the data with only the year of the sighting report and the number of reports within that year. The model takes year as a predictor and the number of records as the response variable. With the p-value at __2.82e-13__ for the year predictor variable, the single linear regression model shows that year is a significant predictor for the number of UFO reports. Hence, it confirms the previous finding of increasing sighting reports as the years go on. Although it is worth noting that the adjusted r squared is __0.46__, which is relatively half, meaning the model accounts for roughly half of the variance and perhaps more predictors are needed for a better model. 



### Trends by Month

In order to analyze the trends across time periods further we decided to look at the data on UFO sightings by month. In order to visualize these trends we created a bar graph that shows the frequency of UFO sightings by month. We used the lubridate package in R to assist in the sorting of data by month. The x-axis of the graph shows the months January through December, and the y-axis shows the number of sightings from 0 to 10,000. 

```{r}
ufo$month <- lubridate::month(ufo$datetime, label = T, abbr = T)
ufo %>%
  ggplot(aes(x=month,y=..count..,fill=..count..))+
  stat_count(show.legend = FALSE)+
  ggtitle("What about month, is there a pattern in the frequency of UFO sightings?")+
  xlab("Month") + ylab("Number of Sightings")
```
![image](https://user-images.githubusercontent.com/55526292/99347993-746ce180-2866-11eb-9166-87cfb61ad3e6.png)

The graph shows a relatively equal distribution of sightings in January through May and then a noticeable jump in the months June, July, and August before declining month by month through December. The month with the most sightings is July, by a wide margin, with a total number of sightings around 9,500. The month with the least sightings is February with less than 5,000 sightings.  



### Trends by Season 

To analyze if there was a pattern in UFO sightings across various time periods we felt it was important to look at seasons. We focused on the United States for this model because we wanted some sort of consistency between season characteristics and the majority of the dataset came from the United States. When thinking about possible patterns between frequency of sightings and various seasons there was the possibility of more sightings during warmer seasons, or seasons with different periods of darkness each day,  or seasons where school was not in session allowing for more time spent outside. To do this we split the “datetime” column into time and month of occurrence. Once the month was extracted, we could determine which season the sighting occurred in. To separate the months we used the traditional meteorological definition. October, November and December are classified as ”Fall”. January, February and March are classified as “Winter.” April, May and June are classified as “Spring.” July, August and September are classified as “Summer.” After plotting the seasons and frequency of sightings we were able to see that the warmer months of Summer have a significantly higher amount of reported UFO sightings. 

```{r, echo= FALSE}
UFOsightings= read.csv("UFO/scrubbed.csv")

#separate Date and time
UFOsightings$Time <- format(as.POSIXct(UFOsightings$datetime, "%m/%d/%Y %H:%M", tz = ""), format = "%H:%M")

#UFOsightings$Date <- format(as.Date(USUFO$datetime,"%d/%m/%Y"), format = "%d/%m")

UFOsightings$Month <- format(as.Date(UFOsightings$datetime,"%m/%d/%Y %H:%M"), format = "%m") 
UFOSeason= UFOsightings %>% 
  mutate(Season = ifelse(Month %in% c("10","11","12"), "Fall",ifelse(Month %in% c("01", "02", "03"), "Winter",ifelse(Month %in% c("04", "05","06"), "Spring","Summer"))))
UFOSeasons <- UFOSeason %>%
  group_by(Season) %>%
  summarise(frequency = n())
color_table <- tibble(
  Season = c("Fall", "Spring", "Summer", "Winter"),
  Color = c("tomato1", "skyblue2", "hotpink", "darkblue" )
  )
ggplot(data=UFOSeasons, aes(x=Season, y=frequency, fill= Season)) +
  geom_bar(stat= "identity")+  scale_fill_manual(values = color_table$Color) + geom_text(aes(label=frequency)) + ggtitle("UFO Sightings in each Season") + ylab("Number of Sightings") 

```
![image](https://user-images.githubusercontent.com/55526292/99348016-88b0de80-2866-11eb-8fdb-5c081e9270b8.png)


### Trend by Time of Day

To figure out if there is a trend in UFO sightings over the different times of the day, we first made a bar plot to see the frequency of sightings for each corresponding time. However, it turned out that since there are so many different “times” the x labels overlap, making it hard to see what time each frequency of sightings falls on. To solve the labels overlapping problem, we looked up in the documentary and found out the “scale_x_discrete(guide = guide_axis(check.overlap = TRUE))” function.  

```{r,echo=FALSE}
Time <- format(as.POSIXct(strptime(UFOsightings$datetime,"%m/%d/%Y %H:%M",tz="")) ,format = "%H:%M")
UFOsightings$Time <- Time

UFOsightings %>%
    ggplot(aes(x=Time,y=..count..,fill=..count..))+ stat_count(show.legend = FALSE)+ 
  ggtitle("Frequency of UFO Sightings by Time of Day")+ 
  xlab("Time")+ ylab("Number of Sightings") + 
  theme(axis.text.x = element_text(angle=50, vjust=0.11))+
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))

```
![image](https://user-images.githubusercontent.com/55526292/99348042-97979100-2866-11eb-95e5-4746e24c9f07.png)

After getting the general idea of the frequency of sightings trend throughout different times of the day, we further investigated the peak time of day for UFO sightings. To carry this out, we first noticed that the highest frequency of the sightings appeared to fall in the time period from 21:49 to 22:33. Therefore we decided to only plot the frequency of this time period. In order to find this time period, we tried to divide the length of the UFOsightings$Time by 24 and then multiplied by 21 and 22 respectively. After several trials, we found the appropriate range of the dataset, which is sort(UFOsightings\$Time)[60010:70010]. This range includes the time from 21:44 to 22:30. We then created a bar plot of the sightings by frequency during this selected time period and got the following result:

```{r,echo=FALSE}
time2 = sort(UFOsightings$Time)[60010:70010]
temp = as.vector(data.frame(numm=1:length(time2),new_time=time2))
ggplot(data=temp,aes(x=new_time,y=..count..,fill=..count..))+stat_count(show.legend = FALSE)+ ggtitle("Peak of UFO Sightings by Time of Day")+ xlab("Time")+ ylab("Number of Sightings")+theme(axis.text.x = element_text(angle=50, vjust=0.11)) + scale_x_discrete(guide = guide_axis(check.overlap = TRUE))
```
![image](https://user-images.githubusercontent.com/55526292/99348061-a3835300-2866-11eb-95ee-4973723faf6a.png)

From the above plot it’s easy to see 22:00 is the peak time for sightings during the day.
 

## CONCLUSION

In our first question we wanted to identify any trends in UFO sightings by season and month. After plotting the UFO sightings based on the traditional meteorological seasons of “Summer,” “Winter,” “Fall,” and “Spring,” we were able to find that UFO sightings were reported at an increased frequency during the Summer months. The second highest frequency of reports soccuring during Fall months. The third highest frequency of reports was during Spring months. The lowest number of reports occurred during Winter months. This is very interesting because in most of the United States warmer months are in the summer and colder months are during the winter. This is a possible explanation for why there are significantly more observations in the Summer. Another possible explanation for increased reports in the Summer is that the traditional school calendar leaves millions of students at home, and out of school for Summer months. These months of free time give more people the opportunity to stay up later and spend more time outside, thus increasing one’s chance of spotting an unidentified flying object in the Summer. To investigate trends by month we created a bar graph to represent the data by month. One trend did become apparent from this graph. The months June, July, and August had the highest frequency of sightings. This finding was a bit surprising because these are the months with the most daylight and we found in examining sightings by time of day that most sightings occur at night when it is dark. However, this could be explained by the fact people spend more time outside during the warm months of June, July, and August making it more likely to have a sighting during months that people spend more time outside. The graph also shows that the month February has the least sightings which could be explained by February being the shortest month of the year. 

In our second question we explored trends in years over the last century and by time of day. After plotting the UFO sightings over the past 80 years, we found there is an overall increase through the years and a peak during the last few years. In addition, to confirm the relationship between year and the number of sightings, a linear regression model was built to assess the strength of this relationship. From the p-value in the model summary, the relationship was statistically significant and there is indeed a relationship between year and the number of UFO sightings. After plotting the UFO sightings by time of the day, we discovered that there is a decreasing trend starting from midnight, and then some ups and downs in between. Starting from 18:55, there is an obvious increase in sightings and at 22:00 sightings reach a peak. Then sightings trend down with a slight increase around 20:33. In general, most UFO sightings occur during the evening from 19:39 to midnight.

In conclusion, we found that the most likely time to be lucky enough to experience a UFO sighting of your own is a night in July around 10 p.m. Reflecting on our work we recognize there are other datasets or models that might benefit our analysis. One possible dataset or model that might be helpful in this analysis would be to investigate the areas from which these reports are coming from. There might be a concentration of sightings in certain areas due to outside influencers that could skew the data. A potential method that could be used to make the results more useful is a textual analysis of the "comment" variable in this dataset, through which we can look at the general sentiment towards UFOs in this dataset and more. 



# Exploratory Analysis
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


