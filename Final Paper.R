########################
## Libraries Required ##
########################
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

########################
####~~~~~~~~~~~~~~######
#### Final Report ######
####______________######
########################

## STOR320.01 Group 6
##### Brent Tompkins, Erica Spruill, Daqi Chen, Yiran Li
##__________________________________________________________________________________________________________________


# INTRODUCTION
##________________________


##### Unidentified Flying Object (UFO) sightings are a rare phenomenon that few people get to experience in their lives. 
## Our dataset has information on over 80,000 reported UFO sightings during the last century. The dataset includes 
## the following information from each sighting; the date of the sighting, the time of day, the duration of the sighting, 
## the location of the sighting, and the shape of the UFO. The data set includes data from mostly the United States but 
## there are some reports from other countries. 

##### The theme of our analysis pertains to the general trend of UFO sightings throughout various time periods. In our 
## initial data exploration, we were able to get a better idea of how the variables relate to one another. This helped 
## us narrow down our focus to two questions related to the patterns in UFO Sightings across various time periods.

##### Question1: Are there any trends in the frequency of UFO sightings during different seasons of the year, and 
## different months of the year?

##### Question2: Is there a trend in the time of day of UFO sightings and is there a trend in sightings by year over 
## the last century? 

##### These questions further explore the data set, searching for trends that might highlight patterns in UFO sightings 
## based on month, season, year, and time of day. Finding and highlighting patterns in the dataset can give us useful 
## information to develop new questions about UFO sightings. Our analysis will help you determine the time when you 
## are most likely to experience the phenomenon of seeing a UFO.
##__________________________________________________________________________________________________________________



# DATA
##_____________

## Describe data

##### The dataset we chose to investigate contains over 80,000 reports of Unidentified Flying Object (UFO) sightings 
## throughout the world from the year 1998 to 2014. The dataset was collected by the NAtional UFO Reporting Center 
## and has been scrubbed of observations that contain no time frame, no location, or errors in recording. There are 
## 11 columns in the UFO sightings dataset. The first column, "datetime," lists the time and date of each observation 
## in the format of month/day/year hour:minute. There are 4 columns identifying the location of each sighting: city, 
## state, country and latitude. The "shape" column describes the object shape as "light," "triangle," "circle," 
## "fireball," or "unknown." The "duration" column specifies how long the sighting was in hours, minutes and seconds. 
## There is also a "comments" column with descriptive notes about each observation.  There is also a "date posted" 
## column specifying when the observation was recorded with the National UFO Reporting Center. 

ufo <- read.csv("scrubbed.csv")
UFOsightings = ufo


## Descriptive Table
##```````````````````````````````````````````````````````````````````````````````````````````````````````

##### What are the variables in this dataset?
##================================================


#####################
## Date formatting ##
#####################
ufo = UFOsightings
ufo$datetime <- mdy_hm(ufo$datetime) 
ufo$date.posted <- mdy(ufo$date.posted)

ufo$year <- year(ufo$datetime)
glimpse(ufo)



##### What are the different shapes of UFOs?
##================================================


shapesUFO <- ufo %>% 
  count(shape, sort=TRUE)
head(tibble(shapesUFO))


##### What are the different countries these reports are from?
##=============================================================


countriesUFO <- ufo %>% 
  count(country, sort=TRUE)
countriesUFO$country <- as.factor(countriesUFO$country)
levels(countriesUFO$country) #[1] "au"           "ca"           "de"           "gb"           "other region" "us"
levels(countriesUFO$country) <- c("Australia","Canada","Germany","Great Britain","Other Regions","US")
head(tibble(countriesUFO))


## Descriptive Figure
##```````````````````````````````````````````````````````````````````````````````````````````````````````


ggplot(shapesUFO, aes(reorder(shape, -n), n, fill = shape)) + 
  geom_bar(stat = "identity") + 
  labs(title="What are the different shapes of UFOs?", x="Shapes", y="") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle=70, vjust=0.5))+
  scale_fill_viridis_d()




ggplot(countriesUFO, aes(reorder(country, -n), n, fill = country)) + 
  geom_bar(stat = "identity") + 
  labs(title="What are the different countries these reports are from?", x="Countries", y="") +
  theme_bw() + scale_fill_viridis_d()



##### Look at "Countries" and "Shapes" Together
##================================================

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



# RESULTS
##_____________

## Trends in the past 80 years 
##================================================

##### To answer the question of whether or not there is a trend in UFO Sightings over the past century, we 
## first look at the overall frequency of sighting reports by year for the entire dataset. However, the data 
## points are not visible on the graph until 1940, and we altered the investigating question to whether or 
## not there is a trend in UFO Sightings over the past 80 years. The graph below demonstrates an overall 
## increase in the number of UFO Sightings across the years and shows a peak during the last few years. 


ufo$year <- year(ufo$datetime)
ufo_year <- subset(ufo, year>1940)

ufo_year %>%
  ggplot(aes(x=year,y=..count..,fill=..count..))+
  stat_count(show.legend = FALSE)+
  ggtitle("Is there a pattern as the years go on in frequency of UFO sightings?")+
  xlab("Year") + ylab("Number of Sightings")



##### In order to verify our findings, we conducted hypothesis testing to confirm that there exists a significant 
## relationship between records of UFO sightings and year. 

{r,echo=FALSE}
trendByYears <- ufo %>% 
  count(year, sort=TRUE)
head(trendByYears)

trendMod <- lm(n~year, data = trendByYears)
summary(trendMod)


##### Before we were able to build the model, we first constructed a new subset of the data with only the year 
## of the sighting report and the number of reports within that year. The model takes year as a predictor and 
## the number of records as the response variable. With the p-value at __2.82e-13__ for the year predictor v
## ariable, the single linear regression model shows that year is a significant predictor for the number of 
## UFO reports. Hence, it confirms the previous finding of increasing sighting reports as the years go on. 
## Although it is worth noting that the adjusted r squared is __0.46__, which is relatively half, meaning the 
## model accounts for roughly half of the variance and perhaps more predictors are needed for a better model. 



## Trends by Month
##================================================

# ##### In order to analyze the trends across time periods further we decided to look at the data on UFO sightings 
# by month. In order to visualize these trends we created a bar graph that shows the frequency of UFO sightings by 
# month. We used the lubridate package in R to assist in the sorting of data by month. The x-axis of the graph shows 
# the months January through December, and the y-axis shows the number of sightings from 0 to 10,000. 


ufo$month <- lubridate::month(ufo$datetime, label = T, abbr = T)
ufo %>%
  ggplot(aes(x=month,y=..count..,fill=..count..))+
  stat_count(show.legend = FALSE)+
  ggtitle("What about month, is there a pattern in the frequency of UFO sightings?")+
  xlab("Month") + ylab("Number of Sightings")

 
# ##### The graph shows a relatively equal distribution of sightings in January through May and then a noticeable 
# jump in the months June, July, and August before declining month by month through December. The month with the most
# sightings is July, by a wide margin, with a total number of sightings around 9,500. The month with the least 
# sightings is February with less than 5,000 sightings.  



## Trends by Season 
##================================================

# ##### To analyze if there was a pattern in UFO sightings across various time periods we felt it was important to 
# look at seasons. We focused on the United States for this model because we wanted some sort of consistency between 
# season characteristics and the majority of the dataset came from the United States. When thinking about possible 
# patterns between frequency of sightings and various seasons there was the possibility of more sightings during warmer
# seasons, or seasons with different periods of darkness each day,  or seasons where school was not in session allowing 
# for more time spent outside. To do this we split the "datetime" column into time and month of occurrence. Once the 
# month was extracted, we could determine which season the sighting occurred in. To separate the months we used the 
# traditional meteorological definition. October, November and December are classified as "Fall". January, February and 
# March are classified as "Winter." April, May and June are classified as "Spring." July, August and September are 
# classified as "Summer." After plotting the seasons and frequency of sightings we were able to see that the warmer
# months of Summer have a significantly higher amount of reported UFO sightings. 

UFOsightings= read.csv("scrubbed.csv")

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




## Trend by Time of Day
##================================================

# ##### To figure out if there is a trend in UFO sightings over the different times of the day, we first made a bar
# plot to see the frequency of sightings for each corresponding time. However, it turned out that since there are so 
# many different "times" the x labels overlap, making it hard to see what time each frequency of sightings falls on. 
# To solve the labels overlapping problem, we looked up in the documentary and found out the "scale_x_discrete
# (guide = guide_axis(check.overlap = TRUE))" function.  

Time <- format(as.POSIXct(strptime(UFOsightings$datetime,"%m/%d/%Y %H:%M",tz="")) ,format = "%H:%M")
UFOsightings$Time <- Time

UFOsightings %>%
  ggplot(aes(x=Time,y=..count..,fill=..count..))+ stat_count(show.legend = FALSE)+ 
  ggtitle("Frequency of UFO Sightings by Time of Day")+ 
  xlab("Time")+ ylab("Number of Sightings") + 
  theme(axis.text.x = element_text(angle=50, vjust=0.11))+
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))



# ##### After getting the general idea of the frequency of sightings trend throughout different times of the day, we 
# further investigated the peak time of day for UFO sightings. To carry this out, we first noticed that the highest 
# frequency of the sightings appeared to fall in the time period from 21:49 to 22:33. Therefore we decided to only plot 
# the frequency of this time period. In order to find this time period, we tried to divide the length of the 
# UFOsightings$Time by 24 and then multiplied by 21 and 22 respectively. After several trials, we found the appropriate 
# range of the dataset, which is sort(UFOsightings\$Time)[60010:70010]. This range includes the time from 21:44 to 22:30. 
# We then created a bar plot of the sightings by frequency during this selected time period and got the following result:

time2 = sort(UFOsightings$Time)[60010:70010]
temp = as.vector(data.frame(numm=1:length(time2),new_time=time2))
ggplot(data=temp,aes(x=new_time,y=..count..,fill=..count..))+stat_count(show.legend = FALSE)+ ggtitle("Peak of UFO Sightings by Time of Day")+ xlab("Time")+ ylab("Number of Sightings")+theme(axis.text.x = element_text(angle=50, vjust=0.11)) + scale_x_discrete(guide = guide_axis(check.overlap = TRUE))


##### From the above plot it's easy to see 22:00 is the peak time for sightings during the day.


# CONCLUSION
##_________________

# ##### In our first question we wanted to identify any trends in UFO sightings by season and month. After plotting the 
# UFO sightings based on the traditional meteorological seasons of "Summer," "Winter," "Fall," and "Spring," we were able 
# to find that UFO sightings were reported at an increased frequency during the Summer months. The second highest frequency 
# of reports soccuring during Fall months. The third highest frequency of reports was during Spring months. The lowest number 
# of reports occurred during Winter months. This is very interesting because in most of the United States warmer months are 
# in the summer and colder months are during the winter. This is a possible explanation for why there are significantly more 
# observations in the Summer. Another possible explanation for increased reports in the Summer is that the traditional school 
# calendar leaves millions of students at home, and out of school for Summer months. These months of free time give more 
# people the opportunity to stay up later and spend more time outside, thus increasing one's chance of spotting an unidentified
# flying object in the Summer. To investigate trends by month we created a bar graph to represent the data by month. One trend 
# did become apparent from this graph. The months June, July, and August had the highest frequency of sightings. This finding
# was a bit surprising because these are the months with the most daylight and we found in examining sightings by time of day 
# that most sightings occur at night when it is dark. However, this could be explained by the fact people spend more time outside 
# during the warm months of June, July, and August making it more likely to have a sighting during months that people spend 
# more time outside. The graph also shows that the month February has the least sightings which could be explained by February 
# being the shortest month of the year. 

# ##### In our second question we explored trends in years over the last century and by time of day. After plotting the UFO sightings 
# over the past 80 years, we found there is an overall increase through the years and a peak during the last few years. In addition, 
# to confirm the relationship between year and the number of sightings, a linear regression model was built to assess the strength 
# of this relationship. From the p-value in the model summary, the relationship was statistically significant and there is indeed 
# a relationship between year and the number of UFO sightings. After plotting the UFO sightings by time of the day, we discovered 
# that there is a decreasing trend starting from midnight, and then some ups and downs in between. Starting from 18:55, there is 
# an obvious increase in sightings and at 22:00 sightings reach a peak. Then sightings trend down with a slight increase around 20:33. 
# In general, most UFO sightings occur during the evening from 19:39 to midnight.
 
# ##### In conclusion, we found that the most likely time to be lucky enough to experience a UFO sighting of your own is a night in 
# July around 10 p.m. Reflecting on our work we recognize there are other datasets or models that might benefit our analysis. One
# possible dataset or model that might be helpful in this analysis would be to investigate the areas from which these reports are 
# coming from. There might be a concentration of sightings in certain areas due to outside influencers that could skew the data. 
# A potential method that could be used to make the results more useful is a textual analysis of the "comment" variable in this 
# dataset, through which we can look at the general sentiment towards UFOs in this dataset and more. 









