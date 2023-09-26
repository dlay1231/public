
library(ggplot2)

pf1<-read.csv('new-york-city.csv')
pf2<-read.csv('washington.csv')
pf3<-read.csv('chicago.csv')


#Using Library
library(dplyr)

#Adding Gender and Birth Year to Washington
pf2<-pf2%>%
  mutate(Gender='NULL')
pf2<-pf2%>%
  mutate(Birth.Year= 'NULL')



#Adding the city source to each data set
pf1<-pf1%>%
  mutate(Source = 'New York')
pf2<-pf2%>%
  mutate(Source = 'Washington')
pf3<-pf3%>%
  mutate(Source='Chicago')

#Combining the data sets to make reading it easier
combined_df <- rbind(pf1, pf2, pf3)

### QUESTION 1:What is the most popular Start Time?

#Aggregate data by Hour
aggregated_df <- combined_df%>%
  mutate(Hour = 
lubridate::hour(Start.Time)) %>%
  group_by(Source, Hour)%>%
  summarize(Count = n())

#Creating a chart to dsiplay the data
ggplot(aggregated_df, aes(x = Hour, y = Count, fill= Source)) +
  geom_bar(stat = 'identity')+
  labs(x = 'Hour of Day', y = "Count") +
  ggtitle("Start Time") +
  scale_fill_manual(values = 
c('blue', 'red', 'green')) +
  scale_x_continuous(breaks = 1:24)

###ANSWER 1: The most popular start time overall is 9AM.
###QUESTION 2: What is the most popular Start Time by City?

#separating the data by city instead of bulk

ggplot(aggregated_df, aes(x = Hour, y = Count, fill= Source)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(x = 'Hour of Day', y = "Count") +
  ggtitle("Start Time") +
  scale_fill_manual(values = 
c('blue', 'red', 'green')) +
  scale_x_continuous(breaks = 1:24)

###ANSWER 2: Chicago's most popular start time is 6 PM
### Washington's most popular start time is 9 AM
### New Yorks most popular start time is 6 PM

#converting Start.Time and End.Time to date-time format from character

combined_df$Start.Time<-
  as.POSIXct(combined_df$Start.Time, format = "%Y-%m-%d %H:%M:%S")

combined_df$End.Time<-
  as.POSIXct(combined_df$End.Time, format = "%Y-%m-%d %H:%M:%S")


###QUESTION 3 What is the average trip duration?

#average Trip Duration

mean_trip_duration<-
  mean(combined_df$Trip.Duration)

#make the mean look more readable

mean_trip_duration<-
  round(mean_trip_duration, 2)

###ANSWER 3: Average trip duration is 1024.4

#printing the data
print(mean_trip_duration)

###Question 4: What is the mean trip time by city?
#mean trip duration by city

mean_trip_duration_by_city <-
  combined_df %>%
  group_by(Source) %>%
  summarise(mean_duration = 
mean(Trip.Duration))

print(mean_trip_duration_by_city)

### ANSWER 4: The mean trip time by city is 
### Chicago- 936
### New York - 900
### Washington- 1237


###Question 5: What is the most popular trip?

#creating a table for the start and end station

station_combinations <-
  table(combined_df$Start.Station,combined_df$End.Station)
library(dplyr)

#grouping the data, counting occurences, and arranging

result<- combined_df %>%
  group_by(Start.Station, End.Station) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

#Extract and print the most common combo

most_common_combinations<-
  result[1,]
print(most_common_combinations)


###ANSWER 5: 
### The most common trip is Lake Shore Dr to Monroe St.


###QUESTION 6: What is the most common month?

#finding the most common month

library(lubridate)

combined_df$Month <-
  month(combined_df$Start.Time, label = 
  TRUE)

most_common_month <-
  names(sort(table(combined_df$Month),
  decreasing = TRUE) [1])
print(most_common_month)


###ANSWER 6: The most common month is June.