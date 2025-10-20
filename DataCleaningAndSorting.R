#This repository includes two solutions for the Google Data Analytics Capstone: Complete a Case Study, part of the Google Data Analytics Professional Certificate.
#The project, "Case Study 1: How does a bike-share navigate speedy success?", involves analyzing a scenario similar to what could be asked in a job interview.
#We used Cyclistic's historical trip data to explore patterns and identify trends. The analysis is based on the data provided in the following ZIP files (available here): 

library(tidyverse)
library(conflicted)
library(ggplot2)
library(fs)
conflict_prefer("filter","dplyr")
conflict_prefer("lag","dplyr")
install.packages("zoo")
library(zoo)
# reading csv files

q1_2019 <- read.csv("Divvy_Trips_2019_Q1.csv")
q2_2019 <- read.csv("Divvy_Trips_2019_Q2.csv")
q3_2019 <- read.csv("Divvy_Trips_2019_Q3.csv")
q4_2019 <- read.csv("Divvy_Trips_2019_Q4.csv")
q1_2020 <- read.csv("Divvy_Trips_2020_Q1.csv")

colnames(q1_2019)
colnames(q2_2019)
colnames(q3_2019)
colnames(q4_2019)
colnames(q1_2020)

str(q1_2020)

# renaming the column names in 2019 with 2020

q1_2019 <- rename(q1_2019,
                  ride_id = trip_id,
                  rideable_type = bikeid,
                  started_at = start_time,
                  ended_at = end_time,
                  start_station_name = from_station_name,
                  start_station_id = from_station_id,
                  end_station_name = to_station_name,
                  end_station_id = to_station_id,
                  member_casual = usertype
) 
# q1_2020 does not have tripdurarion, gender and birthyear columns, so we use the names in q4_2019

q2_2019 <- rename(q2_2019,
                  ride_id = X01...Rental.Details.Rental.ID,
                  rideable_type = X01...Rental.Details.Bike.ID,
                  tripduration = X01...Rental.Details.Duration.In.Seconds.Uncapped, 
                  started_at = X01...Rental.Details.Local.Start.Time,
                  ended_at = X01...Rental.Details.Local.End.Time,
                  start_station_name = X03...Rental.Start.Station.Name,
                  start_station_id = X03...Rental.Start.Station.ID,
                  end_station_name = X02...Rental.End.Station.Name,
                  end_station_id = X02...Rental.End.Station.ID,
                  member_casual = User.Type,
                  gender = Member.Gender,
                  birthyear = X05...Member.Details.Member.Birthday.Year)


q3_2019 <- rename(q3_2019,
                  ride_id = trip_id,
                  rideable_type = bikeid,
                  started_at = start_time,
                  ended_at = end_time,
                  start_station_name = from_station_name,
                  start_station_id = from_station_id,
                  end_station_name = to_station_name,
                  end_station_id = to_station_id,
                  member_casual = usertype)

q4_2019 <- rename(q4_2019,
                  ride_id = trip_id,
                  rideable_type = bikeid,
                  started_at = start_time,
                  ended_at = end_time,
                  start_station_name = from_station_name,
                  start_station_id = from_station_id,
                  end_station_name = to_station_name,
                  end_station_id = to_station_id,
                  member_casual = usertype)

str(q1_2019)
str(q2_2019)
str(q3_2019)
str(q4_2019)
str(q1_2020)



#Now based on the information obtained from the previous code chunk,
# First, since all the previous data-frames will be merged to form a complete data-frame,
#we need all columns have the same data-type. Columns ride_id and rideable_type from the 2019 data-frames do not share the same type as the 2020 (num and chr, respectively). We will need to fix this before we can combine the data.

q1_2019 <- mutate(q1_2019, 
                  ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type),
                  started_at = as.POSIXct(started_at, 
                                          format = "%Y-%m-%d %H:%M:%S", 
                                          tz = "UTC"),
                  ended_at = as.POSIXct(ended_at,format = "%Y-%m-%d %H:%M:%S", 
                                        tz = "UTC")
)

q2_2019 <- mutate(q2_2019, 
                  ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type),
                  started_at = as.POSIXct(started_at, 
                                          format = "%Y-%m-%d %H:%M:%S", 
                                          tz = "UTC"),
                  ended_at = as.POSIXct(ended_at,format = "%Y-%m-%d %H:%M:%S", 
                                        tz = "UTC")
)

q3_2019 <- mutate(q3_2019, 
                  ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type),
                  started_at = as.POSIXct(started_at, 
                                          format = "%Y-%m-%d %H:%M:%S", 
                                          tz = "UTC"),
                  ended_at = as.POSIXct(ended_at,format = "%Y-%m-%d %H:%M:%S", 
                                        tz = "UTC")
)

q4_2019 <- mutate(q4_2019, 
                  ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type),
                  started_at = as.POSIXct(started_at, 
                                          format = "%Y-%m-%d %H:%M:%S", 
                                          tz = "UTC"),
                  ended_at = as.POSIXct(ended_at,format = "%Y-%m-%d %H:%M:%S", 
                                        tz = "UTC")
                  
)

q1_2020 <- mutate(q1_2020, started_at = as.POSIXct(started_at, 
                                                   format = "%Y-%m-%d %H:%M:%S", 
                                                   tz = "UTC"),
                  ended_at = as.POSIXct(ended_at,format = "%Y-%m-%d %H:%M:%S", 
                                        tz = "UTC") )


#combining all trips

combined_trips <- bind_rows(q1_2019,q2_2019,q3_2019,q4_2019,q1_2020)
head(combined_trips)

# removing unnecessary columns

combined_trips <- combined_trips %>% select(-c(tripduration,gender,birthyear,start_lat,start_lng,end_lat,end_lng))


colnames(combined_trips)

dim(combined_trips)
#Second, the column member_casual in q1_2020 has two values: member and casual, while all data-frames from 2019 have the values: Subscriber and Customer. We will get back to this once we have solved the first observation.

head(combined_trips)

summary(combined_trips)

#Data cleaning

# the column member_casual in q1_2020 has two values: member and casual, while all data-frames from 2019 have the values: Subscriber and Customer". Let's check the number of rows for each values

table(combined_trips$member_casual)

# we will change the member types from Customer/Subscriber to member/casual respectively

combined_trips <- combined_trips %>% mutate(member_casual = recode(member_casual,"Subscriber"="member", "Customer"="casual"))

#The all_trips DataFrame originally included a column named tripduration, but we removed it because the q1_2020 DataFrame did not contain this column, 
#while the others did. Later, we will restore this information by calculating a new column using the started_at and ended_at columns.
#In the meantime, since we need details about member trips and more user-friendly date information than what started_at provides,
#we will add five new columns: date, month, day, year, and day_of_week, all derived from the started_at column.

combined_trips$date <- as.Date(combined_trips$started_at)
combined_trips$month <- format(as.Date(combined_trips$date),"%B") #month
combined_trips$day <- format(as.Date(combined_trips$date),"%d") #day
combined_trips$year <- format(as.Date(combined_trips$date),"%Y") #year
combined_trips$weekday <- format(as.Date(combined_trips$date),"%A") #weekday



combined_trips$ride_time <- difftime(combined_trips$ended_at,
                                     combined_trips$started_at,
) 

str(combined_trips)




combined_trips$ride_time <- as.numeric(as.character(combined_trips$ride_time))
is.numeric(combined_trips$ride_time)
summary(combined_trips)

#The summary of the ride_length column shows negative time values, which shouldn’t be possible. This likely indicates an issue with the data. 
#One explanation could be that certain bikes were removed from docks for maintenance (e.g., trips starting at HQ QR),
#or there might be errors in the time records.
#Since this data is not reliable, we’ll exclude
#all rows where start_station_name = HQ QR or where ride_length is less than zero.

combined_trips_v2 <- combined_trips[!(combined_trips$start_station_name == 'HQ QR' | combined_trips$ride_time < 0),]

nrow(combined_trips) - nrow(combined_trips_v2)

#Descriptive Analysis

summary(combined_trips_v2$ride_time)

#exploring the ride length between annual members vs. casual riders in a descriptive way, 

aggregate(combined_trips_v2$ride_time ~ combined_trips_v2$member_casual, FUN = mean)
aggregate(combined_trips_v2$ride_time ~ combined_trips_v2$member_casual,FUN = median)
aggregate(combined_trips_v2$ride_time ~ combined_trips_v2$member_casual,FUN = max)
aggregate(combined_trips_v2$ride_time ~ combined_trips_v2$member_casual,FUN = min)

#from the statistics analysis we can observe that Casual riders are more active in cycling as compared to annual members.

#analysis with respect to month

result <- combined_trips_v2 %>% mutate(year_month = as.yearmon(combined_trips_v2$date)) %>% 
  group_by(member_casual,year_month) %>% 
  summarise(number_of_rides=n(),average_duration = mean(ride_time)) %>% 
  arrange(member_casual,year_month)

View(result)

print(n=combined_trips_v2)

#plotting the information on graph

all_trips_v2 %>% 
  mutate(year_month = as.yearmon(all_trips_v2$date)) %>% 
  group_by(member_casual, year_month) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, year_month) %>%  
  ggplot(aes(x = as.Date(year_month), y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge", stat = "identity") +
  scale_x_date(date_breaks="1 month", date_labels="%Y-%B", expand=c(0,0)) +
  labs(title = 'Number of rides by year-month and riders type',
       x = 'Year-Month',
       y = 'Number of Rides') +
  theme(axis.text.x = element_text(angle = 90))

#from the graph..it can be seen that annual members use more bikes than cyclist and the August being the month of highest demand for cycles

#now lets plot the graph with respect to average duration

combined_trips_v2 %>% 
  mutate(year_month = as.yearmon(combined_trips_v2$date)) %>% 
  group_by(member_casual, year_month) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_time)) %>% 
  arrange(member_casual, year_month) %>%  
  ggplot(aes(x = as.Date(year_month), y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge", stat = "identity") +
  scale_x_date(date_breaks="1 month", date_labels="%Y-%B", expand=c(0,0)) +
  labs(title = 'Average duration by year-month and riders type',
       x = 'Year-Month',
       y = 'Average duration(Sec)') +
  theme(axis.text.x = element_text(angle = 90))

# the output graph shows that January is the month with highest trip duration of casual riders.

# so far we have observed that casual riders have highest trip duration as compared to annual riders. Whereas annual riders have maximum number of rides with shorter duration

# another factor to consider is weekdays

combined_trips_v2 %>% 
  mutate(dayweek = combined_trips_v2$weekday) %>% 
  group_by(member_casual, dayweek) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_time)) %>% 
  arrange(member_casual, dayweek) %>%  
  ggplot(aes(x = dayweek, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = 'Number of rides by week day and riders type',
       x = 'week day',
       y = 'Number of rides') 

#the output graph shows that casual members are less active on weekdays and more active on weekends, while annual members are more active on weekdays and less active on weekends

#Now lets focus on average trips duration with day of the week

combined_trips_v2 %>% 
  mutate(dayweek = combined_trips_v2$weekday) %>% 
  group_by(member_casual, dayweek) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_time)) %>% 
  arrange(member_casual, dayweek) %>%  
  ggplot(aes(x = dayweek, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = 'average duration by week day and riders type',
       x = 'week day',
       y = 'average duration') 

#It can be seen that causal members outperform annual riders with respect to trip duration and weekday

# Exporting final report
#Finally, we’ll export the final report, which includes the total number of rides and the average trip duration broken down by member type and weekday. This makes it easy to analyze in spreadsheet tools, visualize in Tableau for presentations, or use again to recreate the charts we built earlier.

counts_yearmonth <- combined_trips_v2 %>%
  mutate(year_month = as.yearmon(combined_trips_v2$date)) %>% 
  group_by(member_casual, year_month) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_time)) %>% 
  arrange(member_casual, year_month)
write.csv(counts_yearmonth, file = 'year_month_number_rides_and_avg_ride_length_r.csv', row.names = F)

counts_weekday <- combined_trips_v2 %>%
  mutate(dayweek = combined_trips_v2$weekday) %>% 
  group_by(member_casual, dayweek) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_time)) %>% 
  arrange(member_casual, dayweek)
write.csv(counts_weekday, file = 'week_day_number_rides_and_avg_ride_length_r.csv', row.names = F)

getwd()