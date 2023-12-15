rm(list = ls())


library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)

## Uploading Data 
setwd("C:/Users/HP/OneDrive/Documents/Bellabeat_caseStady")
print(getwd())
activity<- read.csv("archive/Fitabase_Data2016/dailyActivity_merged.csv")
daily_Calories<- read.csv("archive/Fitabase_Data2016/dailyCalories_merged.csv")
hourly_Calories<- read.csv("archive/Fitabase_Data2016/hourlyCalories_merged.csv")
daily_Steps <-read.csv("archive/Fitabase_Data2016/dailySteps_merged.csv")
hourly_Steps <- read.csv("archive/Fitabase_Data2016/hourlySteps_merged.csv")
daily_Sleep <-read.csv("archive/Fitabase_Data2016/sleepDay_merged.csv")


avg_cal_per_date<- activity %>%
  select('Calories', 'ActivityDate') %>%
  group_by(ActivityDate) %>%
  summarise(total_calories = mean(Calories)) 

## Dates Formatting Date 
activity$ActivityDate <-format(activity$ActivityDate , format="%m%d%y")
avg_cal_per_date$ActivityDate <- as.Date(avg_cal_per_date$ActivityDate, format = "%m/%d/%Y")

# Order the data by date
avg_cal_per_date <- avg_cal_per_date %>%
  arrange(ActivityDate)

## How Calories change Over time Visualization
ggplot(avg_cal_per_date, aes(x = ActivityDate, y = total_calories, group = 1)) +
  geom_line() +
  geom_point(data = avg_cal_per_date[seq(1, nrow(avg_cal_per_date), by = 2), ], aes(x = ActivityDate, y = total_calories), color = "red") +
  labs(title = "Average Calories Over Time", x = "Date", y = "Average Calories") 

## Is there any correlation between Total Steps and calories 

# Calculate the correlation coefficient
correlation <- cor(activity$TotalSteps, activity$Calories)

# Create a scatter plot
ggplot(activity, aes(x = TotalSteps, y = Calories)) +
  geom_point() + geom_smooth()
  labs(title = paste("Correlation Between TotalSteps and Calories\nCorrelation Coefficient:", round(correlation, 2)),
       x = "Total Steps", y = "Calories")
  
## What types of activities are most commonly practiced?
#per Distance
  activities <- activity %>%
    select('ActivityDate', 'VeryActiveDistance', 'ModeratelyActiveDistance', 'LightActiveDistance', 'SedentaryActiveDistance') %>%
    summarise(
      TotalVeryActiveDistance = mean(VeryActiveDistance),
      TotalModeratelyActiveDistance = mean(ModeratelyActiveDistance),
      TotalLightActiveDistance = mean(LightActiveDistance),
      TotalSedentaryActiveDistance = mean(SedentaryActiveDistance)
    )
  

    activities_avgs <- c(activities$TotalVeryActiveDistance , activities$TotalModeratelyActiveDistance, activities$TotalLightActiveDistance,activities$TotalSedentaryActiveDistance)
    activities_labs <- c('TotalVeryActiveDistance' , 'TotalModeratelyActiveDistance', 'TotalLightActiveDistance','TotalSedentaryActiveDistance')
    activities_data <- data.frame(ActivityLevel = activities_labs, AverageDistance = activities_avgs)
    
    # Create a bar chart
    ggplot(activities_data, aes(x = ActivityLevel, y = AverageDistance, fill = ActivityLevel)) +
      geom_bar(stat = "identity") +
      labs(title = "Average Distances for Each Activity Level",
           x = "Activity Level",
           y = "Average Distance") +
      scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3"),  # Color options
                        name = "Activity Level") +
      theme_minimal()
    
#per Minutes 
    activities_perMinut <- activity %>%
      select('ActivityDate','VeryActiveMinutes', 'FairlyActiveMinutes', 'LightlyActiveMinutes', 'SedentaryMinutes') %>%
      summarise(
        AvgVeryActiveMinutes = mean(VeryActiveMinutes),
        AvgFairlyActiveMinutes= mean(FairlyActiveMinutes),
        AvgLightlyActiveMinutes= mean(LightlyActiveMinutes),
        AvgSedentaryMinutes = mean(SedentaryMinutes)
      )
    
    
    activitiesPerM_avgs <- c(activities_perMinut$AvgVeryActiveMinutes , activities_perMinut$AvgFairlyActiveMinutes, activities_perMinut$AvgLightlyActiveMinutes, activities_perMinut$AvgSedentaryMinutes)
    activitiesPerM_labs <- c('Avg_VeryActive' , 'Avg_FairlyActive', 'Avg_LightlyActive','Avg_Sedentary')
    activities_data <- data.frame(ActivityLevel = activitiesPerM_labs, AveragesPerM = activitiesPerM_avgs)
    
    # Create a bar chart
    ggplot(activities_data, aes(x = ActivityLevel, y = AveragesPerM, fill = ActivityLevel)) +
      geom_bar(stat = "identity") +
      labs(title = "Average Minutes Spent in each Activity Level",
           x = "Activity Level",
           y = "Average Minutes") +
      scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3"),  # Color options
                        name = "Activity Level") +
      theme(axis.text.x = element_text(angle = 90, hjust = 0))
    
##Calories Over Time
    # daily calories
    avg_cal_per_day<- daily_Calories %>%
      select('Calories', 'ActivityDay') %>%
      group_by(ActivityDay) %>%
      summarise(Mean_calories = mean(Calories)) 
    
    avg_cal_per_day$ActivityDay <- as.Date(avg_cal_per_day$ActivityDay, format = "%m/%d/%Y")
    data_orderBy_Day<- avg_cal_per_day %>%
      arrange(ActivityDay)
    ggplot(avg_cal_per_day , aes(x=ActivityDay , y=Mean_calories , group = 1)) +
      geom_line()+
      labs(title = "Average Calories Over Time", x = "Date", y = "Average Calories") 
    
    #per hour 
    avg_cal_per_hour<- hourly_Calories %>%
      select('ActivityHour' , 'Calories') %>%
      group_by(ActivityHour)%>%
      summarise(Mean_calories =mean(Calories))
    
    #Format date time column
    avg_cal_per_hour$ActivityHour <- as.POSIXct(avg_cal_per_hour$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p")
    
    # Add a new column with the time portion
    avg_cal_per_hour$Time <- format(avg_cal_per_hour$ActivityHour, format = "%H:%M")
    avg_cal_per_hour <- avg_cal_per_hour %>%
      arrange(Time)
    
    # How Calories change Over hours Visualization
    ggplot(avg_cal_per_hour, aes(x = Time, y = Mean_calories, fill = Time)) +
      geom_bar(stat = "identity") +
      labs(title = "Average Calories per hour",
           x = "Hourse",
           y = "Calories") +
      theme(axis.text.x = element_text(angle = 90, hjust = 0),legend.position = "none")
    
    
## Steps 
    #Steps Over Time
    avg_steps_per_day<- daily_Steps %>%
      select('ActivityDay', 'StepTotal') %>%
      group_by(ActivityDay) %>%
      summarise(Mean_Steps = mean(StepTotal)) 
    # Date Formatting 
    avg_steps_per_day$ActivityDay <- as.Date(avg_steps_per_day$ActivityDay, format = "%m/%d/%Y")
    #arrange 
    avg_steps_per_day<- avg_steps_per_day %>%
      arrange(ActivityDay)
    #Viz
    ggplot(avg_steps_per_day, aes(x = ActivityDay, y = Mean_Steps, fill = ActivityDay)) +
      geom_bar(stat = "identity") +
      labs(title = "Average Steps per Day",
           x = "Days",
           y = "Average Steps") +
      theme(axis.text.x = element_text(angle = 90, hjust = 0))
    
    #Steps per hour 
    avg_steps_per_hour <- hourly_Steps %>%
      select('ActivityHour' ,'StepTotal') %>%
      group_by(ActivityHour)%>%
      summarise(MeanSteps = mean(StepTotal))
    #Formatting Date
    avg_steps_per_hour$ActivityHour <- as.POSIXct(avg_steps_per_hour$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p")
    # Add a new column with the time portion
    avg_steps_per_hour$Time <- format(avg_steps_per_hour$ActivityHour, format = "%H:%M")    
    #Arrange Dataset
    avg_steps_per_hour %>%
      arrange(Time)
    #Viz
    
    ggplot(avg_steps_per_hour, aes(x = Time, y = MeanSteps)) +
      geom_point() +
      labs(title = "Average Steps per Day",
           x = "Days",
           y = "Average Steps") +
      theme(axis.text.x = element_text(angle = 90, hjust = 0))

## Sleep
    avg_daily_Sleep <- daily_Sleep %>%
      select('SleepDay' , 'TotalSleepRecords' , 'TotalMinutesAsleep' , 'TotalTimeInBed' ) %>%
      group_by(SleepDay) %>%
      summarise(MeanSleepRecords = mean(TotalSleepRecords),
                MeanMinutesAsleep = mean(TotalMinutesAsleep),
                MeanTimeInBed = mean(TotalTimeInBed))
    #Formatting Date
    avg_daily_Sleep$SleepDay <- as.POSIXct(avg_daily_Sleep$SleepDay, format = "%m/%d/%Y %I:%M:%S %p")
    # Add a new column with the Date portion
    avg_daily_Sleep$Day <- format(avg_daily_Sleep$SleepDay, format = "%m/%d/%Y")  
    #Data arrangement
    avg_daily_Sleep <- avg_daily_Sleep %>%
      arrange(Day)
    #Viz
    ggplot(avg_daily_Sleep, aes(x = Day, y = MeanMinutesAsleep)) +
      geom_point() +
      labs(title = "Average Minutes of sleep per Day",
           x = "Days",
           y = "Average Minutes") +
      theme(axis.text.x = element_text(angle = 90, hjust = 0))
    
    #Sleep in days of the week
    
    avg_daily_Sleep$weekDay <- format(avg_daily_Sleep$SleepDay, format = "%a")
    avg_sleep_weekDay <- avg_daily_Sleep %>%
      select( 'MeanSleepRecords' , 'MeanMinutesAsleep','MeanTimeInBed','weekDay') %>%
      group_by(weekDay) %>%
      summarise(AvgMeanSleepRecords =mean(MeanSleepRecords) ,
                AvgMeanMinutesAsleep =mean(MeanMinutesAsleep) ,
                AvgMeanTimeInBed =mean(MeanTimeInBed) )
    
    avg_sleep_weekDay <- avg_sleep_weekDay %>%
      arrange(match(weekDay, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))
    
    ggplot(avg_sleep_weekDay, aes(x = weekDay, y = AvgMeanMinutesAsleep/60 , fill = weekDay)) +
      geom_bar(stat = "identity") +
      labs(title = "Average Minutes of sleep per Dayweek",
           x = "Days",
           y = "Average Sleep hours") 
    
    
    #Sleep Record vs time and Bed
    #total Sleep vs time spend in bed
  
    ggplot(avg_sleep_weekDay, aes(x = weekDay)) +
      geom_line(aes(y = AvgMeanMinutesAsleep/60, group = 1, color = "Avg Sleep Duration"), size = 1, linetype = "solid") +
      geom_line(aes(y = AvgMeanTimeInBed/60, group = 1, color = "Avg Time in Bed"), size = 1, linetype = "solid") +
      labs(title = "Average Sleep Duration vs. Average Time in Bed",
           x = "Weekdays",
           y = "Hours of Sleep") +
      scale_color_manual(values = c("Avg Sleep Duration" = "blue", "Avg Time in Bed" = "red"),
                         labels = c("Avg Sleep Duration", "Avg Time in Bed")) +
      theme_minimal()
              
  
  
