library(lubridate)
library(dplyr)

setwd("~/Online_Courses/Data Analysis R Foundations/Reproducible research/Project 1/RepData_PeerAssessment1")
data <- read.table(unz("activity.zip", "activity.csv"), sep=",", header = TRUE)
data$date <- as.Date(data$date)
# setps: number of steps taken in each five minutes interval
# date: YYYY-MM-DD
# interval: identifier for 5-minute interval 

## Analysis of total steps per day
step_day <- data %>% group_by(date) %>% summarize(stepsperday = sum(steps, na.rm = TRUE))
with(step_day, hist(stepsperday, breaks = 25, xlab = "Steps per day", ylab = "Number of days", main = "Histogram of steps per day"))

mean(step_day$stepsperday)
median(step_day$stepsperday)

## Analysis of daily activity pattern
step_pattern <- data %>% group_by(interval) %>% summarize(stepsperinterval = mean(steps, na.rm = TRUE))

datetime <- as.POSIXct(paste(data$date,strptime(data$interval, "%H%M")))
