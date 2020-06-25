library(lubridate)
library(dplyr)

setwd("~/Online_Courses/Data Analysis R Foundations/Reproducible research/Project 1/RepData_PeerAssessment1")
data <- read.table(unz("activity.zip", "activity.csv"), sep=",", header = TRUE)
data$date <- as.Date(data$date)
# setps: number of steps taken in each five minutes interval
# date: YYYY-MM-DD
# interval: identifier for 5-minute interval 

## Analysis of total steps per day
data_good <- subset(data, !is.na(steps))
step_day <- data_good %>% group_by(date) %>% summarize(stepsperday = sum(steps, na.rm = TRUE))
with(step_day, hist(stepsperday, breaks = 25, xlim = c(0, 25000), ylim = c(0,12),
                    xlab = "Steps per day", ylab = "Number of days", 
                    main = "Histogram of steps per day"))

mean <- mean(step_day$stepsperday)
median <- median(step_day$stepsperday)

## Analysis of daily activity pattern
step_pattern <- data_good %>% group_by(interval) %>% 
                    summarize(stepsperinterval = mean(steps))

# step_pattern$time <- sprintf('%04d', step_pattern$interval) %>% strptime("%H%M")
with(step_pattern, plot(interval, stepsperinterval, type = 'l', xlab = "Time", 
                        ylab = "Average no. of steps", main = "Average steps per Interval"))

max_index <- which.max(step_pattern$stepsperinterval)

# max_interval <- step_pattern$time[max_index] %>% strftime(max_interval, format="%H:%M")

## NA missing values
na_sum <- sum(is.na(data))

data2 <- data
na_index <- which(is.na(data2))
data2$stnew <- data2$steps 
na_interval <- data2$interval[na_index]
data2$stnew[na_index] <- step_pattern$stepsperinterval[sapply(na_interval, 
                        function(x){which(step_pattern$interval == x)})]

step_day_new <- data2 %>% group_by(date) %>% summarize(stepsperday = sum(stnew))
with(step_day_new, hist(stepsperday, breaks = 25, xlim = c(0, 25000), ylim = c(0,20),
                    xlab = "Steps per day", ylab = "Number of days", 
                    main = "Histogram of steps per day"))

mean_new <- mean(step_day_new$stepsperday)
median_new <- median(step_day_new$stepsperday)

# factoring new data based on weekend and weekday
weekday <- c("Monday",  "Tuesday" ,  "Wednesday", "Thursday"  , "Friday" )
weekend <- c("Saturday", "Sunday")
data2$type <- factor(sapply(data2$date, function(x) {if(weekdays(x) %in% weekday) "Weekday" else "Weekend"}))

step_pattern_imputed <- data2 %>% group_by(interval, type) %>% 
    summarize(stepsperinterval = mean(stnew))

step_pattern_imputed$time <- as.POSIXct(sprintf('%04d', step_pattern_imputed$interval) %>% strptime("%H%M"))
q <- with(step_pattern_imputed, qplot(interval, stepsperinterval, facets = type~., 
                                      xlab = "Interval",ylab = "Average no. of steps", 
                                      main = "Average steps per Interval", geom = "path"))
plot(q)