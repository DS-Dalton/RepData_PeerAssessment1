---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
data <- read.csv('./activity.csv')
```

## What is mean total number of steps taken per day?


```r
### Calculate the total number of steps taken per day
library(dplyr); library(lubridate);

# convert date to a datetime obj. and arrange descending
data$date <- ymd(data$date)
stepsPerDay <- arrange(data, desc(steps))

# Remove NA
stepsPerDay <- stepsPerDay[!is.na(stepsPerDay$steps), ]

# delete duplicates and rearrange by date
stepsPerDay <- stepsPerDay[!duplicated(stepsPerDay$date),]
stepsPerDay <- arrange(stepsPerDay, date)

# interval is meaningless in this context
stepsPerDay <- stepsPerDay[, c(1,2)]

# Create a histogram
png('./figures/fig1.png')
hist(stepsPerDay$steps, 
     col='steelblue', 
     main='Histogram of Steps Taken Per Day',
     xlab='Steps Taken')
dev.off()
```

```
## png 
##   2
```

Mean and median of steps taken per day:


```r
paste('Mean:', mean(stepsPerDay$steps))
```

```
## [1] "Mean: 600.88679245283"
```

```r
paste('Median:', median(stepsPerDay$steps))
```

```
## [1] "Median: 555"
```

## What is the average daily activity pattern?


```r
stepsPerInterval <- data
# remove NA
stepsPerInterval <- stepsPerInterval[!is.na(stepsPerInterval$steps),]
# Group by interval and calculate avg steps per interval
stepsPerInterval <- group_by(stepsPerInterval, interval)
stepsPerInterval <- summarise(stepsPerInterval, avgSteps=mean(steps))
# plot a time series
png('./figures/fig2.png')
plot(stepsPerInterval$interval, stepsPerInterval$avgSteps, 
     type='l',
     main = 'Avg Steps Per Interval Over All Days',
     xlab='Interval',
     ylab='Avg Steps Taken',
     col='steelblue')
dev.off()
```

```
## png 
##   2
```

Which interval has the maximum average number of steps taken?


```r
paste('Interval with maximum average number of steps taken:',
      stepsPerInterval[
              stepsPerInterval$avgSteps==max(stepsPerInterval$avgSteps), 
              'interval'])
```

```
## [1] "Interval with maximum average number of steps taken: 835"
```

## Imputing missing values

Total number of missing values in the dataset


```r
# only data$steps has missing values
paste('Number of missing values in the dataset:', 
      count(data[is.na(data$steps), ]))
```

```
## [1] "Number of missing values in the dataset: 2304"
```

Imputation strategy: take the average steps taken for each 5 minute interval, and impute that value anywhere that interval is NA. Round to the nearest whole digit.


```r
# stepsPerInterval is already the avg steps per interval.
dataImpute <- data

# Create a func that returns the proper value for each interval 
### from stepsPerInterval
getAvg <- function(interval){
        x <- stepsPerInterval[
                stepsPerInterval$interval == interval, 'avgSteps']
        x <- as.numeric(round(x, 0))
        x
}

# Impute NAs in dataImpute with the returned value from getAvg
counter <- 1
for(i in 1:nrow(dataImpute)){
        row <- dataImpute[i, ]
        if(is.na(row$steps)){
                dataImpute[counter, 1] <- getAvg((row$interval))
        }
        counter <- counter + 1
}
```

Now that we have imputed, what does the histogram for number of steps taken in a day look like?


```r
# Create a histogram
png('./figures/fig3.png')
hist(dataImpute$steps, 
     main='Histogram of Steps Taken Per Day',
     xlab='Steps Taken')
dev.off()
```

```
## png 
##   2
```

Using imputed data, what are the new mean and median?


```r
paste('Mean:', mean(dataImpute$steps))
```

```
## [1] "Mean: 37.3806921675774"
```

```r
paste('Median:', median(dataImpute$steps))
```

```
## [1] "Median: 0"
```

### Impact:

The imputation caused a severe skew toward 0. A lot of the NA values were likely NA because they were in the early morning hours when one might not be wearing a detection device due to sleeping. On the rare occasion that it was being worn, a low value was recorded, skewing the NAs toward 0 when I imputed with the means of those intervals.

Doing this heavily skews the histogram to the left (toward 0).

## Are there differences in activity patterns between weekdays and weekends?


```r
### Create factor variable of weekend/weekday based on date
days <- data
days <- mutate(days, dayType = weekdays(date))
days[days$dayType %in% c('Monday', 
                     'Tuesday', 
                     'Wednesday', 
                     'Thursday', 
                     'Friday'), 'dayType'] <- 'Weekday'
days[days$dayType %in% c('Saturday', 'Sunday'), 'dayType'] <- 'Weekend'
days$dayType <- factor(days$dayType, levels=c('Weekday', 'Weekend'))

### create table showing average steps per interval for weekday and weekend
# remove NA
days <- days[!is.na(days$steps),]
# Group
days <- group_by(days, dayType, interval)
days <- summarise(days, avgSteps=mean(steps))
```

### Panel plot of the day type showing average steps per interval.


```r
library(lattice);
png('./figures/fig4.png')
xyplot(avgSteps ~ interval | dayType, data=days, layout=c(1, 2), type='l')
dev.off()
```

```
## png 
##   2
```
