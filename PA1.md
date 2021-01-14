---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Introduction 
In order to properly analyze and assess the veracity of the data contained in 
the FitBit, Nike, FuelBand, and Jawbone Up monitoring devices, it is necessary  
that we clean, extrapolate, and visualize the data. 

Over the months of October and November 2012, over 5 minute intervals this
anonymous data tracks the number of steps taken. 


## Loading and preprocessing the data

The natural first step would be to make sure your WD is set, and making sure the 
data exists. For our purposes, assume the data has been downloaded. Time to read 
it in and set our libraries: 


```r
library(data.table)
library(knitr) #I think this preloads when you use "Rmd" but just in case 

data <- read.csv("activity.csv")
```

Now that it's here, lets look at what we have:

```r
str(data) 
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
summary(data)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```

## What is mean total number of steps taken per day?

Visualize Data: 
1.) Total number of steps over a day

```r
hist(tapply(data$steps, data$date, sum, na.rm=TRUE)
    , main = "Total Steps per Day"
    , xlab = "Steps"
    , ylab = "Frequency (days)")
```

![](PA1_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


2.) hist 1 gives us a rough idea of about the distribution, but what about the 
amount of steps on a given day?

```r
barplot(tapply(data$steps, data$date, sum, na.rm = TRUE)
        , main = "Count of Steps Each Day"
        , xlab = "Date"
        , ylab = "Steps")
```

![](PA1_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


Time to do Math: 

```r
mean(tapply(data$steps, data$date, sum, na.rm=TRUE))
```

```
## [1] 9354.23
```

```r
median(tapply(data$steps, data$date, sum, na.rm=TRUE))
```

```
## [1] 10395
```
Mean = 9354.23* (Steps per day)
Median = 10395* (Steps per day)
*WITH MISSING VALUES REMOVED 

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") 
of the 5-minute interval (x-axis) and the average number of steps taken, 
averaged across all days (y-axis)


```r
Avg_Int <- tapply(data$steps, data$interval, mean, na.rm = TRUE)

#visualize results 
plot(names(Avg_Int), Avg_Int, type = "l"
     , xlab = "5 min Interval"
     , ylab = "Avg Number of Steps")
```

![](PA1_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


2. Which 5-minute interval, on average across all the days in the dataset, 
contains the maximum number of steps?


```r
names(Avg_Int)[which.max(Avg_Int)]
```

```
## [1] "835"
```

## Imputing missing values

As noted in the code the missing values are denoted as "NA" 

1. Calculate and report the total number of missing values in the dataset 
(i.e. the total number of rows with NAs)


```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. 
The strategy does not need to be sophisticated (W). For example, you could use
the mean/median for that day, or the mean for that 5 minute interval, etc. 

  The first example seems to be highly judicious for the user, but the mean of
  the 5 minute interval seems reasonable. Lets proceed with that 

3. Create a new dataset that is equal to the original dataset but with the
missing data filled in 

New Dataset: 

```r
data[100:110, ]
```

```
##     steps       date interval
## 100    NA 2012-10-01      815
## 101    NA 2012-10-01      820
## 102    NA 2012-10-01      825
## 103    NA 2012-10-01      830
## 104    NA 2012-10-01      835
## 105    NA 2012-10-01      840
## 106    NA 2012-10-01      845
## 107    NA 2012-10-01      850
## 108    NA 2012-10-01      855
## 109    NA 2012-10-01      900
## 110    NA 2012-10-01      905
```

```r
completeData <- data
completeData$steps <- ifelse(is.na(completeData$steps),
                         Avg_Int[as.character(completeData$interval)],
                         completeData$steps)

completeData[100:110,]
```

```
##        steps       date interval
## 100 157.5283 2012-10-01      815
## 101 171.1509 2012-10-01      820
## 102 155.3962 2012-10-01      825
## 103 177.3019 2012-10-01      830
## 104 206.1698 2012-10-01      835
## 105 195.9245 2012-10-01      840
## 106 179.5660 2012-10-01      845
## 107 183.3962 2012-10-01      850
## 108 167.0189 2012-10-01      855
## 109 143.4528 2012-10-01      900
## 110 124.0377 2012-10-01      905
```

4. Make a histogram of the total number of steps taken each day and Calculate
and report the mean and median total number of steps taken per day. 

```r
hist(tapply(completeData$steps, completeData$date, na.rm= TRUE)
           , main = "Steps per Day"
           , xlab = "Steps"
           , ylab = "Count")
```

![](PA1_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

Clearly there is a left skew

## Are there differences in activity patterns between weekdays and weekends?

1. *Create a new factor variable in the dataset with two levels - "weekday" 
and "weekend" indicating whether a given date is a weekday or weekend day.


```r
# This function takes a string, converts it to a date, and returns whether
# that date is a "weekday" or a "weekend".
DayType <- function(str)
{
    day <- weekdays(as.Date(str))
    
    if (day == "Saturday" | day == "Sunday")
    {
        dayType <- "weekend"
    }
    else
    {
        dayType <- "weekday"
    }
    
    dayType
}
# Create a new factor column with the day type (weekday or weekend)
completeData <- cbind(completeData, daytype = sapply(completeData$date, DayType))
```

Sample data after the update:


```r
completeData[1435:1445,]
```

```
##      steps       date interval daytype
## 1435     0 2012-10-05     2330 weekday
## 1436     0 2012-10-05     2335 weekday
## 1437     0 2012-10-05     2340 weekday
## 1438     0 2012-10-05     2345 weekday
## 1439     0 2012-10-05     2350 weekday
## 1440     0 2012-10-05     2355 weekday
## 1441     0 2012-10-06        0 weekend
## 1442     0 2012-10-06        5 weekend
## 1443     0 2012-10-06       10 weekend
## 1444     0 2012-10-06       15 weekend
## 1445     0 2012-10-06       20 weekend
```

2. *Make a panel plot containing a time series plot (i.e. type = "l") 
of the 5-minute interval (x-axis) and the average number of steps taken, 
averaged across all weekday days or weekend days (y-axis).*


```r
par(mfrow = c(2, 1))
weekendSubset <- subset(completeData, daytype == "weekend")
weekendAverages <- tapply(weekendSubset$steps, weekendSubset$interval, mean, na.rm=TRUE)
plot(names(weekendAverages), weekendAverages, type="l",
     main="weekend",
     xlab="5-Minute Interval", ylab="Average Number of Steps")
weekdaySubset <- subset(completeData, daytype == "weekday")
weekdayAverages <- tapply(weekdaySubset$steps, weekdaySubset$interval, mean, na.rm=TRUE)
plot(names(weekdayAverages), weekdayAverages, type="l",
     main="weekday",
     xlab="5-Minute Interval", ylab="Average Number of Steps")
```

![](PA1_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
