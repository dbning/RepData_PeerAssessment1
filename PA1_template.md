---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:
Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

The variables included in this dataset are:

- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
- date: The date on which the measurement was taken in YYYY-MM-DD format
- interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.


## Loading and preprocessing the data


```r
if(!file.exists("activity.zip")){
      download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", "activity.zip")
}
if(!file.exists("activity.csv")){
      unzip("activity.zip")
}
activity <- read.csv("activity.csv", stringsAsFactors = FALSE)
```

## What is mean total number of steps taken per day?


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)

s <- group_by(activity, date) %>% summarize(totalSteps = sum(steps, na.rm = TRUE))

qplot(totalSteps, data = s, geom = "histogram", xlab = "total steps taken per day", main = "Histogram of the total steps taken each day")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
meanSteps <- summarize(s, meanSteps = mean(totalSteps))
mediSteps <- summarize(s, medianSteps = median(totalSteps))
```

Mean number of steps taken each day is : 9354.2295082.    
Median number of steps taken each day is : 10395.



## What is the average daily activity pattern?

```r
#avgDaily <- tapply(activity$steps, activity$date, mean)

#plot(avgDaily, type = "l", xlab = "date",  ylab = "5-minute interval mean")

i <- group_by(activity, interval) %>% summarize(itervalMean = mean(steps, na.rm = TRUE))
qplot(interval, itervalMean, data = i, geom = "line", xlab = "5-minute interval ", ylab = "mean steps", main = "The 5-minute interval steps on average")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
maxInterval <- i[which.max(i$itervalMean),]$interval
```

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?  Answer: 835


## Imputing missing values

```r
naNum <- length(activity$steps[is.na(activity$steps)])
```
The total number of Na values is 2304.

Now, use the mean of the total '5-minute interval' ... to fill the Na's and make a histogram of the total number of steps taken each day 


```r
mergedActivity <- merge(activity, i, by.x = 'interval', by.y = 'interval')

sub1 <- subset(mergedActivity, is.na(mergedActivity$steps))
sub2 <- subset(mergedActivity, !is.na(mergedActivity$steps))

sub1[,'steps'] <- sub1[,'itervalMean']

r <- rbind(sub1, sub2)
r$date <- as.Date(r$date)

newActivity <- select(r, 'steps', 'date', 'interval')
newActivity <- arrange(newActivity, interval, date)

f <- group_by(newActivity, date) %>% summarize(totalSteps = sum(steps, na.rm = TRUE))

qplot(totalSteps, data = f, geom = "histogram", xlab = "total steps taken per day", main = "Histogram of the total number of steps taken each day after Na's filled")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
meanStepsAfterFilled <- summarize(f, meanSteps = mean(totalSteps))
mediStepsAfterFilled <- summarize(f, medianSteps = median(totalSteps))
```

After Na's filledd, mean number of steps taken each day is : 1.0766189\times 10^{4}. median number of steps taken each day is : 1.0766189\times 10^{4}.


## Are there differences in activity patterns between weekdays and weekends?

```r
newActivity$weekday <- as.factor(weekdays(as.Date(activity$date)))

weekDaysActivity <- subset(newActivity, !newActivity$weekday %in% c("星期六", "星期天"))
weekEndsActivity <- subset(newActivity, newActivity$weekday %in% c("星期六", "星期天"))

weekDaysActivity$daytype <- "weekdays"
weekEndsActivity$daytype <- "weekends"

l1 <- rbind(weekDaysActivity, weekEndsActivity)
l2 <- group_by(l1, daytype, interval) %>% summarize(meanSteps = mean(steps))

qplot(interval, meanSteps, data = l2, geom = "line", facets = daytype~., main = "5-minute interval mean")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

