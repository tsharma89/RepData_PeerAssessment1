---
title: "RR_week2_course_project"
author: "Tanuj Sharma"
date: "April 30, 2018"
output: 
  html_document: 
    keep_md: yes
---



## Assignment (week-2)

### Loading & formatting data


```r
path <- "D:\\Tanuj\\other\\DMP\\5 Reproducible Research\\week2_project\\repdata_data_activity"
setwd(path)

fdata <- read.csv(file="activity.csv", header = TRUE)
fdata$date <- as.Date(fdata$date,"%Y-%m-%d")
```

### 1. What is mean total number of steps taken per day?

#### Calculate the total number of steps taken per day


```r
dailySteps <- aggregate(fdata$steps, by=list(Category=fdata$date), FUN=sum)
colnames(dailySteps)[2] <- "steps"
```
#### Make a histogram of the total number of steps taken each day

```r
hist(dailySteps$steps, 
     main = "Histogram of Steps per day", 
     xlab = "Total steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->


#### Calculate and report the mean and median of the total number of steps taken per day

```r
mean(dailySteps$steps, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(dailySteps$steps, na.rm=TRUE)
```

```
## [1] 10765
```


### 2. What is the average daily activity pattern?

#### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
dt.interval <- aggregate(fdata$steps, by=list(Category=fdata$interval), FUN=mean, na.rm=TRUE)

plot(dt.interval$Category, 
     dt.interval$x, 
     ylab="Average number of steps taken", 
     xlab="5-minute interval", 
     type="n")

lines(dt.interval$Category, 
      dt.interval$x, 
      type= "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

#### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maxSteps <- max(fdata$steps, na.rm=TRUE)
dtWithoutNA <- na.omit(fdata)
dtWithoutNA[dtWithoutNA$steps ==  maxSteps, ]
```

```
##       steps       date interval
## 16492   806 2012-11-27      615
```


### 3. Imputing missing values

#### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 

```r
sum(!is.na(fdata$steps))
```

```
## [1] 15264
```

#### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
aggdatabyInterval <- aggregate(fdata$steps, by=list(Category=fdata$interval), FUN=mean, na.rm=TRUE)
colnames(aggdatabyInterval)[2] <- "meansteps"
colnames(aggdatabyInterval)[1] <- "interval"
```

#### Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
fdata <- merge(fdata, aggdatabyInterval)
fdata$steps[is.na(fdata$steps)] = fdata$meansteps
```

```
## Warning in fdata$steps[is.na(fdata$steps)] = fdata$meansteps: number of
## items to replace is not a multiple of replacement length
```

```r
aggdata <- aggregate(fdata$steps, by=list(Category=fdata$date), FUN=sum)
colnames(aggdata)[2] <- "steps"
```

#### Make a histogram of the total number of steps taken each day

```r
hist(aggdata$steps, 
     main = paste("Histogram of" , "the total number of steps taken each day"), 
     xlab = "total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

#### Calculate and report the mean and median total number of steps taken per day.

```r
mean(aggdata$steps, na.rm=TRUE)
```

```
## [1] 9371.437
```


```r
median(aggdata$steps, na.rm=TRUE)
```

```
## [1] 10395
```

#### Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

##### Yes, after imputing the missing data, mean and median values have changed.

### 4. Are there differences in activity patterns between weekdays and weekends?

#### Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
fdata$day <- weekdays(as.Date(fdata$date))

weekenddt <- subset(fdata, fdata$day == "Saturday" | fdata$day == "Sunday")
weekdaydt <- subset(fdata, !(fdata$day == "Saturday" | fdata$day == "Sunday"))

aggdataweekend <- aggregate(weekenddt$steps, by=list(Category=weekenddt$interval), FUN=mean, na.rm=TRUE)
colnames(aggdataweekend)[2] <- "weekend"

aggdataweekday <- aggregate(weekdaydt$steps, by=list(Category=weekdaydt$interval), FUN=mean, na.rm=TRUE)
colnames(aggdataweekday)[2] <- "weekday"

aggdatamerge <- merge(aggdataweekday, aggdataweekend)
colnames(aggdatamerge)[1] <- "Interval"

library(reshape2)
```

```
## Warning: package 'reshape2' was built under R version 3.4.4
```

```r
mm <- melt(aggdatamerge,id.var="Interval")
```

#### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
library(lattice)
```

```
## Warning: package 'lattice' was built under R version 3.4.4
```

```r
xyplot(value~Interval|variable,data=mm,type="l", ylab = "Number of steps", 
       scales=list(y=list(relation="free")),
       layout=c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

