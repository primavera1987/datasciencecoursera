---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

###read activity.csv


```r
activity = read.csv('activity.csv')
```

###show details 


```r
print(head(activity))
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
print(summary(activity))
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

```r
print(str(activity))
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
## NULL
```



## What is mean total number of steps taken per day?

###Calculate the total number of steps taken per day 


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
sub_activity = subset(activity,!is.na(steps),select = c(steps,date))
STPD = summarise(group_by(sub_activity,date),steps = sum(steps))
print(STPD)
```

```
## # A tibble: 53 x 2
##    date       steps
##    <fct>      <int>
##  1 2012-10-02   126
##  2 2012-10-03 11352
##  3 2012-10-04 12116
##  4 2012-10-05 13294
##  5 2012-10-06 15420
##  6 2012-10-07 11015
##  7 2012-10-09 12811
##  8 2012-10-10  9900
##  9 2012-10-11 10304
## 10 2012-10-12 17382
## # ... with 43 more rows
```

```r
qplot(steps,data = STPD)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/temp3-1.png)<!-- -->
### figure1

###Calculate the mean & median number of steps taken per day 


```r
sub_M_activity = subset(activity,!is.na(steps),select = c(steps,date))
STPD = summarise(group_by(sub_activity,date),steps = sum(steps))
print (mean(STPD$steps))
```

```
## [1] 10766.19
```

```r
print (median(STPD$steps))
```

```
## [1] 10765
```

## What is the average daily activity pattern?


```r
sub_interval_activity = subset(activity,!is.na(steps),select = c(steps,interval))
STI = summarise(group_by(sub_interval_activity,interval),steps = mean(steps))
qplot(interval,steps,data = STI,type = '1')
```

```
## Warning: Ignoring unknown parameters: type
```

![](PA1_template_files/figure-html/temp5-1.png)<!-- -->
### figure2

```r
ggplot(STI, aes(interval, steps)) + geom_line()
```

![](PA1_template_files/figure-html/tempx-1.png)<!-- -->
### figure3

```r
STI[STI$steps == max(STI$steps),]
```

```
## # A tibble: 1 x 2
##   interval steps
##      <int> <dbl>
## 1      835  206.
```



## Imputing missing values
###Calculate and report the total number of missing values in the dataset 

```r
library(dplyr)
library(ggplot2)
sub_activity = subset(activity,is.na(steps),select = c(steps,date))
print(length(sub_activity$steps))
```

```
## [1] 2304
```
### NA values change to main


```r
data = read.csv('activity.csv')
sub_activity = subset(data,!is.na(steps),select = c(steps,date))
data$steps[is.na(data$steps)] = mean(sub_activity$steps)
STPD = summarise(group_by(data,date),steps = sum(steps))
print(STPD)
```

```
## # A tibble: 61 x 2
##    date        steps
##    <fct>       <dbl>
##  1 2012-10-01 10766.
##  2 2012-10-02   126 
##  3 2012-10-03 11352 
##  4 2012-10-04 12116 
##  5 2012-10-05 13294 
##  6 2012-10-06 15420 
##  7 2012-10-07 11015 
##  8 2012-10-08 10766.
##  9 2012-10-09 12811 
## 10 2012-10-10  9900 
## # ... with 51 more rows
```

```r
qplot(steps,data = STPD)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/temp7-1.png)<!-- -->
### figure4

## Are there differences in activity patterns between weekdays and weekends?


```r
data = read.csv('activity.csv')
weekdays = weekdays(as.Date(data$date))
i = 1 
weekend = c()
for(day in weekdays){
    if(day == "Sunday"){
        weekend[i] = TRUE
    }
    else{
        weekend[i] = FALSE
    }
    i = i + 1
}
days =data.frame(weekdays,weekend)
data = cbind(data,days)
sub_activity = subset(data,!is.na(steps),select = c(steps,date))
STPD = summarise(group_by(sub_activity,date),steps = sum(steps))
qplot(x = interval,	y = steps,	data = data,color = factor(weekend))	
```

```
## Warning: Removed 2304 rows containing missing values (geom_point).
```

![](PA1_template_files/figure-html/temp8-1.png)<!-- -->
### figure5
