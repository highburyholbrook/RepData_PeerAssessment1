---
title: "Activity Monitoring Data"
author: "Brian Holbrook"
date: "Friday, November 13, 2015"
output: html_document
---

##Loading and Preprocessing Data##

First of all, we need to download the file to our working directory:

[Activity Monitoring Data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

Then after extracting the csv from the zip file, we can read the data into R


```r
data <- read.csv("activity.csv")
```

##What is mean total number of steps taken per day?##

To address this issue, we can begin by calculating the number of steps taken per day.  I will do this by calling the dplyr package.  I'm turning off messages and warnings since the dplyr package was built under a prior R version and those warnings add nothing to the quality of our analysis.


```r
library(dplyr)
```

I will then group my data by date, and then summarize the data to show the sum of the number of steps per day


```r
grouped <- data %>% group_by(date)
summed <- grouped %>% summarise(steps=sum(steps))
```

Next we will want a histogram of our data.  After researching the difference between a histogram and a barplot, I have decided to use the most basic histogram plot that we can possibly use


```r
hist(summed$steps)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

Next is a calculation of the mean and median total number of steps per day, as well as the reporting of this total.


```r
mean_median <- grouped %>% summarise(steps_mean=mean(steps), steps_median=median(steps))
print(tbl_df(mean_median), n=61)
```

```
## Source: local data frame [61 x 3]
## 
##          date steps_mean steps_median
## 1  2012-10-01         NA           NA
## 2  2012-10-02  0.4375000            0
## 3  2012-10-03 39.4166667            0
## 4  2012-10-04 42.0694444            0
## 5  2012-10-05 46.1597222            0
## 6  2012-10-06 53.5416667            0
## 7  2012-10-07 38.2465278            0
## 8  2012-10-08         NA           NA
## 9  2012-10-09 44.4826389            0
## 10 2012-10-10 34.3750000            0
## 11 2012-10-11 35.7777778            0
## 12 2012-10-12 60.3541667            0
## 13 2012-10-13 43.1458333            0
## 14 2012-10-14 52.4236111            0
## 15 2012-10-15 35.2048611            0
## 16 2012-10-16 52.3750000            0
## 17 2012-10-17 46.7083333            0
## 18 2012-10-18 34.9166667            0
## 19 2012-10-19 41.0729167            0
## 20 2012-10-20 36.0937500            0
## 21 2012-10-21 30.6284722            0
## 22 2012-10-22 46.7361111            0
## 23 2012-10-23 30.9652778            0
## 24 2012-10-24 29.0104167            0
## 25 2012-10-25  8.6527778            0
## 26 2012-10-26 23.5347222            0
## 27 2012-10-27 35.1354167            0
## 28 2012-10-28 39.7847222            0
## 29 2012-10-29 17.4236111            0
## 30 2012-10-30 34.0937500            0
## 31 2012-10-31 53.5208333            0
## 32 2012-11-01         NA           NA
## 33 2012-11-02 36.8055556            0
## 34 2012-11-03 36.7048611            0
## 35 2012-11-04         NA           NA
## 36 2012-11-05 36.2465278            0
## 37 2012-11-06 28.9375000            0
## 38 2012-11-07 44.7326389            0
## 39 2012-11-08 11.1770833            0
## 40 2012-11-09         NA           NA
## 41 2012-11-10         NA           NA
## 42 2012-11-11 43.7777778            0
## 43 2012-11-12 37.3784722            0
## 44 2012-11-13 25.4722222            0
## 45 2012-11-14         NA           NA
## 46 2012-11-15  0.1423611            0
## 47 2012-11-16 18.8923611            0
## 48 2012-11-17 49.7881944            0
## 49 2012-11-18 52.4652778            0
## 50 2012-11-19 30.6979167            0
## 51 2012-11-20 15.5277778            0
## 52 2012-11-21 44.3993056            0
## 53 2012-11-22 70.9270833            0
## 54 2012-11-23 73.5902778            0
## 55 2012-11-24 50.2708333            0
## 56 2012-11-25 41.0902778            0
## 57 2012-11-26 38.7569444            0
## 58 2012-11-27 47.3819444            0
## 59 2012-11-28 35.3576389            0
## 60 2012-11-29 24.4687500            0
## 61 2012-11-30         NA           NA
```

##What is the average daily activity pattern?##

Next is trying to determine the average daily activity pattern across all days.  What we are attempting here is to group the data by interval and average the number of steps by interval across all days.  Again, we will be using the dplyr package to group the data by interval.


```r
grouped <- data %>% group_by(interval)
summed <- grouped %>% summarise(steps=mean(steps, na.rm = TRUE))
```

Now we will plot the data as a time series of the average steps taken at the 5 minute time intervals


```r
plot(summed$interval, summed$steps, type = "l", main = "Average Daily Activity Pattern", xlab = "Time Interval", ylab = "Average Steps")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

Next we want to figure out which 5 minute interval contains the maximum number of steps on average.  Despite there being a probably more elegant solution, first I will just find the location of the max number of average steps.


```r
which.max(summed$steps)
```

```
## [1] 104
```

Now that I have the max location, I can ask for what interval that max location is at.


```r
summed[104,]
```

```
## Source: local data frame [1 x 2]
## 
##   interval    steps
## 1      835 206.1698
```

So the time interval 835 has the maximum number of average steps at right around 206.

##Imputing missing values##

Now we will want to calculate exactly how many rows in our data set have missing values.  


```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

Next we will want to impute the missing values in the steps column with the mean for that time interval.  We will begin by merging our 2 data sets so that we have a new data set that has the NA values, as well as the values for average steps for that specific time interval.


```r
new_data = merge(data, summed, by = "interval")
colnames(new_data)[4] <- "average_steps"
colnames(new_data)[2] <- "steps"
```

Once we have a merged set, then we will want to replace all NA's with the average value for that time interval.


```r
new_data = transform(new_data, steps = ifelse(is.na(steps), average_steps, steps))
```

Now we will want to create a histogram for the new data set for the total number of steps in each day.


```r
new_grouped <- new_data %>% group_by(date)
new_summed <- new_grouped %>% summarise(steps=sum(steps))
hist(new_summed$steps)
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 

The next calculation that we need to do is the mean and median number of steps for each day off of this new data set


```r
new_grouped <- new_data %>% group_by(date)
new_summed <- new_grouped %>% summarise(steps=mean(steps))
new_mean_median <- new_grouped %>% summarise(steps_mean=mean(steps), steps_median=median(steps))
print(tbl_df(new_mean_median), n=61)
```

```
## Source: local data frame [61 x 3]
## 
##          date steps_mean steps_median
## 1  2012-10-01 37.3825996     34.11321
## 2  2012-10-02  0.4375000      0.00000
## 3  2012-10-03 39.4166667      0.00000
## 4  2012-10-04 42.0694444      0.00000
## 5  2012-10-05 46.1597222      0.00000
## 6  2012-10-06 53.5416667      0.00000
## 7  2012-10-07 38.2465278      0.00000
## 8  2012-10-08 37.3825996     34.11321
## 9  2012-10-09 44.4826389      0.00000
## 10 2012-10-10 34.3750000      0.00000
## 11 2012-10-11 35.7777778      0.00000
## 12 2012-10-12 60.3541667      0.00000
## 13 2012-10-13 43.1458333      0.00000
## 14 2012-10-14 52.4236111      0.00000
## 15 2012-10-15 35.2048611      0.00000
## 16 2012-10-16 52.3750000      0.00000
## 17 2012-10-17 46.7083333      0.00000
## 18 2012-10-18 34.9166667      0.00000
## 19 2012-10-19 41.0729167      0.00000
## 20 2012-10-20 36.0937500      0.00000
## 21 2012-10-21 30.6284722      0.00000
## 22 2012-10-22 46.7361111      0.00000
## 23 2012-10-23 30.9652778      0.00000
## 24 2012-10-24 29.0104167      0.00000
## 25 2012-10-25  8.6527778      0.00000
## 26 2012-10-26 23.5347222      0.00000
## 27 2012-10-27 35.1354167      0.00000
## 28 2012-10-28 39.7847222      0.00000
## 29 2012-10-29 17.4236111      0.00000
## 30 2012-10-30 34.0937500      0.00000
## 31 2012-10-31 53.5208333      0.00000
## 32 2012-11-01 37.3825996     34.11321
## 33 2012-11-02 36.8055556      0.00000
## 34 2012-11-03 36.7048611      0.00000
## 35 2012-11-04 37.3825996     34.11321
## 36 2012-11-05 36.2465278      0.00000
## 37 2012-11-06 28.9375000      0.00000
## 38 2012-11-07 44.7326389      0.00000
## 39 2012-11-08 11.1770833      0.00000
## 40 2012-11-09 37.3825996     34.11321
## 41 2012-11-10 37.3825996     34.11321
## 42 2012-11-11 43.7777778      0.00000
## 43 2012-11-12 37.3784722      0.00000
## 44 2012-11-13 25.4722222      0.00000
## 45 2012-11-14 37.3825996     34.11321
## 46 2012-11-15  0.1423611      0.00000
## 47 2012-11-16 18.8923611      0.00000
## 48 2012-11-17 49.7881944      0.00000
## 49 2012-11-18 52.4652778      0.00000
## 50 2012-11-19 30.6979167      0.00000
## 51 2012-11-20 15.5277778      0.00000
## 52 2012-11-21 44.3993056      0.00000
## 53 2012-11-22 70.9270833      0.00000
## 54 2012-11-23 73.5902778      0.00000
## 55 2012-11-24 50.2708333      0.00000
## 56 2012-11-25 41.0902778      0.00000
## 57 2012-11-26 38.7569444      0.00000
## 58 2012-11-27 47.3819444      0.00000
## 59 2012-11-28 35.3576389      0.00000
## 60 2012-11-29 24.4687500      0.00000
## 61 2012-11-30 37.3825996     34.11321
```

The values tend to differ greatly from the first example.  There were all zeroes as the median values in the first example, and the average steps have been increased as well.

##Are there differences in activity patterns between weekdays and weekends?##

First we will want to create a new variable in our dataset that will tell whether the day is on a weekend or weekday.  We will accomplish this by creating a variable that first tells the day of the week, then whether that day is a weekday or weekend


```r
new_data$day <- weekdays(as.Date(new_data$date))
new_data$day <- replace(new_data$day, new_data$day == "Monday", "weekday")
new_data$day <- replace(new_data$day, new_data$day == "Tuesday", "weekday")
new_data$day <- replace(new_data$day, new_data$day == "Wednesday", "weekday")
new_data$day <- replace(new_data$day, new_data$day == "Thursday", "weekday")
new_data$day <- replace(new_data$day, new_data$day == "Friday", "weekday")
new_data$day <- replace(new_data$day, new_data$day == "Saturday", "weekend")
new_data$day <- replace(new_data$day, new_data$day == "Sunday", "weekend")
```

Next we will construct a panel plot of the 5 minute time series and the average number of steps taken on the weekend versus weekday using the lattice package.


```r
library(lattice)
xyplot(steps ~ interval|day, data = new_data, type = "l", xlab = "5 Minute Interval", ylab = "Average Steps", layout=c(1,2))
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png) 


