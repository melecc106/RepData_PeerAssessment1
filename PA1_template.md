#Reproducible Research Course Project 1
Melanie Eccles
=========================================================================================

##What is mean total number of steps taken per day?

Read the activity monotoring data set into R

```r
activity<-read.csv("activity.csv")
```

Change the class of the steps column to "numeric" 

```r
activity$steps<-as.numeric(activity$steps)
```

##What is mean total number of steps taken per day?

###Calculate the total number of steps taken per day

Aggregate the number of steps by the date to calculate the total number of steps per day

```r
steps_date<-aggregate(steps~date, activity, FUN=sum)
head(steps_date)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

```r
dim(steps_date)
```

```
## [1] 53  2
```

###Make a histogram of the total number of steps taken each day

```r
hist(steps_date$steps, breaks=53, main="Total Steps Taken per Day", 
     xlab="Daily Total Steps", col="blue")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

###Calculate and report the mean and median of the total number of steps taken per day

```r
mean(steps_date$steps)
```

```
## [1] 10766.19
```

```r
median(steps_date$steps)
```

```
## [1] 10765
```

##What is the average daily activity pattern?

###Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

Aggregate the average number of steps by the interval

```r
steps_interval<-aggregate(steps~interval, activity, FUN=mean)
```

Make the plot

```r
plot(steps_interval$interval, steps_interval$steps, type="l",
     main = "Average Daily Activity Pattern", 
     xlab="5 min interval", ylab="Average Steps")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

###Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
steps_interval[which.max(steps_interval$steps), ]
```

```
##     interval    steps
## 104      835 206.1698
```

##Imputing missing values
###Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(activity))
```

```
## [1] 2304
```

###Devise a strategy for filling in all of the missing values in the dataset and assign to a new data frame. 
Replace the missing values with the mean for that interval and assign to a new data frame

```r
activity2<-activity
missing<-is.na(activity2$steps)
mean_day<-tapply(activity$steps, activity$interval, mean, na.rm=TRUE)
activity2$steps[missing]<-mean_day[as.character(activity2$interval[missing])]
```

###Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.


```r
newsteps_date<-aggregate(steps~date, activity2, FUN=sum)
hist(newsteps_date$steps, breaks=53, main="Total Steps Taken per Day", xlab="Daily Total Steps", col="blue")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)


```r
mean(newsteps_date$steps)
```

```
## [1] 10766.19
```


```r
median(newsteps_date$steps)
```

```
## [1] 10766.19
```

The mean and median did not change by much after replacing the missing values with the mean for each interval. This is because by imputing the mean we have more data close to the mean.

After the missing values were imputed with the mean for each interval, the plot had higher frequency counts near the mean value because more days were shown to have the mean value

##Are there differences in activity patterns between weekdays and weekends?

###Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

Load the dplyr package to add a column that was mutated to show the weektype

```r
library(dplyr)
activity2$date<-as.Date(activity2$date)
activity2<- activity2 %>%mutate(weektype=ifelse(weekdays(activity2$date)=="Saturday"|
                          weekdays(activity2$date)=="Sunday", "Weekend", "Weekday"))
activity2$weektype<-as.factor(activity2$weektype)
head(activity2)
```

```
##       steps       date interval weektype
## 1 1.7169811 2012-10-01        0  Weekday
## 2 0.3396226 2012-10-01        5  Weekday
## 3 0.1320755 2012-10-01       10  Weekday
## 4 0.1509434 2012-10-01       15  Weekday
## 5 0.0754717 2012-10-01       20  Weekday
## 6 2.0943396 2012-10-01       25  Weekday
```


###Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

Aggregate the number of steps by interval and weekday

```r
steps_interval<-aggregate(steps~interval+weektype, activity2, FUN=mean)
```

Create a plot using the lattice package

```r
library(lattice)
xyplot(steps~interval|weektype, data=steps_interval, 
       layout=c(1,2), type="l", main="Average Number of Steps Taken per Interval by Day",
       xlab="Interval", ylab="Average Steps")
```

![plot of chunk unnamed-chunk-18](figure/unnamed-chunk-18-1.png)

