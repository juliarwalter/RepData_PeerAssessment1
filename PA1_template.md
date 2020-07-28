---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: yes
    
---

## Loading and preprocessing the data
Read the data and unzip the file


```r
# Read the data
data <- read.csv(unz("activity.zip", "activity.csv"), quote="\"", sep=",")
```

Required packages


```r
library(ggplot2)
library(plyr)
library(dplyr)
```

Processing the data


```r
data$as.date<- as.POSIXct(data$date, format="%Y-%m-%d")
```

## What is mean total number of steps taken per day?
Histogram of the total number of steps taken each day


```r
# Plot 1
hist(tapply(data$steps, data$as.date, sum), main = "Histogram total number of steps taken each day", xlab = "Steps")
```

![](PA1_template_files/figure-html/histogram%20-%20plot.1-1.png)<!-- -->

The mean and median number of steps


```r
steps_per_day <- aggregate(data$steps ~ data$date, FUN = sum)
colnames(steps_per_day)<- c("date", "steps")
mean(steps_per_day$steps)
```

```
## [1] 10766.19
```

```r
median(steps_per_day$steps)
```

```
## [1] 10765
```
## What is the average daily activity pattern?
Time series plot of the average number of steps taken


```r
average <- ddply(data[!is.na(data$steps),], "interval", summarize, mean = mean(steps))
# Plot 2
ggplot(average, aes( x=interval, y=mean)) + geom_line() + xlab("Interval") + ylab("Average number of steps") + ggtitle("Average daily activity pattern")
```

![](PA1_template_files/figure-html/time%20series%20-%20plot.2-1.png)<!-- -->

The 5-minute interval that, on average, contains the maximum number of steps


```r
maximum <- filter(na.omit(average), mean == max(mean))
 maximum[1]
```

```
##   interval
## 1      835
```

## Imputing missing values
Number of NA's value


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
##     as.date                   
##  Min.   :2012-10-01 00:00:00  
##  1st Qu.:2012-10-15 18:00:00  
##  Median :2012-10-31 12:00:00  
##  Mean   :2012-10-31 04:20:00  
##  3rd Qu.:2012-11-15 06:00:00  
##  Max.   :2012-11-30 00:00:00  
##  NA's   :288
```

Strategy for imputing missing data: the mean of the 5-minute interval


```r
# Calculate the mean of the 5-minute interval; replace NA value
average_2 <- ddply(na.omit(data), .(interval), summarize, steps = mean(steps))
na.data <- data[is.na(data$steps),]
data_2 <- merge(average_2, na.data, by=c("interval"))
data_2 = data_2 %>%
         select(steps.x, date, interval, as.date)
colnames(data_2)<- c("steps", "date", "interval", "as.date")
data_3 <- rbind(data[!is.na(data$steps),], data_2)
```


```r
# Calculate the total number of steps taken per day
# Plot 3
par(mfrow=c(2,1))
hist(tapply(data$steps, data$as.date, sum), main = "Histogram total number of steps taken ", xlab = "Steps", ylim = c(0,50))
hist(tapply(data_3$steps, data_3$as.date, sum), main = "Histogram total number of steps taken- Imputed", xlab = "Steps", ylim = c(0,50), col = "blue")
```

![](PA1_template_files/figure-html/histogram%20-%20plot.3-1.png)<!-- -->

## Are there differences in activity patterns between weekdays and weekends?


```r
data_3$weekdays <- weekdays(as.Date(data$as.date))
data_3$datetype <- ifelse(data_3$weekdays %in% c("sábado", "domingo"), "Weekend", "Weekday")
 
average_2 <-subset(data_3, data_3$datetype == "Weekend")
average_2 <- ddply(average_2, "interval",  summarize, mean = mean(steps))
 
average_4 <-subset(data_3, data_3$datetype == "Weekday")
average_4 <- ddply(average_4, "interval",  summarize, mean = mean(steps))
```


```r
par(mfrow=c(1,1))
# Plot 4
plot(average_2$interval, average_2$mean, type = "l", ylab = "Average number of steps", xlab = "Interval", main = "Average daily activity pattern" , col = 2)
points(average_4$interval, average_4$mean, type = "l", col = 20)
legend("topright", c("Weekend", "Weekday"),lty=c(1,1), col= c(2,20))
```

![](PA1_template_files/figure-html/-%20plot%204-1.png)<!-- -->

