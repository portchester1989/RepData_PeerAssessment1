# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
library(data.table)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:data.table':
## 
##     between, last
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
data_exp=na.omit(fread('activity.csv'))
data_exp=transform(data_exp,date=as.Date(date))
```
## What is mean total number of steps taken per day?

```r
sum_steps=tapply(data_exp$steps,as.factor(data_exp$date),sum)
hist(sum_steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
dev.copy(png,'figure/Histogram_of_step1.png')
```

```
## quartz_off_screen 
##                 3
```

```r
dev.off()
```

```
## quartz_off_screen 
##                 2
```

```r
my_mean=mean(sum_steps)
my_median=median(sum_steps)
```
The mean of total number of steps taken per day is 1.0766189\times 10^{4}.  The median of total number of steps taken per day is 10765

## What is the average daily activity pattern?

```r
by_interval=summarise(group_by(data_exp,interval),number_of_steps=mean(steps))
with(by_interval,plot(interval,number_of_steps,type='l',xlab='interval',ylab='average steps',main='steps vs interval'))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
max_value=by_interval[which.max(by_interval$number_of_steps),]$interval
```
The interval containing the highest value is 835  


## Imputing missing values

```r
data_exp_new=fread('activity.csv',data.table = F)
data_exp_new=transform(data_exp_new,date=as.Date(date))
na_numbers=length(data_exp$steps[is.na(data_exp$steps)])
returned=data.frame(steps=numeric(0),date=character(0),interval=numeric(0))
returned=transform(returned,date=as.Date(date))
splited=split(data_exp_new,data_exp_new$interval)
for (y in splited){
   steps<-y$steps
   mean_value<-mean(steps,na.rm=T)
   y$steps[is.na(steps)]<-mean_value
   y$date<-as.Date(y$date)
   returned=rbind(returned,y)
}
returned$date=as.Date(data_exp_new$date)
data_exp_new=returned
sum_steps=tapply(data_exp_new$steps,as.factor(data_exp_new$date),sum)
hist(sum_steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
my_mean_new=mean(sum_steps)
my_median_new=median(sum_steps)
```
The mean of total number of steps taken per day is 1.0766189\times 10^{4}.  The median of total number of steps taken per day is 1.0310981\times 10^{4}


## Are there differences in activity patterns between weekdays and weekends?

```r
library(lattice)
new_data=mutate(data_exp_new,day=ifelse(weekdays(date) %in% c('土曜日','日曜日'),'weekend','weekday'))
new_data$day=factor(new_data$day)
by_interval_day=summarise(group_by(new_data,interval,day),number_of_steps=mean(steps))
xyplot(number_of_steps~as.numeric(interval)|day,data=by_interval_day,type='l',xlab='interval',ylab='average steps',main='steps vs day',layout=c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
