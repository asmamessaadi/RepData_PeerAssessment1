#1. Loading and preprocessing the data
dat<- read.csv("activity.csv", header = TRUE, colClasses = c("numeric", "Date", "numeric")
##2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
dat$interval <- strptime(gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", dat$interval), format='%H:%M')
```
#3. What is mean total number of steps taken per day?
## Calculate the total number of steps taken per day

```r
library(ggplot2)
steps<- tapply (dat$steps, dat$date, sum, na.rm =TRUE)
```
##Calculate the total number of steps taken per day

```r
qplot(steps, xlab='Total steps per day', ylab='Frequency using binwith 500', binwidth=500)
```

![plot of chunk unnamed-chunk-28](figure/unnamed-chunk-28-1.png)
##3. Calculate and report the mean and median total number of steps taken per day

```r
mean_steps <- mean(steps)
median_steps <- median(steps)
mean_steps
```

```
## [1] 9354.23
```

```r
median_steps
```

```
## [1] 10395
```
#What is the average daily activity pattern?
##Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
library(dyplr)
```

```
## Error in library(dyplr): there is no package called 'dyplr'
```

```r
actInterval <- dat %>% group_by(interval) %>% summarise(meanSteps = mean(steps,           na.rm = TRUE))
```

```
## Error in grouped_df_impl(data, unname(vars), drop): Column `interval` is of unsupported class POSIXlt/POSIXt
```

```r
head(actInterval)
```

```
## # A tibble: 6 x 2
##   interval meanSteps
##      <dbl>     <dbl>
## 1        0    1.72  
## 2        5    0.340 
## 3       10    0.132 
## 4       15    0.151 
## 5       20    0.0755
## 6       25    2.09
```

```r
p <- ggplot(data = dat, mapping = aes(x = interval, y = meanSteps)) + 
    geom_line() + scale_x_continuous("Day Interval", breaks = seq(min(actInterval$interval), 
                                                                  max(actInterval$interval), 100)) + scale_y_continuous("Average Number of Steps") + 
    ggtitle("Average Number of Steps Taken by Interval")
p2
```

```
## Warning: Removed 2 rows containing missing values (geom_path).
```

![plot of chunk unnamed-chunk-30](figure/unnamed-chunk-30-1.png)

#Imputing missing values
##1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NA\

```r
missingV <- is.na(dat$steps)
```
##Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
fill.value <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps))
        filled <- c(steps)
    else
        filled <- (averages[averages$interval==interval, "steps"])
    return(filled)
}
filled.data <- dat
filled.dat$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)
```

```
## Error in (function (steps, interval) : object 'averages' not found
```
#Are there differences in activity patterns between weekdays and weekends?

```r
total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![plot of chunk unnamed-chunk-33](figure/unnamed-chunk-33-1.png)

```r
mean(total.steps)
```

```
## [1] NA
```

```r
median(total.steps)
```

```
## <NA> 
##   NA
```

#Are there differences in activity patterns between weekdays and weekends?

```r
weekday.or.weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}
#filled.data$date <- as.Date(filled.data$date)
#filled.data$day <- sapply(filled.data$date, FUN=weekday.or.weekend)
```
