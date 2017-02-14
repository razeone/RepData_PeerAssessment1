---
title: "Reproducible Research: Peer Assessment 1"
author: "Jorge Alcaraz"
date: "13/2/2017"
output: html_document
---

# This is the week's 2 assignment for reproducible research
First we load ggplot2, and libraries needed for the analysis.

```{r}
library(ggplot2)
library(lattice)
```

## Loading and preprocessing the data

### Then we extract load and process the text data:

We extract the data into the data directory and load it to an `activity` variable.

```{r}
unzip("activity.zip", exdir = "data/")
activityRaw <- read.csv("data/activity.csv", colClasses = c("integer", "Date", "factor"))
activityRaw$month <- as.numeric(format(activityRaw$date, "%m"))
activity <- na.omit(activityRaw)
dim(activity)
```

### Histogram of the total number of steps taken each day

Let's aggregate the data by day, in this case since we need an array or a list to create a 2D histogram, we'll use `tapply` instead of `aggregate`.

```{r}
stepsEachDay <- tapply(activity$steps, activity$date, sum, na.rm=TRUE)
qplot(stepsEachDay, geom = "histogram", binwidth = max(stepsEachDay/10), xlab = "Total steps each day", ylab = "Frequency", main = "Total steps taken each day")
```

![](figure/plot1.png) 

## What is mean total number of steps taken per day?

### Mean and median of the steps taken each day

```{r}
stepsMean <- mean(stepsEachDay)
stepsMedian <- median(stepsEachDay)
```

The observations show a median of steps taken each day of `r print(stepsMedian)` and a mean of `r print(stepsMean)`


## What is the average daily activity pattern?

* Time series plot of the average number of steps taken

```{r}
averageSteps <- aggregate(activity$steps, list(interval = as.numeric(as.character(activity$interval))), FUN = "mean")
names(averageSteps)[2] <- "meanOfSteps"

ggplot(averageSteps, aes(interval, meanOfSteps)) + geom_line(color = "steelblue", size = 0.8) + labs(title = "Time Series Plot of the 5-minute Interval", x = "5-minute intervals", y = "Average Number of Steps Taken")

```

![](figure/plot2.png) 

* The 5-minute interval that, on average, contains the maximum number of steps

```{r}
maxInterval <- averageSteps[which(averageSteps$meanOfSteps == max(averageSteps$meanOfSteps)), 1]
```


## Inputing missing values

* Get the number of missing values

```{r}
nMissingValues <- length(which(is.na(activityRaw$steps)))
```

There are `r print(nMissingValues)` missing values.

* Devise a strategy for filling all of the missing values in the dataset

We're going to replace missing values with the mean for that 5 minute interval

```{r}
newAverage <- activityRaw
for (i in 1:nrow(newAverage)) {
    if (is.na(newAverage$steps[i])) {
        newAverage$steps[i] <- averageSteps[which(newAverage$interval[i] == averageSteps$interval), ]$meanOfSteps
    }
}

head(newAverage)
sum(is.na(newAverage))
```
* Histogram of the total number of steps taken each day after missing values are imputed

```{r}
ggplot(newAverage, aes(date, steps)) + geom_bar(stat = "identity",
                                             colour = "steelblue",
                                             fill = "steelblue",
                                             width = 0.7) + facet_grid(. ~ month, scales = "free") + labs(title = "Histogram of Total Number of Steps Taken Each Day (no missing data)", x = "Date", y = "Total number of steps")

```

![](figure/plot3.png) 


## Are there differences in activity patterns between weekdays and weekends?

Create a new factor for each day of the week

```{r}
wkdy <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
newAverage$dayType = ifelse(weekdays(newAverage$date) %in% wkdy, "weekday", "weekend")
```

### Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```{r}
# aggregate the average number of steps by interval and dayType
result <- aggregate(newAverage$steps, list(interval = newAverage$interval,
                                       dayType = newAverage$dayType), mean)
# Create a time series plot of interval vs avg steps
xyplot(x ~ interval | dayType, result, layout=c(1,2), type = "l", ylab = "Number of Steps")
```

![](figure/plot4.png) 

There's just a small difference between weekdays and weekends on average.

