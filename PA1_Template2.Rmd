---
output: html_document
---
Peer Assessment 1

# Load Packages
```{r}
library(ggplot2)
library(scales)
library(Hmisc)

```
# Loading and preprocessing the data.
```{r}
ActivityData <- read.csv("./Data/activity.csv")
head(ActivityData)

```

# What is mean total number of steps taken per day?
```{r}
StepsByDay <- tapply(ActivityData$steps, ActivityData$date, sum, na.rm=TRUE)
```
## 1.Make a histogram of the total number of steps taken each day
```{r}
qplot(StepsByDay, xlab='Total Steps per Day', ylab='Frequency', binwidth=500)
```


## 2. Calculate and report mean and median total number of steps taken by day.
```{r}
MeanStepsByDay <- mean(StepsByDay)
MedianStepsByDay <- median(StepsByDay)

MeanStepsByDay
MedianStepsByDay
```

# What is the average daily activity pattern?
```{r}
AVGStepsPerTime <- aggregate(x=list(MeanSteps=ActivityData$steps), by=list(interval=ActivityData$interval), FUN=mean, na.rm= TRUE)
```

## 1. Make a time series plot of the 5-minute interval
```{r}
ggplot(data=AVGStepsPerTime, aes(x=interval, y=MeanSteps)) +
  geom_line() +
  xlab('5 minute interval')+
  ylab('AVG Number of Steps Taken')
```

## 2. Which interval contains the maximum number of steps, AVG.
```{r}
MaxSteps <- which.max(AVGStepsPerTime$MeanSteps)

MaxStepsTime <- gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", AVGStepsPerTime[MaxSteps, 'interval'])
MaxStepsTime
```


# Imputing missing values

## 1. Calculate and report the total number of missing values in the dataset.
```{r}
MissingValues <- length(which(is.na(ActivityData$steps)))
MissingValues
```
## 2. Devise a strategy for filling in all of the missing values in the dataset.

## 3. Create a new dataset that is equal ti the original dataset but with the missing data filled in.
```{r}
ActivityDataImputed <- ActivityData

ActivityDataImputed$steps <- impute(ActivityData$steps, fun=mean)
```

## 4. Make a histogram of the total number of steps taken each day.
```{r}
StepsByDayImputed <- tapply(ActivityDataImputed$steps, ActivityDataImputed$date, sum)

qplot(StepsByDayImputed, xlab='Total Steps per Day', ylab='Frequency', binwidth=500)
```


## Calculate and report the mean and median total number of steps taken per day.
```{r}
MeanStepsByDayImuted <- mean(StepsByDayImputed)
MedianStepsByDayImputed <- median(StepsByDayImputed)

MeanStepsByDayImuted
MedianStepsByDayImputed
```
# Are there differences in activity patterns between weekdays and weekends?

## 1. Create a new factor variable in the dataset with two levels ??? ´weekday¡ and ´weekend¡ indicating whether a given date is a weekday or weekend day.
```{r}
ActivityDataImputed$DateType <- ifelse(as.POSIXlt(ActivityDataImputed$date)$wday %in% c(0,6), 'weekend', 'weekday')
```
## 2. Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```{r}
AVGStepsPerTimeImputed <- aggregate(steps ~ interval + DateType, data = ActivityDataImputed, mean)

ggplot(AVGStepsPerTimeImputed, aes(interval, steps)) +
  geom_line() +
  facet_grid(DateType ~ .) +
  xlab("5 Minute Interval") +
  ylab("AVG Number of Steps")
```
