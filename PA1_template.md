---
title: "Reproducible Research: Peer Assessment 1"
author: "Vincent Phan"
date: "September 16, 2015"
output: 
  html_document: 
    self_contained: no
keep_md: true
---



###**Loading and Preprocessing the data**
```{r}
data <- read.csv('activity.csv')
data <- na.omit(data)
data$date <- as.Date(data$date)
head(data)

```


###**What is the mean total number of steps taken per day? 10766.19 steps**

```{r results='hide', message=FALSE, warning=FALSE}
library(dplyr)
```

####1.) Total Steps per day.
```{r}
gbDate <- group_by(data, date)
totalSteps <- summarise(gbDate, total_steps=sum(steps))
as.data.frame(totalSteps)
```

####2.) Histogram of total steps taken each day.
```{r}
hist(totalSteps$total_steps, xlab = "Total steps", main ="Histogram: Total Steps")
abline(v = mean(totalSteps$total_steps), col = "green", lwd = 10)
abline(v = median(totalSteps$total_steps), col = "red", lwd = 4)
legend("topright", c("mean=10766.19", "median=10765"), col=c("green", "red"), lty =c(1,1), lwd=2)

```


####3.) Mean and median of total steps taken per day.
```{r}
##mean
mean(totalSteps$total_steps)

##median
median(totalSteps$total_steps)
```


###**What is the average daily activity pattern?**

####1.) Time series plot using ggplot2
```{r}
library(ggplot2)
gbInterval <- group_by(data, interval)
totalStepsInterval <- summarize(gbInterval, average_steps = mean(steps))

intervalMaxSteps <- filter(totalStepsInterval, average_steps == max(totalStepsInterval$average_steps))$interval
maxSteps <- filter(totalStepsInterval, average_steps == max(totalStepsInterval$average_steps))$average_steps

ggplot(totalStepsInterval, aes(interval, average_steps)) + geom_line() + geom_vline(xintercept=intervalMaxSteps, color="blue") + annotate("text", x = 1230, y = 206.1698, label = "V line = 5 Min interval MAX steps", color = "blue", size = 3) + ylab("Average Steps") + xlab("Time Intervals") + labs(title ="Time Series Plot(ggplot2)")

```

####2.) The `r intervalMaxSteps` interval contains the maximum number of steps(`r maxSteps`).

```{r}
##Interval containing maximum number of steps
intervalMaxSteps <- filter(totalStepsInterval, average_steps == max(totalStepsInterval$average_steps))$interval
intervalMaxSteps
##Max number of steps
maxSteps <- filter(totalStepsInterval, average_steps == max(totalStepsInterval$average_steps))$average_steps
maxSteps
```

###**Imputing missing values
####1.) Calculating & reporting total number of missing values in the dataset.
```{r}
dataNA <- read.csv("activity.csv")
summary(dataNA)
## there are total of 2304 NA's
```

####2.) I will be defining mean of steps for each 5-minutes interval of each day. For missing values, I will be replacing
####each value with the mean of steps of that specific 5-minutes interval.


####3.) New dataset with all NA replaced

```{r}
# Setting up data: 1.) reading 2.) changing na to x 3.) arranging data by date
testOriginal <- read.csv('activity.csv')
testOriginal$date <- as.Date(testOriginal$date)
testOriginal[is.na(testOriginal)] <- "x"
testOriginal <- arrange(testOriginal, date)

# Splitting DF into 2 DFs. noNA only contains data without NA
noNA <- filter(testOriginal, steps != "x")
noNA$steps <- as.numeric(noNA$steps)
# allNA contains only NA
allNA <- filter(testOriginal, steps == "x")

# Getting mean of steps in each unique interval
meanInterval <- summarize(group_by(noNA, interval), average_steps=mean(steps))

# Combining average steps with allNA DF
newCol <- cbind(meanInterval$average_steps, allNA)

# cleaning revised allNA 
newCol <- select(newCol, -steps)
colnames(newCol)[1] <- "steps"

# combining the 2 DF that was split making new DF with 
newDF <- arrange(rbind(newCol, noNA), date)

```


```{r}
print(newDF[1:10,])
summary(newDF)
```

####4.) Histogram of total # of steps. Mean & Median of total steps taken per day. 
* Yes, value is the same for Mean
* No, median is now 10766.19 instead of 10765.
* There are little to no impact after imputing missing data

```{r}
NDtotalSteps <- summarize(group_by(newDF, date), total_steps = sum(steps))
as.data.frame(NDtotalSteps)

par(mfcol = c(1,2))

# new hist
hist(NDtotalSteps$total_steps, xlab = "Total steps", main ="Histogram: NEW Total Steps")
abline(v = mean(NDtotalSteps$total_steps), col = "green", lwd = 10)
abline(v = median(NDtotalSteps$total_steps), col = "red", lwd = 4)
legend("topright", c("mean=10766.19", "median=10766.19"), col=c("green", "red"), lty =c(1,1), lwd=2, cex=.6)

# Original hist
hist(totalSteps$total_steps, xlab = "Total steps", main ="Histogram: Origina Total Steps")
abline(v = mean(totalSteps$total_steps), col = "green", lwd = 10)
abline(v = median(totalSteps$total_steps), col = "red", lwd = 4)
legend("topright", c("mean=10766.19", "median=10765"), col=c("green", "red"), lty =c(1,1), lwd=2, cex = .60)

# Mean of total steps.
mean(NDtotalSteps$total_steps)

# Median of total steps.
median(NDtotalSteps$total_steps)
```

```{r}
length(NDtotalSteps$total_steps)
length(totalSteps$total_steps)
```


###Are there differences in activity patterns between weekdays and weekends? YES

```{r}
## Adding day to newDF
addDay <- data.frame(steps=newDF$steps, date=newDF$date, day=weekdays(newDF$date), interval=newDF$interval)
addDay$day <- as.character(addDay$day)
weekday <- c("Monday","Tuesday","Wednesday","Thursday","Friday")
weekend <- c("Saturday", "Sunday")

onlyWeekend <- filter(addDay, day == "Sunday" | day == "Saturday")
onlyWeekday <- filter(addDay, day == "Monday" | day == "Tuesday" | day == "Wednesday" | day =="Thursday"|day =="Friday")

averageStepsWeekend <- summarize(group_by(onlyWeekend, interval), average_steps=mean(steps))
averageStepsWeekend$Day <- "Weekend"

averageStepsWeekday <- summarize(group_by(onlyWeekday, interval), average_steps=mean(steps))
averageStepsWeekday$Day <- "Weekday"

averageDay <- rbind(averageStepsWeekend, averageStepsWeekday)

library(lattice)

xyplot(average_steps~interval|Day, data = averageDay, layout=c(1,2), type="l", ylab="Number of steps", xlab="Interval")
```