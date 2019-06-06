---
title: "Course Project 1 - Reproducible Research"
author: "Sanket Bambodkar"
date: "6 June 2019"
output: html_document
---
Summary
--------
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

Questions to be answered:   

1. What is mean total number of steps taken per day?   
2. What is the average daily activity pattern?   
3. Are there differences in activity patterns between weekdays and weekends?   

Loading the Data
----------------
```{r warning=FALSE, error=FALSE, echo=TRUE}
library(ggplot2)
data <- read.csv("activity.csv")
str(data)
summary(data)
```
We can see that date variable is of class factor. We will first convert it's class to required format.   
```{r warning=FALSE, error=FALSE, echo=TRUE}
data$date <- as.POSIXct(data$date, "%Y-%m-%d")
str(data)
```
Now we have our date variable in required format.     

What is mean total number of steps taken per day? 
------------------------------------------------
Here we will aggregate the sum of the steps taken on a particular date and assign it to a new variable.    
```{r warning=FALSE, error=FALSE, echo=TRUE}
steps_per_day <- aggregate(data$steps, by = list(data$date), FUN = sum, na.rm = TRUE)
names(steps_per_day) <- c("Date", "Steps")
steps_per_day
```
Now we will make an histogram using the object steps_per_day.  
```{r warning=FALSE, error=FALSE, echo=TRUE}
hist(steps_per_day$Steps, main = "Total number of steps taken per day", xlab = "Steps taken per day", col = "yellow", ylim = c(0,20), breaks = seq(0,25000, by=2000))
```
Mean of the total number of steps taken per day
```{r warning=FALSE, error=FALSE, echo=TRUE}
mean(steps_per_day$Steps)
```
Median of the total number of steps taken per day
```{r warning=FALSE, error=FALSE, echo=TRUE}
median(steps_per_day$Steps)
```


What is the average daily activity pattern?
-------------------------------------------
Here we will aggregate the mean of the steps taken at a particular interval in a day and assign it to a new variable. 
```{r warning=FALSE, error=FALSE, echo=TRUE}
steps_per_interval <- aggregate(data$steps, by = list(data$interval), FUN = mean, na.rm = TRUE)
names(steps_per_interval) <- c("Interval", "Steps")
steps_per_interval
```
Now we will make a time series plot using the object steps_per_interval
```{r warning=FALSE, error=FALSE, echo=TRUE}
plot(steps_per_interval$Interval, steps_per_interval$Steps, type = "l", col="black", lwd = 2, xlab="Interval", ylab="Average number of steps", main="Average number of steps per intervals")
```
Interval which has the maximum average for the number of steps taken
```{r warning=FALSE, error=FALSE, echo=TRUE}
steps_per_interval[which.max(steps_per_interval$Steps), ]$Interval
```


Imputing Missing Values
----------------------
There are lots of rows in the dataset where data for **steps** in NA. Let's check out how many of these rows are present in the data
```{r warning=FALSE, error=FALSE, echo=TRUE}
sum(is.na(data$steps))
```
In place of these missing values, we will substitute the average for that interval of the day.    
```{r warning=FALSE, error=FALSE, echo=TRUE}
imputed_steps <- steps_per_interval$Steps[match(data$interval, steps_per_interval$Interval)]
imputed_data <- transform(data, steps = ifelse(is.na(data$steps), yes = imputed_steps, no = data$steps))
```
Now we will aggregate the sum of the steps taken on a particular date and assign it to a new variable and make a histogram of total number of steps taken per day.
```{r warning=FALSE, error=FALSE, echo=TRUE}
imputed_steps_per_day <- aggregate(imputed_data$steps, by = list(imputed_data$date), FUN = sum)
names(imputed_steps_per_day) <- c("Date", "Steps")
imputed_steps_per_day
hist(imputed_steps_per_day$Steps, main = "Total number of steps taken per day (Imputed Data", xlab = "Steps taken per day", col = "yellow", ylim = c(0,25), breaks = seq(0,25000, by=2000))
```
Mean of the total number of steps taken per day (Imputed Data)
```{r warning=FALSE, error=FALSE, echo=TRUE}
mean(imputed_steps_per_day$Steps)
```
Median of the total number of steps taken per day
```{r warning=FALSE, error=FALSE, echo=TRUE}
median(imputed_steps_per_day$Steps)
```

Are there differences in activity patterns between weekdays and weekends? 
-------------------------------------------------------------------------

Now, we will create a factor variable named day type depending on whether the date falls on a weekday or a weekend.
```{r warning=FALSE, error=FALSE, echo=TRUE}
weekday <- weekdays(data$date)
unique(weekday)
datetype <- sapply(data$date, function(x) {
        if (weekdays(x) == "Saturday" | weekdays(x) =="Sunday") 
                {y <- "Weekend"} else 
                {y <- "Weekday"}
                y})
```
Now we will bind datetype to our original data
```{r warning=FALSE, error=FALSE, echo=TRUE}
data <- cbind(data,datetype)
```
Now, we will aggregate steps taken at a particular interval of a day differently for weekdays and weekends and make 2 different time series plot for them.
```{r warning=FALSE, error=FALSE, echo=TRUE}
datetype_steps_per_interval <- aggregate(data$steps, by = list(data$interval,data$datetype), FUN = mean, na.rm = TRUE)
names(datetype_steps_per_interval) <- c("Interval", "Daytype", "Steps")
datetype_steps_per_interval
plot<- ggplot(datetype_steps_per_interval, aes(x = Interval , y = Steps, color = Daytype))
plot <- plot + geom_line() 
plot <- plot + labs(title = "Average daily steps by type of day", x = "Interval", y = "Average number of steps")
plot <- plot + facet_wrap(~Daytype, ncol = 1, nrow=2)
plot
```
