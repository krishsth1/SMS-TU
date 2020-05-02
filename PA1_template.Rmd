---
title: ''
author: "Krishna Kumar Shrestha"
date: "5/2/2020"
output: html_document
---


# 1 Code for reading in the dataset and/or processing the data

```{r message=TRUE, warning=FALSE}
library(ggplot2)
library(tidyverse)
library(readr)
activitydata <- read_csv("C:/Users/User/Desktop/activity.csv", 
    col_types = cols(steps = col_integer()))







```


**Finding mean total number of steps taken per day, ignoring NA values and plotting it on a histogram.**

```{r echo=TRUE}
stepsperday<-summarise(group_by(activitydata,date),steps=sum(steps))
qplot(stepsperday$steps,bins=20,main="Histogram of total steps per day",xlab = "Steps",ylab = "frequency")
```

**Calculating the mean and median of the number of steps taken per day.**

```{r echo=TRUE}
mean(stepsperday$steps,na.rm = TRUE)
median(stepsperday$steps,na.rm = TRUE)
```

**Calculating and plotting average daily pattern.**

```{r echo=TRUE}
meandailyactivity<-summarise(group_by(activitydata,interval),steps=mean(steps,na.rm = TRUE))
plot(meandailyactivity$interval,meandailyactivity$steps,type = "l",col="blue",main = "average daily activity pattern",xlab = "Interval",ylab = "Steps")
```

**5-minute interval containing the maximum number of steps (averaged across all days)**

```{r echo=TRUE}
meandailyactivity[which.max(meandailyactivity$steps),]$interval
```

**Now to input missing values, first calculating the number of NA values;**

```{r echo=TRUE}
missingvalues<-sum(is.na(activitydata$steps))
missingvalues
```

**Inputting missing values by using mean for that 5 minute interval**


```{r echo=TRUE}
impdata<-activitydata
for(i in 1:nrow(impdata)){
  if (is.na(impdata$steps[i])){
    impdata$steps[i] <- meandailyactivity[which(impdata$interval[i] == meandailyactivity$interval), ]$steps
  }
}
```

**Now confirming that new data set called impdata doesnt have NA values**

```{r echo=TRUE}
sum(is.na(impdata))
```

**Now plotting histogram of the total number of steps taken each day after imputing missing data**

```{r echo=TRUE}
stepsperday2<-summarise(group_by(impdata,date),steps=sum(steps))
qplot(stepsperday2$steps,bins=20,main="Histogram of total steps per day after imputing missing data",xlab = "Steps",ylab = "frequency")
```

**Now the new mean and median after imputing NA values are;**

```{r echo=TRUE}
mean(stepsperday2$steps,na.rm = TRUE)
median(stepsperday2$steps,na.rm = TRUE)
```



```{r echo=TRUE}
weekdaysvector<-c("Monday","Tuesday","Wednesday","Thursday","Friday")
datevar<-as.Date(impdata$date)
weekdaysempty<-vector()
for(i in 1:length(datevar)){
  if(weekdays(datevar[i]) %in% weekdaysvector){
    weekdaysempty[i]<-"weekday"
  } else{
    weekdaysempty[i]<-"weekend"
  }
}
impdata$weekdays<-weekdaysempty
```

**Now creating subsets of impdata for weekdays and weekends**

```{r echo=TRUE}
weekdayset<-subset(impdata,impdata$weekdays=="weekday")
weekendset<-subset(impdata,impdata$weekdays=="weekend")
```

**Now finding mean daily activities for weekdays and weekends**

```{r echo=TRUE}
meanweekday<-summarise(group_by(weekdayset,interval),steps=mean(steps,na.rm = TRUE))
meanweekend<-summarise(group_by(weekendset,interval),steps=mean(steps,na.rm = TRUE))
```

**Now Plotting the mean daily activities for weekdays and weekends**

```{r echo=TRUE}
par(mfrow=c(2,1))
plot(meanweekend$interval,meanweekend$steps,type = "l",col="green",xlab = "Interval",ylab = "Steps",main="weekend")
plot(meanweekday$interval,meanweekday$steps,type = "l",col="blue",main = "weekday",xlab = "Interval",ylab = "Steps")
```

