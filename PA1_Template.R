## ----loaddata------------------------------------------------------------
unzip(zipfile="activity.zip")
df <- read.csv("activity.csv")


## ------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
c <- df%>%group_by(date)%>%summarise(co=sum(steps,na.rm=TRUE))
qplot(c$co, binwidth=1000, xlab="total number of steps taken each day")
mean(c$co,na.rm=TRUE)
median(c$co,na.rm=TRUE)


## ------------------------------------------------------------------------
library(ggplot2)
avg <- df%>%group_by(interval)%>%summarise(co=mean(steps,na.rm=TRUE))
ggplot(data=avg, aes(x=interval, y=co)) +
  geom_line() +
  xlab("5-minute interval") +
  ylab("average number of steps taken")


## ------------------------------------------------------------------------
avg[which.max(avg$co),]


## ----how_many_missing----------------------------------------------------
missing <- is.na(df$steps)
# How many missing
table(missing)


## ------------------------------------------------------------------------
# Replace each missing value with the mean value of its 5-minute interval
df1 <- df%>%rowwise()%>%
  mutate(steps=ifelse(is.na(steps),avg[avg$interval==interval,"co"],steps))%>% data.frame()
df1$steps <- as.numeric(df1$steps)



## ------------------------------------------------------------------------
total.steps <- df1%>%group_by(date)%>%summarise(co=sum(steps,na.rm=TRUE))
qplot(total.steps$co, binwidth=1000, xlab="total number of steps taken each day")
mean(total.steps$co)
median(total.steps$co)


## ------------------------------------------------------------------------
df$weekendorweekday <- ifelse(weekdays(as.Date(df1$date)) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),"Weekday","Weekend")


## ------------------------------------------------------------------------
sum_int <- df%>%group_by(interval,weekendorweekday)%>%summarise(sumsteps=mean(steps,na.rm=TRUE))
ggplot(sum_int, aes(interval, sumsteps)) + geom_line() + facet_grid(weekendorweekday ~ .) +
  xlab("5-minute interval") + ylab("Number of steps")
