#setwd("./ML/repro_research/RepData_PeerAssessment1")
sessionInfo()
packages<-c("lubridate","dplyr","lattice")
lapply(packages,require,character.only=TRUE)

# read and preprocess
x<-read.csv("./RepData_PeerAssessment1/activity.csv",header=T,stringsAsFactors=F)
x$date<-ymd(x$date)
x<-mutate(x,dow=ordered(wday(x$date)))

# total daily steps by date
x.perday <- x %>% group_by(date) %>% mutate(daily_total=sum(steps,na.rm=T))
x.perday <- x.perday %>% group_by(interval) %>% mutate(interval_mean=mean(steps,na.rm=T))
x.perday.summary <- x.perday %>% group_by(date) %>% summarize(daily_total=sum(steps))

# Calculate and report the mean and median of the total number of steps taken per day
x.dayofweek <- x.perday %>% group_by(dow) %>% summarize(mean_steps=mean(daily_total))
mean(x.perday.summary$daily_total,na.rm=T)
median(x.perday.summary$daily_total,na.rm=T)
# x.median<-x.perday %>% group_by(weekday) %>% summarize(mean_steps=median(daily_total))

# plot
hist(x.perday.summary$daily_total,breaks=30)
abline(v=median(x.perday$daily_total),col="magenta")

# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
x.int <- x %>% group_by(interval) %>% summarize(ave_steps=mean(steps,na.rm=T))
x.int[which.max(x.int$ave_steps),]

# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
with(x.int,plot(interval,ave_steps,type="l")) 

# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
mean(is.na(x)) # proportion of NA
sum(is.na(x))  # number of rows with NA
ind<-which(is.na(x)) #get indicides with NA

# Create a new dataset that is equal to the original dataset but with the missing data filled in.
# replace NAs with the proper interval mean
y.perday <- x.perday
y.perday$steps[ind]<-y.perday$interval_mean[ind]
y.perday <- y.perday %>% group_by(date) %>% mutate(daily_total=sum(steps))
y.perday.summary <- y.perday %>% group_by(date) %>% summarize(daily_total=sum(steps))

# Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
hist(y.perday$daily_total,breaks=30)
median(y.perday$daily_total)
mean(y.perday$daily_total)

# Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
y.perday$wkdy <- factor((ifelse(weekdays(y.perday$date) %in% c("Saturday","Sunday"), "weekend", "weekday")))

# Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
xyplot(steps ~ interval | wkdy, data=y.perday, layout= c(1,2), main="", ylab = "Steps", xlab="Interval",type="l")


