#read in, open, and parse data
fileUrl<-"https://d396qusza40orc.cloudfront.net/repdata/data/activity.zip"
download.file(fileUrl, "data/activity.zip", method="curl")
unzip("data/activity.zip")
activity<-read.csv("activity.csv", na.strings = "NA")
activity$date<-as.Date(as.character(activity$date), "%Y-%m-%d")

#grouped datasets used in multiple steps
require(dplyr)
totalst<-activity %>% select(steps, date)%>% group_by(date)%>%summarise_all(sum)
avgst<-activity %>% select(steps, interval)%>% group_by(interval)%>%summarise_all(funs(mean(., na.rm=TRUE)))

#plot 1, histogram 
hist(totalst$steps, xlab="Steps", main="Distribution, Total steps by date")

#summary stats by day
meanst<-mean(totalst$steps, na.rm=TRUE)
medianst<-median(totalst$steps, na.rm=TRUE)
print(paste("Mean Steps: ", meanst))
print(paste("Median Steps: ", medianst))

#time series plot of the average number of steps taken
#(not specified, over the course of a day)
require(ggplot2)
qplot(avgst$interval, avgst$steps, main="Average steps by interval", xlab="Interval", ylab="Average Steps", geom="line")

#interval with maximum steps on average
maxst<-avgst[which.max(avgst$steps),]
print(paste("Interval with Maximum Steps: ", maxst$interval))

#missing data:Sociological Methods and Research, 2013, 42(1), 105-138
#Should	a	Normal	Imputation	Model	Be	Modified	to	
#Impute	Skewed	Variables?	
#Paul T. von Hippel 
totalMissing<-nrow(subset(activity, is.na(activity$steps)))
print(paste("Total missing values: ", totalMissing))
perMissing<-totalMissing/nrow(activity)
hist(activity$steps)
hist(activity$steps[activity$interval==835])

#due to some technical requirements in mice, we have to work
#with an interim dataset in which the date column is converted to numeric
interactivity<-activity
interactivity$date<-as.numeric(interactivity$date)
activityimputed<-mice(data=interactivity, m=10, meth="norm.predict")
range(activityimputed$imp$steps)
#NB: the range is here because my first attempt, using bootstrap,
#generated negative numbers. that being said, I'm not happy with the results, 
#which are not sufficiently right-skewed
activitycompleted<-complete(activityimputed, 1)
activitycompleted$date<-as.POSIXlt(activitycompleted$date)

#histogram (but not really) of total steps with imputed date
totalimputed<-activitycompleted %>% select(steps, date)%>% group_by(date)%>%summarise_all(sum)
hist(totalimputed$steps, xlab="Steps", main="Distribution, Total imputed steps by date")

#panel plot of average steps, weekdays vs. weekends. Had to use chron since
#lubridate:wday() threw up a crapton of problems
require(chron)
require(lattice)
activitycompleted$wkend<-0
activitycompleted$wkend[is.weekend(activitycompleted$date)]<-1
avgimputed<-activitycompleted %>% select(steps, interval, wkend)%>% group_by(interval, wkend)%>%summarise_all(mean)
#panel plot using lattice
xyplot(log10(steps)~interval|factor(wkend, levels=c(0,1), labels=c("weekday","weekend")),
      data=avgimputed, type="l", main="Weekday vs. weekend steps by interval (average)",
       xlab="Interval", ylab="Steps")
#overlaid plot using ggplot
plot<-qplot(interval, steps, data=avgimputed, color=factor(wkend, labels=c("weekday","weekend")))
plot+geom_line()+labs(title = "Weekday vs. weekend steps by interval (average)", x = "interval", y = "steps", color = "Day\n") 

