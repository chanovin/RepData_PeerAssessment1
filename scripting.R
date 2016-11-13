## initialize ggplot2, lattice
library(ggplot2)
library(lattice)

## read the data
activity <- read.csv((unz("activity.zip","activity.csv")))

## transform dates and intervals into POSIXct and store
activity$tstamp <- as.POSIXct(paste(as.character(activity$date),
                              sprintf("%04d",activity$interval)), format="%Y-%m-%d %H%M")

## calculate total steps per day, ignoring NAs
stepperday <- aggregate(activity$steps, by=list(as.Date(activity$tstamp)), FUN=sum,
                        na.rm=TRUE)
names(stepperday) <- c("date","steps")

## plot a histogram of the steps per day in 15 bins
qplot(steps,data=stepperday,bins=15)

## calculate mean and median steps per day
meandaystep<-mean(stepperday$steps)
meandaystep
median(stepperday$steps)

## calculate mean steps per time of day, ignoring NAs
stepbytime <- aggregate(activity$steps, 
                        by=list(as.integer(strftime(activity$tstamp, format="%H%M"))),
                        FUN=mean, na.rm=TRUE)
names(stepbytime) <- c("time","steps")

## graph the mean steps per time of day
qplot(time,steps,data=stepbytime,geom="line")

## determine the five-minute interval with the highest mean steps
stepbytime$time[stepbytime$steps==max(stepbytime$steps)]

## calculate the number of NAs recorded
missindex <- which(is.na(activity$steps))
length(missindex)

## create a new dataframe for activity with NAs replaced (fixed)
## NAs are replaced using [mean steps for that time interval] * [total steps for that day]
##                        / [mean steps per day]
## if total steps for a day is NA, then 0 is used
## this results in a low estimate of the number of steps that should be in that interval

fixact <- activity
fillact <- data.frame(date=as.Date(activity$tstamp[missindex[i]]),time=as.integer(strftime(activity$tstamp[missindex], format="%H%M")))
for (i in 1:length(missindex)){
  fillact$timestep[i] <- stepbytime$steps[stepbytime$time==fillact$time[i]]
  fillact$daystep[i] <- stepperday$steps[stepperday$date==fillact$date[i]]
  if(is.na(fillact$daystep[i])){
    fillact$daystep[i] <- 0
  }
}
fillact$steps <- fillact$timestep * fillact$daystep / meandaystep
fixact$steps[missindex] <- as.integer(fillact$steps)

## repeat first steps (histogram, basic analysis) with imputed values
fixstepperday <- aggregate(fixact$steps, by=list(as.Date(fixact$tstamp)), FUN=sum,
                        na.rm=TRUE)
names(fixstepperday) <- c("date","steps")

## plot a histogram of the steps per day in 15 bins
qplot(steps,data=fixstepperday,bins=15)

## calculate mean and median steps per day
fixmeandaystep<-mean(fixstepperday$steps)
fixmeandaystep
median(fixstepperday$steps)


## add weekday/weekend factors
fixact$busday <- weekdays(fixact$tstamp)
fixact$busday[fixact$busday == "Sunday"| fixact$busday == "Saturday"] <- "weekend"
fixact$busday[!fixact$busday == "weekend"] <- "weekday"
fixact$busday <- as.factor(fixact$busday)
  
## make a lattice plot of weekdays vs weekends
fixact$time <- as.integer(strftime(fixact$tstamp, format="%H%M"))
fixstepbytime <- aggregate(steps ~ time + busday, fixact, mean)
xyplot(steps ~ time | busday, data=fixstepbytime, type="l")
  
  
  