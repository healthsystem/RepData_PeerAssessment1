library(lattice)

#path <- "C:/Users/Matthew Wiandt/Documents/data_science/"
activity_raw <- read.csv("./reproducible_research/RepData_PeerAssessment1/activity.csv")
activity_raw$date <- as.Date(activity_raw$date, format = "%Y-%m-%d")
activity <- activity_raw

activity_clean <- activity[complete.cases(activity),]
activity_clean$date <- as.Date(activity_clean$date, format = "%Y-%m-%d")


#What is the mean total number of steps taken per day
    #Aggregate the data by date
    steps_date <- tapply(activity_clean$steps,activity_clean$date, mean)

    #Make a histogram of the total number of steps taken each day
    hist(steps_date, breaks = length(steps_date),main = "Steps")
    
    #Calculate and report the mean and median total number of steps taken per day
    median(steps_date)
    mean(steps_date)


#What is the average daily activity pattern?
    #Aggregate steps taken by interval of time
    activity_interval <- tapply(activity_clean$steps,activity_clean$interval,mean)
    #Make a time series plot i.e., type = "l" of the 5 minute interval and 
    #the average number of steps taken, averaged across all days (y-axis)
    plot(names(activity_interval),activity_interval, type = "l")

    #Which 5-minute interval, on average across all the days in the dataset, 
    #contains the maximum number of steps?
    ind <- which(activity_interval == max(activity_interval), arr.ind=T)
    rownames(ind)

#Imputing missing values
    #1. Calculate and report the total number of missing values in the dataset
    sum(!complete.cases(activity_raw))

    #2-3.Replace NAs with the average number of steps in the interval across
    #all dates and create a new dataset with the missing data filled in
    activity_new <- activity_raw    
    activity_new$steps[is.na(activity_new$steps)] <- ave(activity_new$steps,
                                            activity_new$interval,
                                            FUN=function(x)mean(x,
                                            na.rm = T))[is.na(activity_new$steps)]

    #4.Make a histogram of the total number of steps taken each day and calculate
    #and report the mean and median total number of steps taken each day
    activity_day <- tapply(activity$steps,activity$date,mean)
    hist(activity_day, breaks = length(activity_day),main = "Steps by Day")
    median(activity_day)
    mean(activity_day)

    #4., continued, Do these values differ from the estimates from the first part of the assgnmt?
    #What is the impact of imputing missing data on the estimates of the total
    #number of daily steps

#Are these differences in activity patterns between weekdays and weekends?
    #1. Create a new factor vairable in the dataset with two levels - "weekday"
    #   and "weekend" indicating whether a given date is a weekday or a weekend day
    #   Note: Determined weekdays and weekends using POSIXlt (0-6 starting on Sunday) 
    activity$dayofweek <- as.POSIXlt(activity$date)$wday
    activity$weekendorday = ifelse(activity$dayofweek == 0 | 
                            activity$dayofweek == 6,"weekend","weekday")
    activity$weekendorday <- as.factor(activity$weekendorday)

    #2. Make a panel plot containing a time series plot i.e., type = l of the 
    #   5-minute interval (x-axis) and the average number of steps taken, averaged
    #   across all weekday days or weekend days (y-axis).

    activity_date <- activity[,c(5,3,1)]
    activity_week <- aggregate(steps ~weekendorday + interval,
                           FUN = mean,data=activity_date)
    with(activity_week,xyplot(steps ~ interval | weekendorday, 
                          type = "l",layout = c(1,2)))