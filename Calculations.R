
# setwd("C:\\Users\\Karla\\OneDrive\\Documentos\\Courses\\14_ReproducibleResearch\\Project1\\RepData_PeerAssessment1")


library(lubridate)
library(dplyr)
library(lattice)


originalData <- read.csv(file = "activity.csv")
originalData$date <- ymd(originalData$date)


############# What is mean total number of steps taken per day? ################
# What is mean total number of steps taken per day?
#
# For this part of the assignment, you can ignore the missing values in the 
# dataset.
#
# Calculate the total number of steps taken per day
# 
# If you do not understand the difference between a histogram and a barplot, 
# research the difference between them. Make a histogram of the total number of 
# steps taken each day
# 
# Calculate and report the mean and median of the total number of steps taken 
# per day
################################################################################


totalSteps <- sum(originalData$steps, na.rm = TRUE)

nonNaData <- originalData[!is.na(originalData$steps),]

aggregatedData <- aggregate(nonNaData$steps,
                            by=list(date=nonNaData$date),
                            FUN=sum)
names(aggregatedData) <- c("date", "totalSteps")



png("figures/Plot1.png", width = 480, height = 480)

hist(aggregatedData$totalSteps, breaks = 20, xlab = "sum of steps", 
     main = "Frequency of steps taken", col = "#009E73")

text(15000, 8, paste("Mean    =", 
                     round(mean(aggregatedData$totalSteps), 4), 
                     "\nMedian =", 
                     round(median(aggregatedData$totalSteps), 4)), 
     pos = 4) # left aligned
dev.off()



# What is the average daily activity pattern?


# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
# and the average number of steps taken, averaged across all days (y-axis)

aggregateByInterval <- aggregate(nonNaData$steps,
                                 by=list(interval=nonNaData$interval),
                                 FUN=mean)
names(aggregateByInterval) <- c("interval", "averageSteps")


png("figures/Plot2.png", width = 480, height = 480)

plot(aggregateByInterval, type = "l", xlab = "Interval", ylab="Average steps",
     main= "Average steps per daily intervals", xaxp  = c(0, 2400, 24), col = "blue")
dev.off()
# Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?
maxNumSteps <- aggregateByInterval[which.max(aggregateByInterval$averageSteps),]


####################### Imputing missing values ################################
#
#Note that there are a number of days/intervals where there are missing values 
# (coded as NA). The presence of missing days may introduce bias into some 
# calculations or summaries of the data.
#
# Calculate and report the total number of missing values in the dataset (i.e. 
# the total number of rows with NAs)
#
# Devise a strategy for filling in all of the missing values in the dataset. The 
# strategy does not need to be sophisticated. For example, you could use the 
# mean/median for that day, or the mean for that 5-minute interval, etc.
#
# Create a new dataset that is equal to the original dataset but with the 
# missing data filled in.
#
# Make a histogram of the total number of steps taken each day and Calculate and 
# report the mean and median total number of steps taken per day. Do these values 
# differ from the estimates from the first part of the assignment? What is the 
# impact of imputing missing data on the estimates of the total daily number of 
# steps?
#
################################################################################


totalMissingRows <- sum(is.na(originalData$steps))

# There's a total of 2304 rows that have NA instead of the number of steps.
# It wouldn't be good to simply substitute with zeros since this can skew
# calculations such as mean, however, we can fill in the values based on the average 
# calculated for those rows that do have a value.  Given that we already calculated
# a table of averages per interval, the next step is to create a table of the
# rows with missing values, substitute these with the average, and join with
# theose that had values from the beginning.

naRows <- originalData[is.na(originalData$steps),]

joined <- left_join(naRows, aggregateByInterval, by = "interval")

# now recreate the original order of columns: steps, date, interval
naRowsModified <- select(joined, averageSteps, date, interval)
names(naRowsModified)[1] <- "steps"

# create a new data set where there are no NA values by binding both tables together
originalDataNoNa <- bind_rows(naRowsModified, nonNaData)

aggregateFilledInData <- aggregate(originalDataNoNa$steps,
                                   by=list(date=originalDataNoNa$date),
                                   FUN=sum)

names(aggregateFilledInData) <- c("date", "totalSteps")

plot("figures/Plot3.png", width=480, height=480)
hist(aggregateFilledInData$totalSteps, breaks = 20, xlab = "sum of steps", 
     main = "Frequency of steps taken", col = "#E69F00")
text(12000, 15, paste("Mean    =", 
                      round(mean(aggregateFilledInData$totalSteps), 4), 
                      "\nMedian =", 
                      round(median(aggregateFilledInData$totalSteps), 4)), 
     pos = 4) # left aligned
dev.off()

### Are there differences in activity patterns between weekdays and weekends? ##
#
# For this part the weekdays() function may be of some help here. Use the 
# dataset with the filled-in missing values for this part.
#
# Create a new factor variable in the dataset with two levels - "weekday" and 
# "weekend" indicating whether a given date is a weekday or weekend day.
# 
# Make a panel plot containing a time series plot (i.e. type = "l") of the 
# 5-minute interval (x-axis) and the average number of steps taken, averaged 
# across all weekday days or weekend days (y-axis). See the README file in the 
# GitHub repository to see an example of what this plot should look like using 
# simulated data.
################################################################################

dataSetWithWeekDays <- mutate(originalDataNoNa, 
                              dayType = as.factor( ifelse(weekdays(date) %in% c("Saturday", "Sunday"), 
                                                          "Weekend", 
                                                          "Weekday")))

aggregateWithWeekdays <- aggregate(steps~dayType+interval, 
                                   data=dataSetWithWeekDays, 
                                   FUN=mean)

names(aggregateWithWeekdays) <- c("dayType", "interval", "steps")

xyplot(steps ~ interval | dayType, 
       data = aggregateWithWeekdays, 
       layout=c(1,2), 
       type = "l", 
       ylab = "Number of steps")