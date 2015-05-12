
# setwd("C:\\Users\\Karla\\OneDrive\\Documentos\\Courses\\14_ReproducibleResearch\\Project1\\RepData_PeerAssessment1")


library(lubridate)
library(ggplot2)


file <- read.csv(file = "activity.csv")
View(file)
file$date <- ymd(file$date)


#  What is mean total number of steps taken per day?
totalSteps <- sum(file$steps, na.rm = TRUE)

filteredData <- file[!is.na(file$steps),]

aggregatedData <- aggregate(filteredData$steps,
                            by=list(date=filteredData$date),
                            FUN=sum)
names(aggregatedData) <- c("date", "totalSteps")

hist(aggregatedData$totalSteps, breaks = 20, xlab = "sum of steps", 
     main = "Frequency of steps taken", col = "#009E73")

meanHist <- mean(aggregatedData$totalSteps)
medianHist <- median(aggregatedData$totalSteps)


# What is the average daily activity pattern?

# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
