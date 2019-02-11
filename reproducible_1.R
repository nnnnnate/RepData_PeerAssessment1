
acts <- read.csv("activity.csv")
library(ggplot2)

# What is mean total number of steps taken per day?
sumByDay <- tapply(acts$steps, acts$date, sum, na.rm = TRUE)
g1 <- ggplot(as.data.frame(sumByDay), aes(x = sumByDay)) 
g1 + geom_histogram(color = "white",fill= "lightblue", bins = 9) + scale_x_continuous(breaks = seq(0,22500, 2500)) + labs(x = "Daily Total Steps", y = "Counts") 

mean(sumByDay)
median(sumByDay)

# What is the average daily activity pattern?
meanByInterval <- tapply(acts$steps, acts$interval, mean, na.rm = TRUE)
meanByInterval <- cbind(unique(acts$interval), meanByInterval)
colnames(meanByInterval) <- c("interval", "mean")
meanByInterval <- as.data.frame(meanByInterval)
g2 <- ggplot(meanByInterval, aes(x = interval, y = mean))
g2 + geom_line(color = "darkred") + scale_x_continuous(breaks =seq(0,2500,250)) +labs(y = "avergae steps")

# Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?
meanByInterval[which.max(meanByInterval[,2]),1]


# Imputing missing values
# Calculate and report the total number of missing values in the dataset 
sum(is.na(acts$steps))
# use the mean for that 5-minute interval to impute the NAs
acts2 <- acts
for (i in 1:nrow(acts2)) {
    if (is.na(acts2[i,1])) {
        interval = acts2[i,3]
        acts2[i,1] <- meanByInterval[meanByInterval$interval == interval,][2]
        rm(interval)
    }
}


sumByDay2 <- tapply(acts2$steps, acts2$date, sum)
g1 <- ggplot(as.data.frame(sumByDay2), aes(x = sumByDay2)) 
g1 + geom_histogram(color = "white",fill= "lightgreen", bins = 9) + scale_x_continuous(breaks = seq(0,22500, 2500)) + labs(x = "Daily Total Steps", y = "Counts") 

mean(sumByDay2)
median(sumByDay2)


# Are there differences in activity patterns between weekdays and weekends?
acts2$date <- as.Date(acts2$date)
acts2$weekday <- weekdays(acts2$date)
weekday2 <-c()
for (i in 1: nrow(acts2)){
    if (acts2$weekday[i] == "Saturday" | acts2$weekday[i] == "Sunday"){
        weekday2[i] <- "Weekend"
    }else{
        weekday2[i] <- "Weekday"
    }
}
acts2$weekday2 <- weekday2

weekdaySub <- subset(acts2, weekday2 == "Weekday")
weekendSub <- subset(acts2, weekday2 == "Weekend")

meanByIntervalWeekday <- tapply(weekdaySub$steps, weekdaySub$interval, mean, na.rm = TRUE)
meanByIntervalWeekday <- cbind(unique(weekdaySub$interval), meanByIntervalWeekday)
meanByIntervalWeekday <- as.data.frame(meanByIntervalWeekday)
factor <- rep("weekday", nrow(meanByIntervalWeekday))
meanByIntervalWeekday <- cbind(meanByIntervalWeekday, factor)
colnames(meanByIntervalWeekday) <- c("interval", "meanByInterval", "weekday")

meanByIntervalWeekend <- tapply(weekendSub$steps, weekendSub$interval, mean, na.rm = TRUE)
meanByIntervalWeekend <- cbind(unique(weekendSub$interval), meanByIntervalWeekend)
meanByIntervalWeekend <- as.data.frame(meanByIntervalWeekend)
factor <- rep("weekend", nrow(meanByIntervalWeekend))
meanByIntervalWeekend <- cbind(meanByIntervalWeekend, factor)
colnames(meanByIntervalWeekend) <- c("interval", "meanByInterval", "weekday")

meanByInterval2 <- rbind(meanByIntervalWeekday, meanByIntervalWeekend)

g3 <- ggplot(meanByInterval2, aes(x = interval, y = meanByInterval))
g3 + geom_line(color = "darkblue") + scale_x_continuous(breaks =seq(0,2500,250)) +labs(y = "avergae steps") + facet_grid(weekday~.)
