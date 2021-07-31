\#STEP1

\#Loading and pre processing the dataset

    activity <- read.csv("activity.csv")

    # Overview of the activity data set

    head(activity)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

    dim(activity)

    ## [1] 17568     3

    str(activity)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

    #Looking at datea set we find the we need to transform the date column into date format

    library(lubridate)

    ## Warning: package 'lubridate' was built under R version 4.0.5

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

    activity$date<-ymd(activity$date)
    length(unique(activity$date))

    ## [1] 61

    summary(activity)

    ##      steps             date               interval     
    ##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
    ##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
    ##  Median :  0.00   Median :2012-10-31   Median :1177.5  
    ##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
    ##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
    ##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
    ##  NA's   :2304

\#\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_

\#STEP2:Histogram of the total number of steps taken each day

    activity_total_steps <- with(activity, aggregate(steps, by = list(date), FUN = sum, na.rm = TRUE))
    names(activity_total_steps) <- c("date", "steps")
    hist(activity_total_steps$steps, main = "Histogram of the total number of steps taken each day", xlab = "Days", ylab="Count", col = "darkblue", ylim = c(0,20), breaks = seq(0,25000, by=2500))

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-2-1.png)

\#\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_

\#STEP3: Mean and median number of steps taken each day

    su <- tapply(activity$steps, activity$date, sum, na.rm=T)
    mean_su <- round(mean(su))
    median_su <- round(median(su))
    print(c("Mean",mean_su))

    ## [1] "Mean" "9354"

    print(c("Median",median_su))

    ## [1] "Median" "10395"

\#\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_

\#STEP4: Time series plot of the average number of steps taken

    meanStepsInterval <- tapply(activity$steps, activity$interval, mean, na.rm=T)
    plot(meanStepsInterval ~ unique(activity$interval), type="l", xlab = "5-min interval", main="Time series plot of the average number of steps taken")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png)

\#\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_

\#STEP5: The 5-minute interval that, on average, contains the maximum
number of steps

    meanStepsInterval[which.max(meanStepsInterval)]

    ##      835 
    ## 206.1698

\#\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_

\#STEP6: Code to describe and show a strategy for imputing missing data

    activity2 <- activity
    for (i in 1:nrow(activity)){
        if(is.na(activity$steps[i])){
            activity2$steps[i]<- meanStepsInterval[[as.character(activity[i, "interval"])]]
        }
    }

\#\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_

\#STEP7:Histogram of the total number of steps taken each day after
missing values are imputed

    finnnn <- tapply(activity2$steps, activity2$date, sum, na.rm=T)
    hist(finnnn, xlab = "Days", ylab = "Count" , col = "darkblue", main = "Histogram of the total number of steps taken after missing values are imputed")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-7-1.png)

    # Mean and median after imputing missing values with their 5 minute interval average
    mean_su2 <- round(mean(finnnn))
    median_su2 <- round(median(finnnn))
    print(c("New Mean",mean_su2))

    ## [1] "New Mean" "10766"

    print(c("New Median",median_su2))

    ## [1] "New Median" "10766"

\#\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_

\#STEP8:Panel plot comparing the average number of steps taken per
5-minute interval across weekdays and weekends

    Sys.setenv(LANGUAGE = "en")
    Sys.setlocale("LC_TIME", "English")

    ## [1] "English_United States.1252"

    activity2$weekday <- c("weekday")
    activity2[weekdays(as.Date(activity2[, 2])) %in% c("Saturday", "Sunday", "saturday", "sunday"), ][4] <- c("weekend")
    activity2$weekday <- factor(activity2$weekday)

    activity2_weekend <- subset(activity2, activity2$weekday == "weekend")
    activity2_weekday <- subset(activity2, activity2$weekday == "weekday")
    mean_activity2_weekday <- tapply(activity2_weekday$steps, activity2_weekday$interval, mean)
    mean_activity2_weekend <- tapply(activity2_weekend$steps, activity2_weekend$interval, mean)
    library(lattice)
    df_weekday <- data.frame(interval = unique(activity2_weekday$interval), avg = as.numeric(mean_activity2_weekday), day = rep("weekday", length(mean_activity2_weekday)))
    df_weekend <- data.frame(interval = unique(activity2_weekend$interval), avg = as.numeric(mean_activity2_weekend), day = rep("weekend", length(mean_activity2_weekend)))
    df_final <- rbind(df_weekday, df_weekend)
    xyplot(avg ~ interval | day, data = df_final, layout = c(1, 2), type = "l", ylab = "Average Number of Steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-8-1.png)
