    activity <- read.csv("activity.csv")
    activity$date <- as.Date(activity$date)

    total_steps_per_day <- activity %>%
      group_by(date) %>%
      summarize(total_steps = sum(steps, na.rm = TRUE))


    hist(total_steps_per_day$total_steps,
         main = "Total Steps Per Day",
         xlab = "Steps",
         col = "skyblue",
         breaks = 20)

![](Course-Project-_files/figure-markdown_strict/unnamed-chunk-2-1.png)

    mean_steps <- mean(total_steps_per_day$total_steps, na.rm = TRUE)
    median_steps <- median(total_steps_per_day$total_steps, na.rm = TRUE)
    mean_steps

    ## [1] 9354.23

    median_steps

    ## [1] 10395

    average_interval <- activity %>%
      group_by(interval) %>%
      summarize(mean_steps = mean(steps, na.rm = TRUE))


    plot(average_interval$interval, average_interval$mean_steps, type = "l",
         main = "Average Daily Activity Pattern",
         xlab = "5-minute Interval",
         ylab = "Average Number of Steps")

![](Course-Project-_files/figure-markdown_strict/unnamed-chunk-3-1.png)

    max_interval <- average_interval[which.max(average_interval$mean_steps), ]
    max_interval

    ## # A tibble: 1 × 2
    ##   interval mean_steps
    ##      <int>      <dbl>
    ## 1      835       206.

    sum(is.na(activity$steps))

    ## [1] 2304

    activity_filled <- activity %>%
      left_join(average_interval, by = "interval") %>%
      mutate(steps = ifelse(is.na(steps), mean_steps, steps)) %>%
      select(steps, date, interval)

    total_steps_filled <- activity_filled %>%
      group_by(date) %>%
      summarize(total_steps = sum(steps))


    hist(total_steps_filled$total_steps,
         main = "Total Steps Per Day (After Imputation)",
         xlab = "Steps",
         col = "salmon",
         breaks = 20)

![](Course-Project-_files/figure-markdown_strict/unnamed-chunk-6-1.png)

    mean_filled <- mean(total_steps_filled$total_steps)
    median_filled <- median(total_steps_filled$total_steps)
    mean_filled

    ## [1] 10766.19

    median_filled

    ## [1] 10766.19

    activity_filled$day_type <- ifelse(weekdays(activity_filled$date) %in% c("Saturday", "Sunday"),
                                       "Weekend", "Weekday")


    avg_by_daytype <- activity_filled %>%
      group_by(interval, day_type) %>%
      summarize(mean_steps = mean(steps), .groups = "drop")


    ggplot(avg_by_daytype, aes(x = interval, y = mean_steps)) +
      geom_line() +
      facet_wrap(~day_type, nrow = 2) +
      labs(title = "Activity Patterns: Weekdays vs Weekends",
           x = "Interval", y = "Average Steps")

![](Course-Project-_files/figure-markdown_strict/unnamed-chunk-7-1.png)
