---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


```r
library(tidyverse)

# Prevent scientific notation and display results with one significant digit
options(scipen = 999, digits=1)
```

## Loading and preprocessing the data


```r
data <- read_csv("activity.csv")
```

```
## Parsed with column specification:
## cols(
##   steps = col_integer(),
##   date = col_date(format = ""),
##   interval = col_integer()
## )
```

## What is mean total number of steps taken per day?


```r
total_steps_per_day <- data %>%
    group_by(date) %>%
    summarize(steps = sum(steps)) %>%
    na.omit()

ggplot(data = total_steps_per_day, aes(steps)) +
    geom_histogram(binwidth = 1000)
```

![](PA1_template_files/figure-html/unnamed-chunk-68-1.png)<!-- -->

The mean number of steps taken per day was **10766.2**. The median number of steps taken per day was **10765**.

## What is the average daily activity pattern?


```r
average_steps_by_interval <- data %>%
    group_by(interval) %>%
    summarize(steps = mean(steps, na.rm = TRUE))

ggplot(data = average_steps_by_interval, aes(x = interval, y = steps)) +
    geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-69-1.png)<!-- -->


```r
max_interval <- average_steps_by_interval %>%
    filter(steps == max(steps))
```

The 5-minute interval with the highest average number of steps (i.e. **206.2** steps) was **835**.

## Imputing missing values

The total number of rows with missing values in `data` is **2304**.

We will impute missing values by substituting the median of each 5-minute interval. Do this using the awesome `simputation` package.


```r
library(simputation)

data_imputed <- data %>%
    impute_median(steps ~ interval)

total_steps_per_day_imputed <- data_imputed %>%
    group_by(date) %>%
    summarize(steps = sum(steps))

ggplot(data = total_steps_per_day_imputed, aes(steps)) +
    geom_histogram(binwidth = 1000)
```

![](PA1_template_files/figure-html/unnamed-chunk-71-1.png)<!-- -->

After using imputation, the mean number of steps taken per day was **9402.8**. The median number of steps taken per day was **10395**. Both of these values are lower than in the original data set.

## Are there differences in activity patterns between weekdays and weekends?


```r
data_imputed <- data_imputed %>%
    mutate(
        weekend = ifelse(weekdays(date) %in% c("Saturday", "Sunday"), 
                         "weekend", "weekday"),
        weekend = as.factor(weekend)
    )

average_steps_by_interval_weekend <- data_imputed %>%
    group_by(interval, weekend) %>%
    summarize(steps = mean(steps, na.rm = TRUE))

ggplot(data = average_steps_by_interval_weekend, aes(x = interval, y = steps)) +
    geom_line() +
    facet_wrap (~weekend, nrow = 2)
```

![](PA1_template_files/figure-html/unnamed-chunk-72-1.png)<!-- -->
