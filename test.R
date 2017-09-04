# Clustering Code
# Irwin Cassells
# Spree: Customer Clustering
# 14 August 2017

# Clear Global Environment
rm(list = ls())
options(warn = -1)

# Check for packages, and install if not there
if(!require(rstudioapi)) install.packages("rstudioapi")
if(!require(dplyr)) install.packages("dplyr")
if(!require(zoo)) install.packages("zoo")
if(!require(tidyr)) install.packages("tidyr")
if(!require(bigrquery)) install.packages("bigrquery")
if(!require(lubridate)) install.packages("lubridate")

# Imports packages
require("rstudioapi")
require("dplyr")
require("zoo")
require("tidyr")
require("bigrquery")
require("lubridate")

# Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load data
rawData = read.csv("activity.csv", stringsAsFactors = F)

# change to date

# collapse and count steps per day (dplyr)
stepsPerDay =   rawData %>%
                group_by(date) %>%
                summarise(Steps = sum(steps,na.rm = T))

# change to date
stepsPerDay$date = date(stepsPerDay$date)

# days since first day
stepsPerDay$DaysSinceFirst = stepsPerDay$date - min(stepsPerDay$date)
                
# plot hist using plot
with(stepsPerDay,barplot(Steps,
                         names.arg= DaysSinceFirst, 
                         cex.names = 0.6,
                         xlab = "Days since recording started",
                         ylab = "Number of steps",
                         main = "Chart depicting the number of steps per day since the first day of recording",
                         space = 0))

# mean and median per day
meanSteps = round(mean(stepsPerDay$Steps),0)
medianSteps = round(median(stepsPerDay$Steps),0)

###################################################################

# make a dat frame for the 5-min intervals
 intervalSteps =    rawData %>%
                    group_by(interval) %>%
                    summarise(Average = mean(steps,na.rm = T))

# plot data
with(intervalSteps,plot(interval,Average,
                        type = "l",
                        ylab = "Avg. steps",
                        xlab = "Time (min)",
                        main = "Time series plot of the average number of steps",
                        cex = 0.6))

# which interval
maxValue = max(intervalSteps$Average)
maxMinutes = as.integer(intervalSteps$interval[match(max(intervalSteps$Average),intervalSteps$Average)])
maxThTime = (maxMinutes / 5) + 1

###################################################################

# count number of NA's
stepsNA = sum(is.na(rawData$steps))
dateNA = sum(is.na(rawData$date))
intervalNA = sum(is.na(rawData$interval))

# split data set
dataNA = rawData[is.na(rawData$steps),]
data = rawData[!is.na(rawData$steps),]

# merge data from previous set
dataNA = merge(dataNA,intervalSteps,by.x = "interval",by.y = "interval", all.x = T)

# make average equal to the value
dataNA$steps = dataNA$Average

# rearrange dataNA
dataNA = dataNA[,c("steps","date","interval")]

# merge data together
data = rbind(data,dataNA)

# do the same as you did before ---------------

# collapse and count steps per day (dplyr)
stepsPerDayNA =   data %>%
    group_by(date) %>%
    summarise(Steps = sum(steps,na.rm = T))

# change to date
stepsPerDayNA$date = date(stepsPerDayNA$date)

# days since first day
stepsPerDayNA$DaysSinceFirst = stepsPerDayNA$date - min(stepsPerDayNA$date)

# plot hist using plot
with(stepsPerDayNA,barplot(Steps,
                         names.arg= DaysSinceFirst, 
                         cex.names = 0.6,
                         xlab = "Days since recording started",
                         ylab = "Number of steps",
                         main = "With NA replaced",
                         space = 0))

# mean and median per day
meanStepsNA = round(mean(stepsPerDayNA$Steps),0)
medianStepsNA = round(median(stepsPerDayNA$Steps),0)


# ------------------------------------------------------------
# add the weekday and weekend factor
rawData$week = ifelse((wday(rawData$date)>0 & wday(rawData$date)<6),"Weekday","Weekend")

# reduce to average number
rawData =   rawData %>%
    group_by(week,interval) %>%
    summarise(AvgSteps = mean(steps,na.rm = T))

# replace NA values with 0
rawData$AvgSteps[is.na(rawData$AvgSteps)] = 0

# create factor
rawData$week = factor(rawData$week)

library(lattice)

xyplot(AvgSteps~interval|week, data = rawData, type = "l", layout = c(1,2), ylab = "Number of steps", xlab = "interval")