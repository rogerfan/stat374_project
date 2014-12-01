rm(list = ls())

library('ggplot2')
library('reshape2')
library('plyr')
library('extrafont')


set.seed(134678684)

setwd("M:/Google Drive/Classes/Stat 37400/project/")
# setwd("C:/Users/g1rxf01/Downloads/New Folder/class/assignment4")


data = read.csv("./data/treasury_data.csv")
data$date = as.Date(data$date)

test_date = as.Date('2006-05-08')
test_data = data[data$date == test_date,]
plot(test_data$duration, test_data$yield)
