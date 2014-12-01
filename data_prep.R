rm(list = ls())


setwd("M:/Google Drive/Classes/Stat 37400/project/")
# setwd("C:/Users/g1rxf01/Downloads/New Folder/class/assignment4")


data = read.csv("./data/treasury_data_raw.csv", sep=',')

names(data) = c('drop1', 'id_crsp', 'cusip', 'date_issued', 'date_maturity',
                'coupon_rate', 'drop2', 'drop3', 'type', 'date',
                'price_nominal', 'daily_yield', 'duration')

data = data[, !(names(data) %in% c('drop1', 'drop2', 'drop3'))]
data = data[data$type %in% c(1, 2),]


data$date            = as.Date(data$date, format="%m/%d/%Y")
data$date_issued     = as.Date(data$date_issued, format="%m/%d/%Y")
data$date_maturity   = as.Date(data$date_maturity, format="%m/%d/%Y")

data$maturity = as.numeric(data$date_maturity - data$date) / 365.25
data$duration = data$duration / 365.25
data$yield = data$daily_yield * 365.25 * 100

data = data[order(data$date, data$maturity),]

write.csv(data, "./data/treasury_data.csv")
