library(tidyverse)
library(lubridate)
library(SimDesign)
library(randomForest)
library(caTools)

#Set working directory
setwd("C:/Users/cmcfa/OneDrive/Desktop/2020 Research/")
purpledata <- read.csv("correct_PA_Kampala_timeseries.csv")

#change date column into date format and specify appropriate time zone
purpledata$date <- as.POSIXct(purpledata$date, format = "%Y-%m-%d %H:%M:%OS", tz = "Africa/Kampala")

#average two sensors
purpledata$pm <- ave(purpledata$pm2_5_atm, purpledata$pm2_5_atm_b)

#change temperature to be in Fahrenheit
purpledata$temp_C <- (purpledata$current_temp_f - 32)*(5/9)

#average PA data by day
purple <- 
  purpledata %>%
  group_by(date = floor_date(date, "day")) %>% 
  dplyr::summarise(
    purpleair_pm2.5 = mean(pm, na.rm = TRUE),
    temp = mean(temp_C, na.rm = TRUE), 
    RH = mean(current_humidity, na.rm = TRUE))

#remove rows with null values, although the na.rm term should have done so.
purple <- na.omit(purple)

#change working directory to location of Embasssy (reference) data
setwd("C:/Users/cmcfa/OneDrive/Desktop/2020 Research/Embassy Data")

#read in data and convert date column to date data type
embassydata2019 <- read.csv("Kampala_PM2.5_2019_YTD.csv")
embassydata2019$date <- as.POSIXct(embassydata2019$Date..LT., format = "%Y-%m-%d %I:%M %p", tz = "Africa/Kampala" )
embassydata2020 <- read.csv("Kampala_PM2.5_2020_YTD.csv")
embassydata2020$date <- as.POSIXct(embassydata2020$Date..LT., format = "%Y-%m-%d %I:%M %p", tz = "Africa/Kampala")

#isolate dates that we have purple air data for
dates2019 <- filter(embassydata2019, between(embassydata2019$date, as.POSIXct("2019-09-03",tz = "Africa/Kampala"), as.POSIXct("2019-12-31", tz = "Africa/Kampala")))
dates2020 <- filter(embassydata2020, between(embassydata2020$date, as.POSIXct("2020-01-01",tz = "Africa/Kampala"), as.POSIXct("2020-02-19", tz = "Africa/Kampala")))
embassydata <- merge(dates2019, dates2020, all=TRUE)

#fix QC issue where there are random -999 listed as data values
embassydata[embassydata < 0] <- NA

#day average embassy data
embassy <-
  embassydata %>%
  group_by(date = floor_date(date, "day")) %>% 
  dplyr::summarise(
    embassy_pm2.5 = mean(Raw.Conc. , na.rm = TRUE))

#merge data frames
purple_embassy <- merge(purple, embassy, by = "date")

#hour average PA data
purplehour <-
  purpledata %>%
  group_by(date = ceiling_date(date, "hour")) %>% #note embassy data begins at 1 am each day, so when using hourly data make sure to round time UP to the hour
  dplyr::summarise(
    purpleair_pm2.5 = mean(pm, na.rm = TRUE),
    temp = mean(temp_C, na.rm = TRUE), 
    RH = mean(current_humidity, na.rm = TRUE))

#merge data frames
purple_embassy_h <- merge(purplehour, embassydata, by = "date")
purple_embassy_h <- na.omit(purple_embassy_h)

set.seed(70)#set random seed to ensure reproducabiltiy

sample = sample.split(purple_embassy$purpleair_pm2.5, SplitRatio = .75) #use a split ratio of 75/25
train = subset(purple_embassy, sample == TRUE)
test  = subset(purple_embassy, sample == FALSE)

set.seed(70)
output.forest <- randomForest(embassy_pm2.5 ~ purpleair_pm2.5 + temp + RH, 
                              data = train, importance = TRUE)

#apply model to test data for statistical evaluation
test$pm2.5c <- predict(output.forest, test)
meanaberr <- MAE(test$pm2.5c, test$embassy_pm2.5)
corrcoeff <- cor(test$pm2.5c, test$embassy_pm2.5, use = "everything",
                 method = "pearson")

########################################Hourly Model##############################
set.seed(70)
sample_h = sample.split(purple_embassy_h$Raw.Conc., SplitRatio = .75)
train_h = subset(purple_embassy_h, sample == TRUE)
test_h  = subset(purple_embassy_h, sample == FALSE)

set.seed(70)
output.forest_h <- randomForest(Raw.Conc. ~ purpleair_pm2.5 + temp + RH, 
                              data = train_h, importance = TRUE)

test_h$pm2.5c <- predict(output.forest_h, test_h)

#summary stats
meanaberr_h <- MAE(test_h$pm2.5c, test_h$Raw.Conc.)
corrcoeff_h <- cor(test_h$pm2.5c, test_h$Raw.Conc., use = "everything",
                 method = "pearson")

print(importance(output.forest))
print(importance(output.forest_h))