library(SimDesign)
library(tidyverse)
library(lubridate)
library(caTools)
library(ggpubr)
library(extrafont)
extrafont::loadfonts(device = "win")

#Set working directory -- note all file paths in script are local
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

#Split into training and test datasets
set.seed(70) #set random seed to ensure reproducabiltiy

#use a split ratio of 75/25
sample = sample.split(purple_embassy$purpleair_pm2.5, SplitRatio = .75)
train = subset(purple_embassy, sample == TRUE)
test  = subset(purple_embassy, sample == FALSE)

#create MLR model with training data
model <- lm(embassy_pm2.5 ~ purpleair_pm2.5 + temp + RH, data = train)
dailyMLR <- summary(model)

#apply model to test data for statistical evaluation
test$pm2.5_c <- predict(model, test)

#apply model to entire data series for visualization
purple_embassy$pm2.5_c <- predict(model,purple_embassy)

#summary stats
meanaberr <- MAE(test$pm2.5_c, test$embassy_pm2.5)
rawmeanaber <- MAE(purple_embassy$purpleair_pm2.5, purple_embassy$embassy_pm2.5)
rawmeanaber_h <- MAE(purple_embassy_h$purpleair_pm2.5, purple_embassy_h$Raw.Conc.)
corrcoeff <- cor(test$pm2.5_c, test$embassy_pm2.5, use = "everything",
                 method = "pearson")

###############################################Hourly Model##################################
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

set.seed(70) #set random seed to ensure reproducabiltiy

#use a split ratio of 75/25
sample = sample.split(purple_embassy_h$purpleair_pm2.5, SplitRatio = .75)
train_h = subset(purple_embassy_h, sample == TRUE)
test_h  = subset(purple_embassy_h, sample == FALSE)

#create hourl MLR model
model_h <- lm(Raw.Conc. ~ purpleair_pm2.5 + temp + RH, data = train_h)
hourlyMLR <- summary(model_h)

#apply model to test data for statistical evaluation
test_h$pm2.5_c <- predict(model_h,test_h)

#summary stats
meanaberr_h <- MAE(test_h$pm2.5_c, test_h$Raw.Conc.)
corrcoeff_h <- cor(test_h$pm2.5_c, test_h$Raw.Conc., use = "everything",
                 method = "pearson")

#############################Graphing the Corrected Data##############################
setwd("C:/Users/cmcfa/OneDrive/Desktop/2020 Research/Repository")

#for visualization we will plot the full corrected time series
purple_embassy$year <- format(purple_embassy$date, "%Y")

#for secondary axis
label_range <- purple_embassy %>%
  group_by(year) %>%
  dplyr::summarize(xmin = min(date),
            xmax = max(date),
            ymin = -25,
            ymax = ymin + 10)

p <- ggplot() +
  geom_line(data = purple_embassy, aes(x = date, y = embassy_pm2.5, color = 'PM2.5 (Embassy Data)')) +
  geom_line(data = purple_embassy, aes(x = date, y = pm2.5_c, color = 'PM2.5 (Corrected Purple Air Data)')) +
  geom_line(data = purple_embassy, aes(x = date, y = purpleair_pm2.5, color = 'PM2.5 (Purple Air Data)')) +
  labs(x = "Date (2019-2020)" , y = expression(bold(paste(PM[2.5]~ "(", mu, "g/m"^"3",")"))))+
  scale_colour_manual(values=c("mediumturquoise" , "orange1", "purple4"),
                      labels = expression(PM[2.5]~"(Corrected Purple Air Data)",PM[2.5]~"(Embassy Data)",PM[2.5]~"(Purple Air Data)"))+
  geom_rect(data = label_range, fill = "gray100", color = "gray79",
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, group = year))+
  geom_text(data = label_range, aes(x = as.POSIXct((as.numeric(xmin) + as.numeric(xmax)) / 2, origin = '1970-01-01'), y = ymax - 5, group = year, label = year, family = "Calibri"), size = 4.5, family = "Calibri")+
  coord_cartesian(ylim = c(0, 225), clip = "off")+
  theme(legend.title = element_blank ())
show(p)

font = "Calibri"

p_format <- p + 
    scale_x_datetime(breaks = seq(as.POSIXct("2019-09-01"), as.POSIXct("2020-02-21"), by="1 month"), 
    date_labels = "%b", expand = expansion(mult = c(0.005,0.01)), limits = c(as.POSIXct("2019-09-01"), as.POSIXct("2020-02-21"))) + 
    scale_y_continuous(expand = c(0,0))+
  
    theme(legend.key = element_rect(fill = "white"), 
          legend.text = element_text(family="Calibri", size=12),
          strip.text = element_text(family="Calibri", size=12),
    legend.text.align = 0,
    legend.title = element_blank(),
    legend.position = "top",
    legend.direction = "horizontal",
    plot.title = element_text(family = font, face="bold", size = 14),
    axis.text.x = element_text(family = font, size = 12),
    axis.text.y = element_text(family = font, size = 12),
    axis.title.x = element_text(family= font, face ="bold", size = 14, vjust = -10),
    axis.title.y = element_text(family= font, face ="bold",size = 13),
    plot.margin = unit(c(1,1,4,1), "lines"),
    axis.line.x.bottom = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks = element_line(color = "gray90"),
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_text(family = font))
show(p_format)   

ggsave("kampala corrected.png", plot = p_format, width = 8, height = 7)
