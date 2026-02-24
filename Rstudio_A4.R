# James Maisano
# 02/24/26
# Envst_325

# Loading in packages
install.packages(c("dplyr","lubridate","ggplot2"))
# library for packages
library(dplyr)
library(lubridate)
library(ggplot2)
# In class work ----

# Reading in data from activity 4 csv
weather <- read.csv("/cloud/project/activity04/campus_weather.csv",
                    na.strings = "#N/A")
# Reading in meta data
metaDat <- read.csv("/cloud/project/activity04/meter_weather_metadata.csv",
                    na.strings = "#N/A")
# Reading in sensor data
sensorLog <- read.csv("/cloud/project/activity04/Sensor log.csv",
                      na.strings = "#N/A")
# Date with no specific timezone (UTC)
weather$dateF <- mdy_hm(weather$Date)
# Date with specific timezone (EST)
weather$dateET <- mdy_hm(weather$Date, tz = "America/New_York")
# filter for checking the time in EST
weatherCheck <- weather %>%
  filter(is.na(weather$dateET))
# checking for count during interval
weather$dateF[2] %--% weather$dateF [3]
# getting interval in seconds
int_length(weather$dateF[2] %--% weather$dateF [3])

# subset test
test <- weather$dateF[1:10]
# viewing first vector
test
# removing value in vector
test[-1]

# matching vectors time length with a function
# x is a date vector
timeCheck900 <- function(x){
  intervals <- x[-length(x)] %--% x[-1]
  interval_times <- int_length(intervals)
  intervals[interval_times != 900]
}
# check for 900 sec intervals
timeCheck900(weather$dateF)


# for loops
# vector for file names
soilFiles <- (list.files("/cloud/project/activity04/soil"))
# setting up variable to be used in for loops
soilList <- list()

for(i in 1:length(soilFiles)){
  soilList[[i]] <- read.csv(paste0("/cloud/project/activity04/soil/", soilFiles[i]))
}
# get info about your list
str(soilList)

# combining the data frames
soilData <- do.call("rbind", soilList)

# calculate moving average
airMA <- numeric()

for(i in 8:length(weather$AirTemp)){
  airMA[i] <- mean(weather$AirTemp[(i-7):i])
}

# adding average back
weather$airMA <- airMA

# In class prompts ----
# Prompt 1 (done in class)
# airMA <- numeric()

#for(i in 8:length(weather$AirTemp)){
  #airMA[i] <- mean(weather$AirTemp[(i-7):i])
#}

# Prompt 2
# graphing solar radiation

SolRad <- weather$SolRad
ggplot(data = weather$SolRad)