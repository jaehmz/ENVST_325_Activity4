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
airMA <- numeric()

for(i in 8:length(weather$AirTemp)){
  airMA[i] <- mean(weather$AirTemp[(i-7):i])
}

weather$airMA = airMA

ggplot(data = weather, aes(x = Date)) +
  geom_line(aes(y = AirTemp), color = "blue", linewidth = 1) +
  geom_line(aes(y = airMA), color = "red", linewidth = 1) +
  labs(
    title = "Air Temperature and 2-Hour Rolling Average",
    subtitle = "January 2022",
    x = "Date",
    y = "Air Temperature (°C)"
  )

# Prompt 2
# graphing solar radiation

SolRad <- weather %>%
  mutate(Date = mdy_hm(Date)) %>%
  filter(Date >= mdy_hm("4/1/21 0:00") & Date <= mdy_hm("7/27/21 23:45"))

ggplot(SolRad, aes(x = Date, y = SolRad)) +
  geom_line(color = "orange2") +
  labs(
    title = "Solar Radiation Levels (April–July 2021)",
    subtitle = "Assessing for sensor buildup/accumulation",
    x = "Date",
    y = "Solar Radiation"
  )

# Prompt 3

# Talked about this in class

# Homework ----
# Prompt 1
sum(is.na(weather$Precip))

# Prompt 2
weather = weather %>%
  mutate(VoltFlag = ifelse(BatVolt < 8500, "Warning", "Normal"))

# Prompt 3
SolRadCheck <- function(AirTemp, SolRad) {
  flag <- rep("Valid", length(AirTemp))
  temp_problem <- AirTemp < -40 | AirTemp > 45
  rad_problem  <- SolRad < 0 | SolRad > 1400
  flag[temp_problem | rad_problem] <- "OutOfRange"
  return(flag)
}

weather <- weather %>%
  mutate(DataQuality = SolRadCheck(AirTemp, SolRad))

# Prompt 4




