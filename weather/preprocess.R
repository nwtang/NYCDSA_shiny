library(dplyr)
library(tidyr)
library(lubridate)

# PROCESS TEMPERATURE DATA
# Select just cities in USA, read in temperature.csv and filter only US cities
city <- read.csv(file="./city_attributes.csv")
city$City <- gsub(" ",".",city$City)
city <- city[(city$Country=="United States"),]
usa_cities <- unlist(lapply(city['City'],as.character))
temp <- read.csv(file="./temperature.csv")
temp_sub <- temp[,which(colnames(temp) %in% append(usa_cities,"datetime"))]
temp_sub <- temp_sub %>% gather("City","Temp",usa_cities)
# Convert temperature from K to F
temp_sub$Temp <- round((temp_sub$Temp-273.15)*9/5+32,digits=0)
temp_sub <- merge(temp_sub,city,by="City")

# PROCESS WEATHER DESCRIPTION DATA
# Read in description csv, filter only US cities, inner join with temp_sub
desc <- read.csv(file="./weather_description.csv")
desc_sub <- desc[,which(colnames(desc) %in% append(usa_cities,"datetime"))]
desc_sub <- desc_sub %>% gather("City","Desc",usa_cities)
temp_sub <- inner_join(temp_sub,desc_sub,by=c("datetime","City"))
# Read in desc_summ (manually created to match detailed descriptions to general ones)
# Join with temp_sub, get rid of old detailed description column and keep general one
desc_summ <- read.csv(file="./desc_summ.csv")
temp_sub <- inner_join(temp_sub,desc_summ,by="Desc")
temp_sub <- select(temp_sub,-Desc)

# PROCESS WIND DATA
# Bring wind direction and speed into temp_sub
wdir <- read.csv(file="./wind_direction.csv")
wspd <- read.csv(file="./wind_speed.csv")
wdir_sub <- wdir[,which(colnames(wdir) %in% append(usa_cities,"datetime"))]
wdir_sub <- wdir_sub %>% gather("City","Wdir",usa_cities)
wspd_sub <- wspd[,which(colnames(wspd) %in% append(usa_cities,"datetime"))]
wspd_sub <- wspd_sub %>% gather("City","Wspd",usa_cities)
temp_sub <- inner_join(temp_sub,wdir_sub,by=c("datetime","City"))
temp_sub <- inner_join(temp_sub,wspd_sub,by=c("datetime","City"))

# CLEAN LAT/LON COLUMNS
# Convert timezone based on longitude (a rough approximation, appropriate for cities in this app)
convert_time <- function(datetime,longitude) {
  datetime_conv <- as.character(datetime)
  datetime_conv <- as.POSIXct(datetime_conv,format="%Y-%m-%d %H:%M:%S")
  if (longitude <= -115) {
    datetime_conv <- datetime_conv-hours(8)
  } else if ((longitude > -115) & (longitude <= -100)) {
    datetime_conv <- datetime_conv-hours(7)
  } else if ((longitude > -100) & (longitude <= -87)) {
    datetime_conv <- datetime_conv-hours(6)
  } else {
    datetime_conv <- datetime_conv-hours(5)
  }
  return(datetime_conv)
}

# PROCESS O3 AND NO2 DATA 
# Read in O3 and NO2, which were downloaded using python API
o3 <- read.csv(file='o3_sub.csv')
no2 <- read.csv(file='no2_sub.csv')
# Concatenate O3 and NO2 into one dataframe, add columns for year/month/day/hour
o3_no2 <- inner_join(o3,no2,by=c("Date","Time","City"))
o3_no2 <- o3_no2 %>% mutate(Datetime = paste(Date,Time))
o3_no2 <- o3_no2 %>% mutate(Year=lubridate::year(o3_no2$Datetime),
                            Month=lubridate::month(o3_no2$Datetime,label=TRUE,abbr=FALSE),
                            Day=lubridate::day(o3_no2$Datetime),
                            HrofDay=lubridate::hour(o3_no2$Datetime)) %>% select(-Date,-Time,-Datetime)
# Add columns for year/month/day/hour to temp_sub as well
temp_sub$datetime <- convert_time(temp_sub$datetime,temp_sub$Longitude)
temp_sub <- temp_sub %>% mutate(Year=lubridate::year(temp_sub$datetime),
                                       Month=lubridate::month(temp_sub$datetime,label=TRUE,abbr=FALSE),
                                       Day=lubridate::day(temp_sub$datetime),
                                       HrofDay=lubridate::hour(temp_sub$datetime)) 
temp_sub$City <- gsub("\\."," ",temp_sub$City)
# Join temp_sub with O3/NO2 dataframe on time and city name
temp_sub_pol <- inner_join(temp_sub,o3_no2,by=c("Year","Month","Day","HrofDay","City"))
temp_sub_pol <- select(temp_sub_pol,-datetime,-Country,-Latitude,-Longitude)
temp_sub_pol <- temp_sub_pol %>% filter(Year > 2012)
# Save final table as csv for use in shiny app
write.csv(temp_sub_pol,"temp_sub.csv")



