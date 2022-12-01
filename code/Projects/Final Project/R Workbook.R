#setting working directory
setwd("C:/GitHub/tryon/code/Projects/Final Project")

install.packages("rwunderground")
library(rwunderground)

library(dplyr)          #for data manipulation
library(ggplot2)        #for visualisations
library(lubridate)      #to work with dates

set_api_key("put_your_own_api_key_here")     #API key in weather underground page
set_api_key("08e660296d2a4a86a660296d2aea8655")

my_weather_station<-"IAGLANDJ2"              #The uni of cyprus weather station id
my_weather_station<-"PASD" 

weather_data<-history(set_location(PWS_id = my_weather_station), date = "20170801")



#try two

start_date <- as.POSIXct("2019-09-01",tz="gmt")
end_date <-  as.POSIXct(Sys.Date(),tz="gmt")

dates <- seq.POSIXt(start_date,end_date,by="day")


my_dates<-as.integer(unlist(strsplit(as.character(dates),"-")))

my_dates<-array(my_dates,dim=c(3,length(my_dates)/3))

my_dates<-as.data.frame(t(my_dates), )

colnames(my_dates)<-c("year","month","day")

station <- "IISLEOFW4"

myurl <- NULL

for(i in 1:length(my_dates)) {
  
  #myurl[i] <- paste("https://www.wunderground.com/weatherstation/WXDailyHistory.asp?ID=",station,"&day=",my_dates$day[i],"&month=",my_dates$month[i],"&year=",my_dates$year[i],"&graphspan=day&format=1", sep = "")
  myurl[i] <- paste("https://www.wunderground.com/PASD/WXDailyHistory.asp?ID=",station,"&day=",my_dates$day[i],"&month=",my_dates$month[i],"&year=",my_dates$year[i],"&graphspan=day&format=1", sep = "")
   folder <- "D:\\WeatherData"
  myfile <- paste("NewportWeather",rownames(my_dates),".csv")
  download.file(url = myurl[i], file.path(folder, myfile, fsep = "\\" ))
}

