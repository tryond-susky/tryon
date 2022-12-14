##Packages
library(stringr)
library(readr)

library(MASS)
library(MuMIn)
library(mgcv)

#setting working directory and inputting data

  setwd("C:/Github/tryon/code/Projects/Final Project")

#Fish Data
  #read in with dates changed to numeric from character
  Bear <- read_csv("BearFishCounts.csv", col_types = cols(count_date = col_date(format = "%m/%d/%Y")))  
  Nelson <- read_csv("NelsonFishCounts.csv", col_types = cols(count_date = col_date(format = "%m/%d/%Y")))
  Ilnik <- read_csv("IlnikFishCounts.csv", col_types = cols(count_date = col_date(format = "%m/%d/%Y")))
  Sandy <- read_csv("SandyFishCounts.csv", col_types = cols(count_date = col_date(format = "%m/%d/%Y")))  
  
  #Combine Fish Data from all locations
  fishdata <- rbind(Nelson,Ilnik,Bear,Sandy)
      
      #rename columns to make merging easier
      names(fishdata)[2] <- 'date'
      
      #test plotting the master fishdata to make sure it works nicely
      plot(fishdata$fish_count ~ fishdata$date)


#Weather Data

  weather  <- read_csv("weatherdata.csv", col_types = cols(Time = col_date(format = "%m/%d/%Y")))
  head(weather)
  colnames(weather)

    #aggregate numeric weather data by date so each date is respresented by average hourly data

       numericweather <- aggregate(weather[,c(2,3,4,5,6,7,8)], by= list(weather$Time), FUN = "mean")
       
       #rename columns to make merging easier
       names(numericweather)[1] <- 'date'

    #create table for wind direction then sort for the predominant daily direction

       max(table(weather$Wind))
       Windfunction <- function(x) names(sort(table(x),decreasing = TRUE)[1])
       Wind <- aggregate(weather$Wind, by= list(weather$Time), FUN = "Windfunction")
       
       #rename columns this simply adds understandable column headings
       names(Wind) <- c('date', 'Wind')

    #create table for condition then sort for the predominant daily condition

        max(table(weather$Condition))
        conditionfunction <- function(x) names(sort(table(x),decreasing = TRUE)[1])
        condition <- aggregate(weather$Condition, by= list(weather$Time), FUN = "conditionfunction")
        
        #rename columns this simply adds understandable column headings
        names(condition) <- c('date', 'condition')
    
    #Merge Weather Data numeric and character: this pulls all the weather data together into weatherdata
        characterweather <- merge(condition,Wind, by = "date")
        weatherdata <- merge(characterweather,numericweather, by = "date")

#Merge Weather and Fish!!: this puts all the gathered data into one large data frame
        data <- as.data.frame(merge(weatherdata, fishdata, by = "date"))
        colnames(data) 
        #rename columns to make them all match nicely
        names(data) <- c('date', 'condition',"wind","temperature","dew.point","humidity","wind get rid of","wind.speed","wind.gust","pressure", "year","fish.count","species", "speices.id","location","location.id")
        #get rid of extra wind column created with NAs
        data <- data[,-7]
        

#Running the Stats
        #Understanding North Peninsula Salmon Run 2022
    
        # Create a scatter plot between date and daily fish counts by species      
        plot(data$date, data$fish.count,ylim = c(0,20000), xlab = "Time", ylab = "Daily Count", main = "Pacific Salmon 2022 Run", col = "black")
     
      #Create a scatter plot between count per species throughout the season
        #Make a subset from the data set to look at specific species
        chinook <-subset(data, data$species == "Chinook") 
        chum  <-subset(data, data$species == "Chum")  
        coho  <-subset(data, data$species == "Coho") 
        sockeye  <-subset(data, data$species == "Sockeye")
        pink  <-subset(data, data$species == "Pink") 
        #Create Scatter plot adding lines on for each species *this helps us look at when the runs happen at each location
          plot(sockeye$date, sockeye$fish.count/10, type = "h" ,xlab = "Date", ylab = "Count", ylim = c(0,2000), main = "Pacific Salmon 2022 Run" , col = "red", lwd=5.0)     
          lines(chum$date, chum$fish.count, type = "h", col = "cyan4", lwd=5.0)
          lines(chinook$date, chinook$fish.count, type = "h", col = "lightsalmon", lwd=5.0)
          lines(pink$date, pink$fish.count, type = "h", col = "pink", lwd=5.0)
          lines(coho$date, coho$fish.count, type = "h", col = "ivory4", lwd=5.0)
          legend("topright", legend=c("Chinook", "Chum", "Coho", "Sockeye / 10", "Pink"),col=c("lightsalmon", "cyan4", "ivory4", "red","pink"), lty=1:2, cex=0.8, lwd=5.0)
      
        
##GAMM for all weather parameters to identifiy the most predominant weather factor
        gamm.mod1 <- gam(data$fish.count ~ data$condition + data$wind + data$temperature + data$dew.point + data$humidity + data$wind.speed + data$wind.gust + data$pressure, family = gaussian, random = ~ 1 | location, data = data)
        AIC(gamm.mod1)
        summary(gamm.mod1)
            
        ##GAMM minus wind gust
        gamm.mod1 <- gam(data$fish.count ~ data$condition + data$wind + data$temperature + data$dew.point + data$humidity + data$wind.speed  + data$pressure, family = gaussian, random = ~ 1 | location, data = data)
        AIC(gamm.mod1)
        summary(gamm.mod1)
        
        ##GAMM wind speed
        gamm.mod1 <- gam(data$fish.count ~ data$condition + data$wind + data$temperature + data$dew.point + data$humidity + data$pressure, family = gaussian, random = ~ 1 | location, data = data)
        AIC(gamm.mod1)
        summary(gamm.mod1)
        
        ##GAMM minus temperature
        gamm.mod1 <- gam(data$fish.count ~ data$condition + data$wind + data$dew.point + data$humidity + data$pressure, family = gaussian, random = ~ 1 | location, data = data)
        AIC(gamm.mod1)
        summary(gamm.mod1)
        
        ##GAMM minus pressure
        gamm.mod1 <- gam(data$fish.count ~ data$condition + data$wind + data$dew.point + data$humidity, family = gaussian, random = ~ 1 | location, data = data)
        AIC(gamm.mod1)
        summary(gamm.mod1)
        
        ##GAMM minus humidity
        gamm.mod1 <- gam(data$fish.count ~ data$condition + data$wind + data$dew.point, family = gaussian, random = ~ 1 | location, data = data)
        AIC(gamm.mod1)
        summary(gamm.mod1)
        
        ##GAMM minus dew point
        gamm.mod1 <- gam(data$fish.count ~ data$condition + data$wind, family = gaussian, random = ~ 1 | location, data = data)
        AIC(gamm.mod1)
        summary(gamm.mod1)
        
        ##GAMM of condition
        gamm.mod1 <- gam(data$fish.count ~ data$condition, family = gaussian, random = ~ 1 | location, data = data)
        AIC(gamm.mod1)
        summary(gamm.mod1)
        
        ##GAMM of wind direction
        gamm.mod1 <- gam(data$fish.count ~ data$wind, family = gaussian, random = ~ 1 | location, data = data)
        AIC(gamm.mod1)
        summary(gamm.mod1)
        
  # Plot Sky condition and Count Distribution using a box plot
   plot(data$fish.count~factor(data$condition),xlab = "Condition", ylab = "Count", main = "Condition and Count Distribution")
        
   # Plot Wind Direction and Count Distribution using a box plot
   plot(data$fish.count~factor(data$wind),xlab = "Wind Direction", ylab = "Count", main = "Wind Direction and Count Distribution")
   