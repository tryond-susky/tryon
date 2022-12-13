library(stringr)
library(readr)

library(MASS)
library(MuMIn)
library(mgcv)
install.packages("stargazer")
library(stargazer)


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
        #Understanding North Pennisula Salmon Run 2022
    
        # Create a scatter plot between two of your numeric columns.
        # Change the point shape and color to something NOT used in the example.
        # Change the x and y labels and add a title
        # Export the plot as a JPEG by using the "Export" button in the plotting pane.
        
   
        # Create a scatter plot between date and daily fish counts      
        plot(data$date, data$fish.count, xlab = "Time", ylab = "Count", main = "Pacific Salmon 2022 Run", col = "black")
     
      #Create a scatter plot between count per species throughout the season
        #Make a subset from the data set to look at specific species
        chinook <-subset(data, data$species == "Chinook")        
        chum  <-subset(data, data$species == "Chum")  
        coho  <-subset(data, data$species == "Coho") 
        sockeye  <-subset(data, data$species == "Sockeye")
        pink  <-subset(data, data$species == "Pink") 
        #Create Scatter plot adding lines on for each species 
          plot(chinook$date, chinook$fish.count, type = "l" ,xlab = "Date", ylab = "Count", ylim = c(0,3000), main = "Pacific Salmon 2022 Run" , col = "lightsalmon")     
          lines(chum$date, chum$fish.count, type = "l", col = "cyan4")
          lines(coho$date, coho$fish.count, type = "l", col = "ivory4")
          lines(sockeye$date, sockeye$fish.count/10, type = "l", col = "red")
          lines(pink$date, pink$fish.count, type = "l", col = "pink")
          
      #Create a scatter plot between count per location throughout the season
          #Make a subset from the data set to look at specific species
          Nelson <-subset(data, data$location == "Nelson River (Sapsuk)")
          Sandy <-subset(data, data$location == "Sandy River")
          Ilnik <-subset(data, data$location == "Ilnik River")
          Bear <-subset(data, data$location == "Bear River")
            #Create Scatter plot adding lines on for each species 
            plot(Nelson$date, Nelson$fish.count,type = "l", xlab = "Date", ylab = "Count", ylim = c(0,20000), main = "Pacific Salmon 2022 Run" , col = "orange")     
            lines(Sandy$date, Sandy$fish.count, type = "l", col = "purple")
            lines(Ilnik$date, Ilnik$fish.count, type = "l", col = "blue")
            lines(Bear$date, Bear$fish.count, type = "l", col = "red")
            legend(1, 95, legend=c("Nelson", "Line 2"),
                   col=c("orange", "blue"), lty=1:2, cex=0.8)

          #If i want to add data points
points(weatherdata$date, 10*weatherdata$`Wind Speed`)        
 

    # Plot Sky condition and Count Distribution using a box plot
plot(data$fish.count~factor(data$condition),xlab = "Condition", ylab = "Count", main = "Condition and Count Distribution")
legend("topleft", legend=c("Line 1", "Line 2"),
       col=c("red", "blue"), lty=1:2, cex=0.8)
  
        
        #Table
        sum(table(data$fish.count))
        stargazer package
        
##GAMM for all weather parameters 
        gamm.mod1 <- gam(data$fish.count ~ data$date + data$condition + data$wind + data$temperature + data$dew.point + data$humidity + data$wind.speed + data$wind.gust + data$pressure + data$species + data$location, family = gaussian, random = ~ 1 | location, data = data)
        AIC(gamm.mod1)
        summary(gamm.mod1)
