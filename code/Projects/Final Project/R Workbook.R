library(stringr)
library(readr)

library(MASS)
library(MuMIn)
library(mgcv)

#setting working directory and inputting data

  setwd("C:/Github/tryon/code/Projects/Final Project")

#Fish Data
  Bear <- read_csv("BearFishCounts.csv", col_types = cols(count_date = col_date(format = "%m/%d/%Y")))  
  Nelson <- read_csv("NelsonFishCounts.csv", col_types = cols(count_date = col_date(format = "%m/%d/%Y")))
  Ilnik <- read_csv("IlnikFishCounts.csv", col_types = cols(count_date = col_date(format = "%m/%d/%Y")))
  Sandy <- read_csv("SandyFishCounts.csv", col_types = cols(count_date = col_date(format = "%m/%d/%Y")))  
  
  #Merge Weather Data
  fishdata <- rbind(Nelson,Ilnik,Bear,Sandy)
      
      #rename columns
      names(fishdata)[2] <- 'date'
      
      plot(fishdata$fish_count ~ fishdata$date)


#Weather Data

  weather  <- read_csv("weatherdata.csv", col_types = cols(Time = col_date(format = "%m/%d/%Y")))
  head(weather)
  colnames(weather)

    #aggregate numeric data by date

       numericweather <- aggregate(weather[,c(2,3,4,5,6,7,8)], by= list(weather$Time), FUN = "mean")
       
       #rename columns
       names(numericweather)[1] <- 'date'

    #create table for wind then sort for the predominant direction

       max(table(weather$Wind))
       Windfunction <- function(x) names(sort(table(x),decreasing = TRUE)[1])
       Wind <- aggregate(weather$Wind, by= list(weather$Time), FUN = "Windfunction")
       
       #rename columns
       names(Wind) <- c('date', 'Wind')

    #create table for condition then sort for the predominant direction

        max(table(weather$Condition))
        conditionfunction <- function(x) names(sort(table(x),decreasing = TRUE)[1])
        condition <- aggregate(weather$Condition, by= list(weather$Time), FUN = "conditionfunction")
        
        #rename columns
        names(condition) <- c('date', 'condition')
    
    #Merge Weather Data numeric and character
        characterweather <- merge(condition,Wind, by = "date")
        weatherdata <- merge(characterweather,numericweather, by = "date")

#Merge Weather and Fish!!
        data <- as.data.frame(merge(weatherdata, fishdata, by = "date"))
        colnames(data) 
        #rename columns
        names(data) <- c('date', 'condition',"wind","temperature","dew.point","humidity","wind get rid of","wind.speed","wind.gust","pressure", "year","fish.count","species", "speices.id","location","location.id")
        #get rid of extra wind column
        data <- data[,-7]

#Running the Stats
        #Salmon Run 2022
        ####Scatterplot
        # Create a scatter plot between two of your numeric columns.
        # Change the point shape and color to something NOT used in the example.
        # Change the x and y labels and add a title
        # Export the plot as a JPEG by using the "Export" button in the plotting pane.
        
   
              
        plot(data$date, data$fish.count, xlab = "Time", ylab = "Count", main = "Pacific Salmon 2022 Run", pch = 8 , col = "dodgerblue2")
     
chinook <-subset(data, data$species == "Chinook")        
chum  <-subset(data, data$species == "Chum")         
        
        
        
        
        
plot(chinook$date, chinook$fish.count, type = "l" ,xlab = "Time", ylab = "Count", main = "Pacific Salmon 2022 Run", pch = 8 , col = "dodgerblue2")     
lines(chum$date, chum$fish.count, type = "l", col = "red")        
points(weatherdata$date, 10*weatherdata$`Wind Speed`)        
 


plot(data$fish.count~factor(data$condition))

  
        barplot?
          ?complete.cases
        ?plot.ecdf
        ?pch
        demo('colors')
        
        #Table
        sum(table(data$fish.count))
        stargazer package
        
##GAMM for all 
        gamm.mod1 <- gam(data$fish.count ~ data$date + data$condition + data$wind + data$temperature + data$dew.point + data$humidity + data$wind.speed + data$wind.gust + data$pressure + data$species + data$location, family = gaussian, random = ~ 1 | location, data = data)
        plot(gamm.mod1)
        AIC(gamm.mod1)
summary(gamm.mod1)
