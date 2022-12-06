#setting working directory and inputting data

setwd("C:/Users/tryon/Documents/GitHub/tryon/code/Projects/Final Project")

#Fish Data
Nelson <- read.csv("NelsonFishCounts.csv", header=TRUE)
Ilnik <- read.csv("IlnikFishCounts.csv", header=TRUE)
Bear <- read.csv("BearFishCounts.csv", header=TRUE)
Sandy <- read.csv("SandyFishCounts.csv", header=TRUE)

#Weather Data

Juneweather <- read.csv("Juneweatherdata.csv", header = TRUE)
head(Juneweather)
