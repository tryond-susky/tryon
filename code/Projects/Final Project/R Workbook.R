#setting working directory and inputting data

setwd("C:/Github/tryon/code/Projects/Final Project")

#Fish Data
Nelson <- read.csv("NelsonFishCounts.csv", header=TRUE)
Ilnik <- read.csv("IlnikFishCounts.csv", header=TRUE)
Bear <- read.csv("BearFishCounts.csv", header=TRUE)
Sandy <- read.csv("SandyFishCounts.csv", header=TRUE)

#Weather Data

Juneweather <- read.csv("Juneweatherdata.csv", header = TRUE)
head(Juneweather)

colnames(Juneweather)

aggregate(Juneweather[,2:3], by= list(Juneweather$Time), FUN = "mean")
table(Juneweather)[,max(table(Juneweather$Wind))]

