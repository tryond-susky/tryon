# (1) Approximately how many hours ahead of Sunbury was the peak flow in Lewisburg during the 2011 flood? (2 pt)
  
# About 9 hours, but around 7-11 hours

# (2) Give one reason why information on the time between peak flow events up- and downstream could be valuable? (4 pts)

#By understanding the peak flow times we can analysis what amount of water to expect in flooding events and what 
#mitigation efforts are needed from the expected flow.

# Package scavenger hunt! (12 pts each)

## (3) Using Google and ONLY packages from GitHub or CRAN:
    # Find a package that contains at least one function specifically designed to measure genetic drift.
    # Copy-paste into your script - and run - an example from the reference manual for a function within this package related to a measure of genetic drift. 
        # Depending on the function, either upload a plot of the result or use print() and copy/paste the console output into your script.
    # After running the function example, manipulate a parameter within the function to create a new result. 
        # Common options might be allele frequency, population size, fitness level, etc. 
        # Add the results of this manipulation to your script (if in the console) or upload the new plot.
       
          # By manipulating these parameters you can see how it impacts the results.
          # This type of manipulation is one example of how theoretical ecology and modelling are used to predict patterns in nature.

install.packages("learnPopGen")
library(learnPopGen)

?drift.selection

drift.selection()
p<-drift.selection(p0=0.01,Ne=100,w=c(1,0.9,0.8),ngen=200,nrep=5)
plot(p)

drift.selection()
d<-drift.selection(p0=0.01,Ne=100,w=c(5,0.7,0.6),ngen=200,nrep=100, color=blues9)
plot(d)



## (4) Using Google and ONLY packages from GitHub or CRAN:
    # Find a package that will generate standard diversity metrics for community ecology, specifically Simpson's Diversity Index.
    # Copy-paste into your script - and run - an example from the reference manual for a function to calculate Simpson's diversity. 
        # Depending on the example usage of the function, either upload a plot of the result or use print() and copy/paste the console output into your script.
    # After running the function example, modify your script to generate another diversity metric that is NOT part of the example. 
        # If there are two diversity metrics in the example script, neither of these will count as the modified script.
        # Hint: If the function can "only" caluclate Simpson's diversity, the inverse of Simpson's diversity is another common metric. 
        # Add the results of this manipulation to your script (if in the console) or upload the new plot.
        
          # Diversity metrics are frequently used in community ecology for reasons ranging from a quick comparison between sites to understanding community stability.
          # Their calculation can be very tedious by hand - and very fast with a package designed for the operation.


install.packages("simboot")
library(simboot)
rep.num <- c(1,2,3,1,2,3,1,2,3,5,2,3,4,0,5)
uniqu.num <- c(0,1,1,2,3,5,8,13,21,34,55,89,144,233,377)



sbdiv(rep.num, uniqu.num, theta = c("Shannon", "Simpson"),
      type = c("Dunnett", "Tukey", "Sequen", "AVE",
               "Changepoint", "Williams", "Marcus",
               "McDermott", "UmbrellaWilliams", "GrandMean"),
      cmat = NULL, method = c("WYht", "tsht", "rpht", "asht"), conf.level =
        0.95, alternative = c("two.sided", "less", "greater"), R = 2000, base =
        1)
?sbdiv
## For plots of the datasets see the help files for the data sets.

## First dataset
data(predatGM)

## structure of data
str(predatGM)

## remove block variable
datspec_1 <- predatGM[, -1]
str(datspec_1)

## Order of factorial variable
datspec_1$Variety

## argument base = 1 uses GM as control group. Not directly executable
## due to intensive computing time
# sbdiv(X = datspec_1[, 2:length(datspec_1)], f = datspec_1[, 1], theta =
# "Shannon", type = "Dunnett", method = "WYht", conf.level = 0.95,
# alternative = "two.sided", R = 2000, base = 1)

## Directly executable but senseless value for boot steps R
sbdiv(X = datspec_1[, 2:length(datspec_1)], f = datspec_1[, 1], theta =
        "Shannon", type = "Dunnett", method = "WYht", conf.level = 0.95,
      alternative = "two.sided", R = 100, base = 1)


## Second dataset
data(saproDipGM)

## structure
str(saproDipGM)

## remove block variable
datspec_2 <- saproDipGM[, -1]
str(datspec_2)

## Order of factor variable
datspec_2$Variety

## argument base = 2 uses Ins as control group. Not directly executable
## due to intensive computing time
# sbdiv(X = datspec_2[, 2:length(datspec_2)], f = datspec_2[, 1], theta =
# "Shannon", type = "Dunnett", method = "rpht", conf.level = 0.95,
# alternative = "two.sided", R = 2000, base = 2)

## Directly executable but senseless value for boot steps R
sbdiv(X = datspec_2[, 2:length(datspec_2)], f = datspec_2[, 1], theta =
        "Shannon", type = "Dunnett", method = "rpht", conf.level = 0.95,
      alternative = "two.sided", R = 100, base = 2)
