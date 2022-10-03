# First, recreate Figure 4 from Herron et al. (2019). De novo origins of multicellularity in response to predation. Scientific Reports.
  # Search datadryad.org by the paper title and download the dataset. It will include .csv files and R scripts, organized by figure.
  # Save the script and change the working directory on lines 8 and 115 to match your GitHub repository. (6 points)
  # Export and save the plot you've created. (2 points)
  # Zoom into your plot to look at the distribution for different strains.

# Do all of the strains in the plot have the same distributions (yes/no)? (2 pt)
# The strains in the plots do no have the same distributions. We can tell this since the distributions are not normal across the distributions.

# Based on these observations of your strain distributions, why did the authors use a Kruskal-Wallis test rather than ANOVA to compare the strains? (2 pts)
#The authors used a Kruskal-Wallis test since ANOVAs are used to test the equality of means of values whereas the Kruskal-Wallis looks at the comparsion of mean ranks. This test works best since the data is not normal and thus does not have normal means. 

# Use the fitdist() and gofstat() functions to compare the poisson, negative binomial, and logistic distributions for:
  # (1) - The number of cells of progeny (data$Num.Cells.Progeny)
  # (2) - The replication time (data$RepTime.sec)
      # 3 points each
    #HINT- "Num.Cells.Progeny" has defined breaks. To display results, use the formula with the "chisqbreaks" argument as follows:
      #gofstat(list(fit.1, fit.2, fit.3, etc), chisqbreaks=c(1,2,4,8,16,32,64))
#Installation
install.packages("fitdistrplus")
install.packages("logspline")
library(fitdistrplus)
library(logspline)
?fitdist

#Get the happy data
setwd("C:/GitHub/tryon/code/Projects/Week5")
data <- read.csv("Figure4Data.csv", header=TRUE)

#Plot the happy data
Cell <- lm(data$Num.Cells.Progeny ~ data$RepTime.sec)

plot(data$Num.Cells.Progeny ~ data$RepTime.sec, xlab = "Repetition Time", ylab = "Number Cells Progeny", main = "Cell Replication")
abline(Cell, col = "grey", lwd= 3)

# (1) - The number of cells of progeny (data$Num.Cells.Progeny)

fit.logis1 <- fitdist(c (na.exclude (data$Num.Cells.Progeny)), distr = "logis")
fit.pois1 <- fitdist(c (na.exclude (data$Num.Cells.Progeny)), distr = "pois")
fit.nbinom1 <- fitdist(c (na.exclude (data$Num.Cells.Progeny)), distr = "nbinom")

gofstat(list(fit.logis1, fit.pois1, fit.nbinom1), chisqbreaks=c(1,2,4,8,16,32,64))

# (2) - The replication time (data$RepTime.sec)
fit.logis2 <- fitdist(c (na.exclude (data$RepTime.sec)), distr = "logis")
fit.pois2 <- fitdist(c (na.exclude (data$RepTime.sec)), distr = "pois")
fit.nbinom2 <- fitdist(c (na.exclude (data$RepTime.sec)), distr = "nbinom")

gofstat(list(fit.logis2, fit.pois2, fit.nbinom2))

# Based on the AIC scores, which distribution is the best fit for: (4 pts)
  # (1) - The number of cells of progeny (data$Num.Cells.Progeny)?

#The negative bionomial distribution is the best fit for data$Num.Cells.Progeny, since the AIC number is the lowest for this distribution.

  # (2) - The replication time (data$RepTime.sec)?

#The negative bionomial distribution is the best fit for data$RepTime.sec, since the AIC number is the lowest for this distribution.



# Plot a generic histogram for the replication time (data$RepTime.sec) (2 pt)
hist(data$RepTime.sec, xlab = "Repetition Time", main = "Cell Replication")

# Based on the patterns of this histograms and Figure 4:
  #Give one hypothesis for an evolutionary process represented by the two tallest bars in your histogram. (6 pts)
  # Don't cheat by looking at the paper! 
    # This hypothesis does not need to be correct - it only needs to be ecologically rational based these two figures.

#The strains that are the most successful have cell replication times of 50,000 and 200,000.




#I gotta say Grrrrrr isn't grrrr without r, this assignment finally made sense!

